library(countrycode)
library(openxlsx)
library(foreign)

rm(list=ls())

options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/COVID and institutions/GitHub/PPI-data")

indir <- normalizePath("ingredients")
dfp_github <- normalizePath("data")
start.date <- as.Date("2020-1-1")

regm1 <- file.path(dfp_github, "PPI_regions_m1")
regm2 <- file.path(dfp_github, "PPI_regions_m2")

# PPI data
weights <- readRDS(file.path(indir, "ppi_weights.rds"))
totals <- aggregate(score ~ group, data=subset(weights, template=="o"), FUN=max)
totals.vec <- setNames(totals$score, totals$group)

geo.grid <- read.csv(file.path(indir, "population_template.csv"), encoding = "UTF-8")
geo.grid.x <- unique(subset(geo.grid, include=="Y", select=c("ccode","rcode","iso_state","name")))
 
meta <- readRDS(file.path(indir, "PPI_meta.rds"))
meta <- subset(meta, group!="all")
meta <- merge(meta, totals, by="group")
meta <- within(meta, ups <- round(score.x/score.y,5))

ppi.regions <- readRDS(file.path(indir, "ppi_regions.rds"))
ppi.regions$counter <- as.numeric(ppi.regions$date-start.date)

# stack national and subnational policies
ppi.regions.w <- subset(ppi.regions, template=="o" & group!="all", 
                        select=c("ccode","rcode","date","counter","group","score.s","score.n"))
ppi.regions.l <- tidyr::pivot_longer(ppi.regions.w, cols=all_of(c("score.s","score.n")))
ppi.lagged <- within(ppi.regions.l, counter <- counter+1)
ppi.regions.l <- dplyr::inner_join(ppi.regions.l, ppi.lagged,
                                  by = c("ccode","rcode","counter","group","name"),
                                  suffix=c("",".l1"))
ppi.regions.l <- within(ppi.regions.l, {
  total_change <- round((value-value.l1),3)
  subnational <- as.numeric(name=="score.s")
})
ppi.regions.l <- subset(ppi.regions.l, total_change !=0, 
                        select=c("ccode","subnational","rcode","group","date","total_change","value.l1"))

# meta information
core.meta.regions <- unique(meta[c("ccode","rcode","subnational","group","date","branch","source_link")])
core.meta.regions <- core.meta.regions[with(core.meta.regions, order(ccode, rcode, subnational, group, date, branch)),]
core.meta.regions$nr <- 1L:nrow(core.meta.regions)
meta.regions <- dplyr::inner_join(core.meta.regions,meta)
meta.regions <- meta.regions[with(meta.regions, order(nr, rid)),]
meta.regions <- unique(meta.regions[c("nr","ccode","rcode","subnational","group","date","branch","who","institution","source_link","report_date","expiration_date","ups")])

# policies applied to the whole country
countrywide <- subset(meta.regions, rcode == "ZZ")
countrywide$rcode <- NULL
# associate each policy with a region
regionspec <- rbind(subset(meta.regions, rcode != "ZZ"),
                    dplyr::inner_join(countrywide, unique(ppi.regions[c("ccode","rcode")])))

## combine into a meta file
meta.regions <- dplyr::left_join(ppi.regions.l, regionspec, by=c("ccode","rcode","subnational","group","date"))

iso_state <- setNames(geo.grid.x$iso_state, geo.grid.x$rcode)
state_province <- setNames(geo.grid.x$name, geo.grid.x$rcode)
branch.codes <- c(unk="missing", eld="executive leadership",jud="judiciary", bur="bureaucracy", leg="legislative", oth="other")

meta.regions <- within(meta.regions, {
  report_change <- round(ups-value.l1,3) 
  isocode <- ccode
  isoabbr <- countrycode(isocode, origin="iso3n", destination="iso2c")
  dimension <- group
  branch <- branch.codes[branch]
  state_province <- state_province[rcode]
  iso_state <- iso_state[rcode]
})
meta.colnames <- c("isocode","isoabbr","state_province","iso_state","date","dimension","subnational","total_change",
                   "branch","who","institution","report_date","expiration_date","report_change","source_link")
meta.regions <- meta.regions[with(meta.regions, order(isoabbr, iso_state, subnational, dimension, date)),]
write.table(meta.regions[meta.colnames], file.path(dfp_github,"changes_regions_m1.csv"), na="", append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)


####  prepare clean versions
ppi.country <- readRDS(file.path(indir, "ppi_country.rds"))
ppi.country <- within(subset(ppi.country, template %in% c("o","n")), {
  field <- paste(category, group, sep=".")
  reg <- round(score.s, 3)
  nat <- round(score.n, 3)
  tot <- round(val, 3)
})

ppi.regions <- within(subset(ppi.regions, template %in% c("o","n")), {
  field <- paste(category, group, sep=".")
  reg <- round(score.s, 3)
  nat <- round(score.n, 3)
  tot <- round(val, 3)
})

labels  <- unique(ppi.country[c("template","category","group","field")])

sort.cat <- data.frame(category = sort(unique(labels$category)), sort.cat=seq_along(unique(labels$category)))
sort.cat$sort.cat[sort.cat$category=="ppi"] <- 0
sort.gr <- data.frame(group = sort(unique(labels$group)), sort.gr=seq_along(unique(labels$group)))
sort.gr$sort.gr[sort.gr$group=="all"] <- 0
labels <- merge(labels,sort.cat)
labels <- merge(labels,sort.gr)
labels <- merge(labels, data.frame(cov=c("tot","nat","reg"), sort.cov = c(0.1,0.2,0.3)))
labels <- within(labels, {
  label <- paste0(field,".",cov)
  gr.field <- field
  field <- paste0(cov,".",field)
  sort <- sort.cat*100+sort.gr + sort.cov
})
labels.n <- subset(labels[order(labels$sort),], template=="n" & !gr.field %in% c("emerg.emerg","borders.border","masks.masks"))
labels.o <- subset(labels[order(labels$sort),], template=="o" & !gr.field %in% c("emerg.emerg","borders.border","masks.masks"))

## v1
y <- subset(ppi.country, template=="o", select=c("ccode", "date","field","reg","nat","tot")) 
y.w <- tidyr::pivot_wider(y, names_from=field, values_from=c(reg, nat, tot), names_sep=".")
y.w <- within(y.w, {
  isocode <- ccode
  cname <- countrycode(isocode, origin="iso3n", destination="cow.name")
  isoabbr <- countrycode(isocode, origin="iso3n", destination="iso2c")
})
y.w <- y.w[order(y.w$date), c("cname","isocode","isoabbr","date", labels.o$field)]
colnames(y.w) <- c("cname","isocode","isoabbr","date", paste0(labels.o$label,".ave"))  
write.table(y.w, file.path(dfp_github,"PPI_country_m1.csv"), append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)
attr(y.w, "datalabel") <- paste0("Protective Policy Index M1, Country-Day file, ", format(Sys.Date(), "%Y-%m-%d"))
varl.corr <- c("cname"="Country name", "isocode"="ISO 3166 country code",
            "isoabbr"="ISO 3166 (2c) country code",
            "ppi.all.tot.ave"="Average total PPI",
            "ppi.all.nat.ave"="Average national PPI",               
            "ppi.all.reg.ave"="Average regional PPI")
varl.st <- rep("",ncol(y.w))
varl.st[match(names(varl.corr), colnames(y.w))] <- varl.corr

attr(y.w, "var.labels") <- varl.st
write.dta(y.w, file.path(dfp_github,"PPI_country_m1.dta"))

## v2
y <- subset(ppi.country, template=="n", select=c("ccode", "date","field","reg","nat","tot")) 
y.w <- tidyr::pivot_wider(y, names_from=field, values_from=c(reg, nat, tot), names_sep=".")
y.w <- within(y.w, {
  isocode <- ccode
  cname <- countrycode(isocode, origin="iso3n", destination="cow.name")
  isoabbr <- countrycode(isocode, origin="iso3n", destination="iso2c")
})
y.w <- y.w[order(y.w$date), c("cname","isocode","isoabbr","date", labels.n$field)]
colnames(y.w) <- c("cname","isocode","isoabbr","date", paste0(labels.n$label,".ave.2")) 
write.table(y.w, file.path(dfp_github,"PPI_country_m2.csv"), append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)

attr(y.w, "datalabel") <- paste0("Protective Policy Index M2, Country-Day file, ", format(Sys.Date(), "%Y-%m-%d"))
varl.corr <- c("cname"="Country name", "isocode"="ISO 3166 country code",
               "isoabbr"="ISO 3166 (2c) country code",
               "ppi.all.tot.ave.2"="Average total PPI",
               "ppi.all.nat.ave.2"="Average national PPI",               
               "ppi.all.reg.ave.2"="Average regional PPI")
varl.st <- rep("",ncol(y.w))
varl.st[match(names(varl.corr), colnames(y.w))] <- varl.corr

attr(y.w, "var.labels") <- varl.st
write.dta(y.w, file.path(dfp_github,"PPI_country_m2.dta"))

## regions v1
y <- subset(ppi.regions, template=="o", select=c("ccode", "rcode", "date","field","reg","nat","tot")) 
y.w <- tidyr::pivot_wider(y, names_from=field, values_from=c(reg, nat, tot), names_sep=".")
y.w <- within(y.w, {
  isocode <- ccode
  isoabbr <- countrycode(isocode, origin="iso3n", destination="iso2c")
  state_province <- state_province[rcode]
  iso_state <- iso_state[rcode]
})

y.w <- y.w[c("isocode","isoabbr","state_province","iso_state","date", labels.o$field)]
colnames(y.w) <- c("isocode","isoabbr","state_province","iso_state","date", labels.o$label)

y.w.li <- split(y.w, y.w$isoabbr)
for (cn in names(y.w.li)) {
  temp <- y.w.li[[cn]][order(y.w.li[[cn]][["date"]], y.w.li[[cn]][["iso_state"]]),]
  write.table(temp, paste0(regm1,"/PPI_regions_",cn,"_m1.csv"), append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)
}

# export as an archive with a Stata file
fn <- file.path(dfp_github,"PPI_regions_m1_dta.zip")
temp_loc <- file.path(tempdir(),"PPI_regions_m1.dta")
attr(y.w, "datalabel") <- paste0("Protective Policy Index M1, Region-Day file, ", format(Sys.Date(), "%Y-%m-%d"))
varl.corr <- c("isocode"="ISO 3166 country code",
               "isoabbr"="ISO 3166 (2c) country code",
               "state_province"="Region name",
               "iso_state"="ISO 3166 unit code",
               "ppi.all.tot"="Average total PPI",
               "ppi.all.nat"="Average national PPI",               
               "ppi.all.reg"="Average regional PPI")
varl.st <- rep("",ncol(y.w))
varl.st[match(names(varl.corr), colnames(y.w))] <- varl.corr

attr(y.w, "var.labels") <- varl.st
write.dta(y.w, temp_loc)  

if (file.exists(fn)) file.remove(fn)
zip(fn, temp_loc, flags="-r9Xj")

## regions v2
y <- subset(ppi.regions, template=="n", select=c("ccode", "rcode", "date","field","reg","nat","tot")) 
y.w <- tidyr::pivot_wider(y, names_from=field, values_from=c(reg, nat, tot), names_sep=".")
y.w <- within(y.w, {
  isocode <- ccode
  isoabbr <- countrycode(isocode, origin="iso3n", destination="iso2c")
  state_province <- state_province[rcode]
  iso_state <- iso_state[rcode]
})

y.w <- y.w[c("isocode","isoabbr","state_province","iso_state","date", labels.n$field)]
colnames(y.w) <- c("isocode","isoabbr","state_province","iso_state","date", paste0(labels.n$label, ".2"))

y.w.li <- split(y.w, y.w$isoabbr)
for (cn in names(y.w.li)) {
  temp <- y.w.li[[cn]][order(y.w.li[[cn]][["date"]], y.w.li[[cn]][["iso_state"]]),]
  write.table(temp, paste0(regm2,"/PPI_regions_",cn,"_m2.csv"), append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)
}

# export as an archive with a Stata file
fn <- file.path(dfp_github,"PPI_regions_m2_dta.zip")
temp_loc <- file.path(tempdir(),"PPI_regions_m2.dta")
attr(y.w, "datalabel") <- paste0("Protective Policy Index M2, Region-Day file, ", format(Sys.Date(), "%Y-%m-%d"))
varl.corr <- c("isocode"="ISO 3166 country code",
               "isoabbr"="ISO 3166 (2c) country code",
               "state_province"="Region name",
               "iso_state"="ISO 3166 unit code",
               "ppi.all.tot.2"="Average total PPI",
               "ppi.all.nat.2"="Average national PPI",               
               "ppi.all.reg.2"="Average regional PPI")
varl.st <- rep("",ncol(y.w))
varl.st[match(names(varl.corr), colnames(y.w))] <- varl.corr

attr(y.w, "var.labels") <- varl.st
write.dta(y.w, temp_loc)  

if (file.exists(fn)) file.remove(fn)
zip(fn, temp_loc, flags="-r9Xj")



