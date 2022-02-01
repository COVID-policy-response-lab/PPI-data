library(countrycode)
library(openxlsx)

rm(list=ls())

options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/COVID and institutions/GitHub/PPI-data")

indir <- normalizePath("ingredients")
dfp_github <- normalizePath("data")

regm1 <- file.path(dfp_github, "PPI_regions_m1")
regm2 <- file.path(dfp_github, "PPI_regions_m2")

# PPI data
weights <- readRDS(file.path(indir, "ppi_weights.rds"))
totals <- aggregate(score ~ group, data=subset(weights, template=="o"), FUN=max)
totals.vec <- setNames(totals$score, totals$group)

geo.grid <- read.csv(file.path(indir, "population_template.csv"), encoding = "UTF-8")
geo.grid.x <- unique(subset(geo.grid, include=="Y", select=c("ccode","rcode","iso_state","name")))
 
meta <- readRDS(file.path(indir, "PPI_meta.rds"))
meta <- merge(meta, totals, by="group")
meta <- within(meta, ups <- round(score.x/score.y,5))

ppi.regions <- readRDS(file.path(indir, "ppi_regions.rds"))
novar <- unique(subset(ppi.regions, popw <0, select="ccode", drop=TRUE))

ppi.regions.x <- subset(ppi.regions, template=="o" & !ccode %in% novar & group!="all")
meta.regions <- subset(meta, group!="all")

start.date <- as.Date("2020-1-1")

# by region
ppi.current <- list()
ppi.current[["nat"]] <- within(ppi.regions.x, {
  counter <- as.numeric(date-start.date)
  score <- score.n
})[c("ccode","rcode","date","counter","group","score")]
ppi.current[["nat"]]$subnational <- 0
ppi.current[["sub"]] <- within(ppi.regions.x, {
  counter <- as.numeric(date-start.date)
  score <- score.s
})[c("ccode","rcode","date","counter","group","score")]
ppi.current[["sub"]]$subnational <- 1
ppi.current <- do.call("rbind", ppi.current)

ppi.lagged <- within(ppi.current, counter <- counter+1)[c("ccode","subnational","rcode","counter","group","score")]
ppi.regions.x <- dplyr::inner_join(ppi.current, ppi.lagged,by=c("ccode","subnational","rcode","counter","group"), suffix=c("",".l1"))
ppi.regions.x <- within(ppi.regions.x, {
  total_change <- round((score-score.l1),3)
})
ppi.regions.x <- subset(ppi.regions.x, total_change >0)

core.meta.regions <- unique(meta.regions[c("ccode","rcode","subnational","group","date","branch")])
core.meta.regions <- core.meta.regions[with(core.meta.regions, order(ccode, rcode, subnational, group, date, branch)),]
core.meta.regions$nr <-1:nrow(core.meta.regions)
meta.regions <- merge(core.meta.regions,meta.regions)
meta.regions <- meta.regions[with(meta.regions, order(nr, rid)),]
meta.regions <- unique(meta.regions[c("nr","ccode","rcode","subnational","group","date","branch","who","institution","report_date","expiration_date","ups")])

countrywide <- subset(meta.regions, rcode == "ZZ")
countrywide$rcode <- NULL
regionspec <- rbind(subset(meta.regions, rcode != "ZZ"),
                    dplyr::inner_join(countrywide, unique(ppi.regions.x[c("ccode","rcode")])))

## combine into a meta file
meta.regions <- dplyr::left_join(ppi.regions.x, regionspec, by=c("ccode","rcode","subnational","group","date"))
iso_state <- setNames(geo.grid.x$iso_state, geo.grid.x$rcode)
state_province <- setNames(geo.grid.x$name, geo.grid.x$rcode)

branch.codes <- c(unk="missing", eld="executive leadership",jud="judiciary", bur="bureaucracy", leg="legislative", oth="other")

meta.regions <- within(meta.regions, {
  report_change <- round(ups-score.l1,3) 
  isocode <- ccode
  isoabbr <- countrycode(isocode, origin="iso3n", destination="iso2c")
  dimension <- group
  branch <- branch.codes[branch]
  state_province <- state_province[rcode]
  iso_state <- iso_state[rcode]
})
meta.colnames <- c("isocode","isoabbr","state_province","iso_state","date","dimension","subnational","total_change",
                   "branch","who","institution","report_date","expiration_date","report_change")

write.table(meta.regions[meta.colnames], file.path(dfp_github,"changes_regions_m1.csv"), append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)


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


