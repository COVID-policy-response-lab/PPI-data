library(countrycode)
library(openxlsx)

rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("C:/Users/az310/Dropbox/COVID and institutions/GitHub/PPI-data")

indir <- "./ingredients"
outdir <- "./prelim"

# PPI data - weights
weights <- c(o="old weights", n="weights v2")
weights <- lapply(names(weights), function(x) {
  y <- read.xlsx(file.path(indir, "PPI weights.xlsx"), sheet=weights[x])
  y$template <- x
  y[c("template","category","group","score")]
})
weights <- do.call("rbind",weights) 

# PPI data - chunks
fl1 <- load(file.path(indir,"data_chunks.RData"))
start.date <- as.Date("2020-1-05")
def.end.date <- as.Date("2020-4-30")
max.end.date <- max(dtrange$end.date)
dtrange$ccode <- countrycode(dtrange$ccodealp, "iso3c", "iso3n")

# geographic grid
geo.grid <- read.csv(file.path(indir,"population_template.csv"), encoding = "UTF-8")

# something new
new.collabs <- c(
air_bord="Air borders",
land_bord="Land borders",
sea_bord="Sea borders",
border="Borders",
soc_gath="Social gatherings",
schools="School closures",
emerg="State of emergency",
venues="Entertainment venues",
restrts="Restaurants",
ne_busn="Nonessential businesses",
gov_offs="Government offices",
wfh="Work from home",
ind_mob="Individual mobility",
med_stay="Self-isolation of exposed",
med_quar="Quarantine of exposed",
pub_transp="Public transportation",
masks="Masks",
dist_mand="Social distancing")

# combine old and new chunks
chunks.o$template <- "o" 
chunks.n$template <- "n"

bulk2 <- list(chunks.o, chunks.n)
bulk2 <- lapply(bulk2, "[", c("ccode","rcode","subnational","date","group","template","score"))
bulk2[["make.row.names"]] <- FALSE
bulk2 <- do.call("rbind", bulk2)
bulk2 <- aggregate(score ~ ccode + rcode + subnational + group + template + date, data=bulk2, FUN=max)
rm(list=setdiff(fl1, "dtrange"))

# check if new grid entries needed
setdiff(unique(bulk2$ccode), unique(geo.grid$ccode))
countrycode(setdiff(unique(bulk2$ccode), unique(geo.grid$ccode)), "iso3n", "iso2c") 

# check if required population weights missing in the grid
maxval <- aggregate(score ~ ccode, data=subset(bulk2, rcode !='ZZ'), FUN=max)
weights.needed <- subset(maxval, score >0)[["ccode"]]

pop.master <- subset(geo.grid, !is.na(population))

(check <- setdiff(weights.needed, unique(pop.master$ccode)))

# build a grid
geo.grid.x <- subset(geo.grid, include=="Y" & ccode %in% bulk2$ccode)
pop.tot <- aggregate(population ~ ccode, data=subset(geo.grid.x, !is.na(population)), FUN=sum)

geo.grid.x <- merge(geo.grid.x, pop.tot, by="ccode", all.x=TRUE)
geo.grid.x <- within(geo.grid.x, popw <- population.x/population.y )
geo.grid.x <- within(geo.grid.x, {
  end.date <- dtrange[match(ccode, dtrange$ccode),"end.date"]
  end.date[is.na(end.date)] <- def.end.date
})

## find the combined ppi components and total
# geographic grid
geo.grid.li <- subset(geo.grid.x, (ccode %in% weights.needed), select = c("ccode","rcode","popw","end.date"))
geo.grid.li <- lapply(split(geo.grid.li, geo.grid.li$ccode), function(x) {
  within(x, geo.id <- as.numeric(factor(rcode)))
})
names(geo.grid.li) <- paste0("w.",names(geo.grid.li))
geo.grid.li[["nw"]] <- subset(geo.grid.x, !(ccode %in% weights.needed), select = c("ccode","rcode","end.date"))
geo.grid.li[["nw"]][["popw"]] <- -99
geo.grid.li[["nw"]][["geo.id"]] <- as.numeric(factor(geo.grid.li[["nw"]][["ccode"]]))

gnam <-  names(geo.grid.li)
names(gnam) <- gnam

geo.xwalk <- lapply(gnam, function(j) {
  if (j=="nw") {
    u <- unique(geo.grid.li[[j]][c("geo.id","ccode")])
    u <- within(u, rcode <- 'ZZ')
  } else {
    u <- geo.grid.li[[j]][c("geo.id","ccode","rcode")]
    v <- within(u, rcode <- 'ZZ')
    u <- rbind(u,v)
  }
  within(u, gnam <- j)
})
geo.xwalk <- do.call("rbind", geo.xwalk)

geo.xwalk2 <- lapply(gnam, function(j) {
  u <- geo.grid.li[[j]]
  within(u, {
    gnam <- j
    end.date <- NULL
    })
})
geo.xwalk2 <- do.call("rbind", geo.xwalk2)

ppi.parts.grid <- lapply(gnam, function(j) {
  y  <- unique(geo.grid.li[[j]][c("geo.id","end.date")])
  y$gnam <- j
  y
  })
ppi.parts.grid <- do.call("rbind", ppi.parts.grid  )

# policies grid
group.grid <- unique(weights[c("template","group","category")])
totals <- list()
totals[["group"]] <- aggregate(score ~ group + category + template, data=weights, FUN=max)
totals[["category"]] <- aggregate(score ~ category + template, data=totals[["group"]], FUN=sum)
totals[["category"]][["group"]] <- "all"
totals[["all"]] <- aggregate(score ~ template, data=totals[["group"]], FUN=sum)
totals[["all"]][["group"]] <- "all"
totals[["all"]][["category"]] <- "ppi"
totals <- do.call("rbind", c(totals, make.row.names = FALSE))

# time grid
time.grid <- data.frame(date=seq(start.date,max.end.date,by=1))
time.grid$counter <- seq_len(nrow(time.grid))

ppi.parts.grid <- 
  dplyr::filter(  
  dplyr::inner_join(  
  dplyr::inner_join(
  ppi.parts.grid, group.grid, by=character()),
  time.grid, by=character()),  
  date <= end.date   
)
ppi.parts.grid$date <- ppi.parts.grid$end.date <- NULL 

# add information
bulk3 <- merge(bulk2, time.grid, by="date")
bulk3 <- merge(bulk3,geo.xwalk, by=c("ccode","rcode"))

ppi.parts0 <- list()
ppi.parts0[["score.s"]] <- subset(bulk3, subnational==1 & !is.na(score), select=c("gnam","geo.id","counter","template","group","score"))
temp <- within(subset(bulk3, subnational==0 & !is.na(score)), priority <- ifelse(rcode=="ZZ", 0, 1))
temp <- dplyr::inner_join(
  temp,
  dplyr::summarize(dplyr::group_by(.data=temp, gnam, geo.id, template, group, counter), priority=max(priority)))
ppi.parts0[["score.n"]] <- temp[c("gnam","geo.id","counter","template","group","score")]

## stretch data
initial.b <- dplyr::summarize(dplyr::group_by(.data=ppi.parts.grid, gnam, geo.id, template, group), counter=min(counter)-1)
initial.b$score <- 0
initial.e <- dplyr::summarize(.data=dplyr::group_by(.data=ppi.parts.grid, gnam, geo.id, template, group), counter=max(counter)+1)
initial.e$score <- -99
initial <- rbind(as.data.frame(initial.b), as.data.frame(initial.e), make.row.names=FALSE)

ppi.parts <- ppi.parts.grid
for (j in paste0("score",c(".s",".n"))) {
  t <- ppi.parts0[[j]][c("score", "gnam", "geo.id", "template", "group","counter")]
  t <- subset(t, !is.na(score))
  t <- dplyr::filter(
    dplyr::left_join(t, initial.e, by=c("gnam", "geo.id", "template", "group"), suffix=c("",".e")),
                        counter < counter.e)
  t$counter.e <- t$score.e <- NULL
  start <- rbind(initial, t)  
  
  z <- dplyr::filter(
    .data=dplyr::full_join(start, start[c("gnam", "geo.id", "template", "group","counter")], 
                           by=c("gnam", "geo.id", "template", "group"), suffix=c(".b",".e")),
    counter.e > counter.b)
  keeper <- dplyr::summarize(
    .data=dplyr::group_by(.data=z, gnam, geo.id, template, group, counter.b),
    counter.e=min(counter.e))
  z <- dplyr::filter(
    .data=dplyr::inner_join(
      z, keeper, 
      by=c("gnam", "geo.id", "template", "group","counter.e","counter.b")),
    counter.e-counter.b >1)
  
  z$diff <- z$counter.e-z$counter.b - 1
  q <- dplyr::summarize(.data=dplyr::group_by(.data=z, gnam, group, template), mdiff=max(diff))
  q <- apply(q, 1, function(u) {
    data.frame(add=seq_len(u["mdiff"]), as.list(u))
    })
  q[["make.row.names"]] <- FALSE
  q <- do.call("rbind", q)
  q$mdiff <- NULL
  
  z.long <- dplyr::mutate(
    dplyr::filter(
      .data=dplyr::inner_join(
        z,
        q,
        by=c("gnam", "group", "template")),
      add <= diff),
    counter = counter.b + add, 
    counter.b=NULL,
    add=NULL, diff=NULL,counter.e=NULL
  )
  z.long <- rbind(t, z.long)
  ppi.parts <- dplyr::left_join(
    ppi.parts, 
    dplyr::rename(z.long, !!j := score),
    by=c("gnam", "geo.id", "template", "group","counter")
  )
} 
ppi.parts$val <- pmax(ppi.parts$score.s, ppi.parts$score.n)

### add regional aggregates
ppi.categories <- dplyr::summarize(
  .data=dplyr::group_by(
    .data=ppi.parts,
    gnam, geo.id, counter, template, category),
  score.s=sum(score.s), score.n=sum(score.n), val=sum(val))
ppi.categories[["group"]] <- "all"
ppi.overall <- dplyr::summarize(
  .data=dplyr::group_by(
    .data=ppi.parts,
    gnam, geo.id, counter, template),
  score.s=sum(score.s), score.n=sum(score.n), val=sum(val))  
ppi.overall[["group"]] <- "all"
ppi.overall[["category"]] <- "ppi" 

## region combinations
nam.country <- c("ccode","date","template","category","group","score.s","score.n","val")
nam.regions <- c("ccode","rcode","date","template","category","group","score.s","score.n","val","popw")

ppi.regions0 <- do.call("rbind", list(ppi.parts, ppi.categories, ppi.overall, make.row.names=FALSE))
ppi.regions0 <- dplyr::mutate(
  dplyr::inner_join(
  dplyr::inner_join(
    ppi.regions0, totals, by= c("template","group","category")),
  time.grid, by="counter"),
  score.s = score.s/score,
  score.n = score.n/score,  
  val = val/score)

ppi.regions <- dplyr::inner_join(ppi.regions0, geo.xwalk2, by=c("gnam","geo.id"))
ppi.regions <- ppi.regions[nam.regions]

### country average
ppi.country <- list()
ppi.country[["nw"]] <- dplyr::inner_join(subset(ppi.regions0, gnam=="nw"), geo.xwalk, by = c("gnam","geo.id")) 
ppi.country[["nw"]][["score.s"]] <- 0
ppi.country[["nw"]][["val"]] <- ppi.country[["nw"]][["score.n"]]
ppi.country[["nw"]] <- ppi.country[["nw"]][nam.country]

ppi.country[["w"]] <- dplyr::inner_join(subset(ppi.regions0, gnam!="nw"), geo.xwalk2, by = c("gnam","geo.id")) 
ppi.country[["w"]] <- within(ppi.country[["w"]], {
  score.s.p <- score.s*popw
  score.n.p <- score.n*popw
  val.p <- val*popw
})
ppi.country[["w"]] <- dplyr::summarize(
  .data=dplyr::group_by(
    .data=ppi.country[["w"]],
    ccode, date, template, category, group),
  score.s=sum(score.s.p), score.n=sum(score.n.p), val=sum(val.p), popw=sum(popw))
ppi.country[["w"]] <- within(ppi.country[["w"]], {
  score.n <- score.n/popw
  score.s <- score.s/popw
  val <- val/popw
})
ppi.country[["w"]] <- ppi.country[["w"]][nam.country]
ppi.country[["make.row.names"]] <- FALSE
ppi.country <- do.call("rbind", ppi.country)

saveRDS(ppi.regions, file=file.path(indir,"ppi_regions.rds"))
saveRDS(ppi.country, file=file.path(indir,"ppi_country.rds"))
saveRDS(weights, file=file.path(indir,"ppi_weights.rds"))

###################################################################################################
################################### EXPORTS FOR CHECKS ############################################
###################################################################################################

ccodes <- list()
ccodes[["n"]] <- unique(subset(ppi.regions, template=="n", select="ccode", drop=TRUE))
ccodes[["n-w"]] <- unique(subset(ppi.regions, template=="n" & popw!=-99, select="ccode", drop=TRUE))
ccodes[["o"]] <- unique(subset(ppi.regions, template=="o", select="ccode", drop=TRUE))
ccodes[["o-w"]] <- unique(subset(ppi.regions, template=="o" & popw!=-99, select="ccode", drop=TRUE))
ccodes <- lapply(ccodes, function(x) {
  y <- x
  names(y) <- countrycode(x, "iso3n", "iso3c")
  y
})

ppi.regions <- within(ppi.regions, {
  field <- paste(category, group, sep=".")
  reg <- round(score.s, 4)
  nat <- round(score.n, 4)
  tot <- round(val, 4)
  })

ppi.country <- within(ppi.country, {
  field <- paste(category, group, sep=".")
  reg <- round(score.s, 4)
  nat <- round(score.n, 4)
  tot <- round(val, 4)
})

labels <- unique(ppi.country[c("template","category","group","field")])
sort.cat <- data.frame(category = sort(unique(labels$category)), sort.cat=seq_along(unique(labels$category)))
sort.cat$sort.cat[sort.cat$category=="ppi"] <- 0
sort.gr <- data.frame(group = sort(unique(labels$group)), sort.gr=seq_along(unique(labels$group)))
sort.gr$sort.gr[sort.gr$group=="all"] <- 0
labels <- merge(labels,sort.cat)
labels <- merge(labels,sort.gr)
labels <- merge(labels, data.frame(cov=c("tot","nat","reg"), sort.cov = c(0.1,0.2,0.3))) 
labels <- within(labels, {
  gr.label <- new.collabs[group]
  label <- paste0(field," [",cov,"]")
  gr.field <- field
  field <- paste0(cov,".",field)
  sort <- sort.cat*100+sort.gr + sort.cov
  })
labels.n <- subset(labels[order(labels$sort),], template=="n")
labels.o <- subset(labels[order(labels$sort),], template=="o")

## old scores -- country
output <- lapply(ccodes[["o"]], function(j) { 
  df.list <- list()
  if (j %in% ccodes[["o"]]) {
  y <- subset(ppi.country, template=="o" & ccode==j, select=c("date","field","reg","nat","tot")) 
  y.w <- tidyr::pivot_wider(y, names_from=field, values_from=c(reg, nat, tot), names_sep=".")
  y.w$iso2c <- countrycode(j, "iso3n", "iso2c") 
  y.w$country <- countrycode(j, "iso3n", "country.name")
  y.w <- y.w[order(y.w$date), c("iso2c","country","date", labels.o$field)]
  colnames(y.w) <- c("iso2c","country","date", labels.o$label)
  df.list[["Country - Old Formula"]] <- as.data.frame(y.w)
  }
  if (j %in% ccodes[["n"]]) {
    y <- subset(ppi.country, template=="n" & ccode==j, select=c("date","field","reg","nat","tot"))
    y.w <- tidyr::pivot_wider(y, names_from=field, values_from=c(reg, nat, tot), names_sep=".")
    y.w$iso2c <- countrycode(j, "iso3n", "iso2c")
    y.w$country <- countrycode(j, "iso3n", "country.name")
    y.w <- y.w[order(y.w$date), c("iso2c","country","date", labels.n$field)]
    colnames(y.w) <- c("iso2c","country","date", labels.n$label)
    df.list[["Country - New Formula"]] <- as.data.frame(y.w)
  }
  if (j %in% ccodes[["o-w"]]) {
    y <- subset(ppi.regions, template=="o" & ccode==j, select=c("rcode","date","field","reg","nat","tot"))
    y.w <- tidyr::pivot_wider(y, names_from=field, values_from=c(reg, nat, tot), names_sep=".")
    y.w <- merge(y.w, subset(geo.grid.x, ccode==j, select=c("rcode","iso_state","country","name")), by="rcode")
    y.w <- y.w[order(y.w$iso_state, y.w$date), c("iso_state","name", "date", labels.o$field)]
    colnames(y.w) <- c("iso_state","name","date",labels.o$label) 
    df.list[["Regions - Old Formula"]] <- as.data.frame(y.w)
  }  
  if (j %in% ccodes[["n-w"]]) {
    y <- subset(ppi.regions, template=="n" & ccode==j, select=c("rcode","date","field","reg","nat","tot"))
    y.w <- tidyr::pivot_wider(y, names_from=field, values_from=c(reg, nat, tot), names_sep=".")
    y.w <- merge(y.w, subset(geo.grid.x, ccode==j, select=c("rcode","iso_state","country","name")), by="rcode")
    y.w <- y.w[order(y.w$iso_state, y.w$date), c("iso_state","name","date", labels.n$field)]
    colnames(y.w) <- c("iso_state","name","date",labels.n$label) 
    df.list[["Regions - New Formula"]] <- as.data.frame(y.w)
  }
  df.list
})


## tab for checking
pal <- read.table(header=TRUE, sep=",", text= "
lo,hi,hex
0,0.1,ffffcc 
0.15,0.3,ffeda0
0.3,0.45,fed976
0.45,0.6,feb24c
0.6,0.75,fd8d3c
0.75,0.9,e31a1c 
0.9,1.1,800026
") 
pal$hex <- paste0("#", pal$hex)

lab.x <- c(score.s="regional", score.n="national")
ppi.reg.x <- subset(ppi.regions, template=="n" & group != 'all', select=c("ccode","rcode","date","field","score.s","score.n"))
new <- lapply(ccodes[["n"]], function(j) {
  y <- subset(ppi.reg.x, ccode==j, select=c("rcode","date","field","score.s","score.n")) 
  if (j %in% ccodes[["n-w"]]) {
    y <- dplyr::inner_join(y,subset(geo.grid.x, ccode==j, select=c("rcode","iso_state","name")),by="rcode")
  } else {
    y <- dplyr::summarize(.data=dplyr::group_by(.data=y, field, date), score.n=max(score.n))
    y$score.s <- 0
    y$iso_state <- countrycode(j,"iso3n","iso2c")
    y$name <- paste0(countrycode(j,"iso3n","country.name"), ": All regions")
  }
  min.dt <- min(subset(y, score.s >0 | score.n >0, select="date", drop=TRUE))
  y$dt <- as.numeric(y$date - min.dt)
  collabs <- data.frame(dt = seq(0,max(y$dt)))
  collabs <- within(collabs, {
    field <- paste0("value.", dt)
    lab <- format(min.dt + dt, "%b.%d")
  })
  collabs <- collabs[order(collabs$dt),] 
  y <- subset(y, dt >=0, select= c("iso_state","name","dt","field","score.s","score.n"))
  y.l <- tidyr::pivot_longer(y, cols=starts_with("score"), names_to="level", values_to="value")
  y.l$level <- lab.x[y.l$level]
  y.w <- tidyr::pivot_wider(y.l, names_from=dt, values_from=value, names_prefix="value.")
  y.w$label  <- labels.n[match(y.w$field, labels.n$gr.field), "gr.label"]
  y.w <- y.w[order(y.w$iso_state, y.w$field, y.w$level), c("iso_state","name","field","label","level", collabs$field) ]
  colnames(y.w) <- c("iso_state","name","field","label","level", collabs$lab)
  y.w
})

#### export
for (j in names(output)) {
  wb <- createWorkbook() 
  for (k in names(output[[j]])) {
    addWorksheet(wb, sheetName=k)
    tryCatch(writeData(wb, sheet = k, x = output[[j]][[k]]), error=function(e) message(paste0("error in ",j)))
   setColWidths(wb, sheet = k, cols = 1:ncol(output[[j]][[k]]), widths = "auto")
  }
  if (!is.null(new[[j]])) {
    addWorksheet(wb, sheetName="Check Consistency")
   writeData(wb, sheet = "Check Consistency",x=new[[j]])
    setColWidths(wb, sheet = "Check Consistency", cols = 1:ncol(new[[j]]), widths = "auto") 
    
    conditionalFormatting(wb, "Check Consistency",
                            cols = 6:ncol(new[[j]]), rows = 2:nrow(new[[j]]), type="colourScale", rule=c(0,1), style = c("#ffffcc","#800026"))
  }
  saveWorkbook(wb, file.path(outdir, paste0("PPI Info (",countrycode(j, "iso3c", "country.name"),").xlsx")), overwrite=TRUE) 
}
