library(countrycode)
library(ggplot2)
library(emIRT)

set.seed(123)

options(stringsAsFactors = FALSE)
theme_set(theme_bw() + theme(strip.text.x =element_text(size=12, family="sans"), 
                             strip.text.y =element_text(size=12, family="sans"),  
                             axis.title.x =element_text(size=12, family="sans"), 
                             axis.title.y =element_text(size=12, family="sans"), 
                             axis.text=element_text(size=12, family="sans"),
                             legend.text=element_text(size=12, family="sans")                             
))


rm(list=ls())

indir <- "C:/Users/az310/Dropbox/COVID and institutions/GitHub/PPI-data/ingredients"
outdir <- "C:/Users/az310/Dropbox/COVID and institutions/GitHub/PPI-data/misc"

oxfn <- paste0("OxCGRT_", format(Sys.Date(), "%Y%m%d"), ".csv")
download.file("https://raw.github.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv", file.path(outdir, oxfn), mode="wb")
ox.wp <- read.csv(file.path(outdir, oxfn))

geo.grid <- read.csv(file.path(indir, "population_template.csv"), encoding = "UTF-8")

start.date.train <- as.Date("2020-1-24") 
end.date.train <- as.Date("2020-7-31")
start.date.test <- as.Date("2020-1-24") 
end.date.test <- as.Date("2020-4-24")
ccodes <- countrycode::countrycode("US", origin = "iso2c", destination="iso3n")

ppi.regions <- readRDS(file.path(indir, "ppi_regions.rds"))
ppi.country <- readRDS(file.path(indir, "ppi_country.rds"))

## compute the IRT based values
sam <- subset(ppi.regions, date >= start.date.train & date <= end.date.train & group != "all" &
                (template == "o"|(template=="n" & group=="dist_mand")),
              select=c("ccode","rcode","date","group","val","popw"))

sam1 <- subset(sam, popw>-99)
sam2 <- subset(sam, popw==-99)
sam2 <- sam2[!duplicated(sam2[c("ccode","date","group")]),]
sam2$rcode <- paste0(countrycode::countrycode(sam2$ccode, "iso3n", "iso2c"), "99")
sam <- rbind(sam1, sam2)

sam$t <- as.numeric(sam$date-min(sam$date))+1
vars <- unique(sam$group)
sam.w <- tidyr::pivot_wider(data=sam, names_from=group, values_from=val)
class(sam.w) <- "data.frame"
works <- apply(sam.w[,vars], 2, function(u) {
  f <- factor(u)
  l <- levels(f)
  if (length(l) <= 3) return(as.numeric(f))
  else return(findInterval(u, c(-1,1/3,2/3,4/3)))
})

rownames(works) <- NULL

start.values <- list(
  alpha = matrix(0,ncol(works),1),
  beta = matrix(0.5,ncol(works),1),
  x = as.matrix(-1 + sam$t*2/max(sam$t)),
  DD = matrix(0.5,ncol(works),1),
  tau = matrix(-0.5,ncol(works),1)
)

priors <-list(
  x=list(mu=as.matrix(0),
         sigma=as.matrix(1)),
  beta=list(mu=matrix(0,2,1),
            sigma=matrix(c(25,0,0,25), nrow=2, ncol=2))
)

fit <- ordIRT(.rc=works,
              .starts = start.values,
              .priors = priors,
              .D = 1L,
              .control = {list(verbose = TRUE, thresh = 1e-6, maxit = 500)})
estimates <- data.frame(sam.w[c("rcode","date")], ind2=fit$means$x)
saveRDS(fit, file.path(outdir, "fitted_IRT.rds"))
saveRDS(estimates, file.path(outdir,"index_IRT.rds"))

# Comparisons against Oxford Stringency Index and IRT-based index
est <- readRDS(file.path(outdir, "index_IRT.rds"))
ox.wp$date <- as.Date(as.character(ox.wp$Date), format="%Y%m%d")

ox.li <- list(
  us = subset(ox.wp, date>=start.date.train & date<=end.date.train & CountryCode=="USA" & Jurisdiction=="STATE_TOTAL",
              select=c("RegionCode","date","StringencyIndex")),
  xn = subset(ox.wp, date>=start.date.train & date<=end.date.train & Jurisdiction=="NAT_TOTAL",
              select=c("CountryName","date","StringencyIndex"))
)

ox.li[["us"]] <- within(ox.li[["us"]], {
  rcode <- gsub("_","",RegionCode)
  RegionCode <- NULL
})

ox.li[["xn"]] <- within(ox.li[["xn"]], {
  ccode <- countryname(CountryName, "iso3n")
})


## compare cross US states
test <- subset(ppi.regions, ccode %in% ccodes & category=="ppi" & group=="all", select=c("rcode","date","template","score.s","score.n","val"))
test <- dplyr::inner_join(est, test, by=c("rcode","date"))
test <- dplyr::left_join(test, ox.li$us, by=c("rcode","date"))
lapply(split(test, test$template), function(x) cor(x[c("val","ind2","StringencyIndex")]))

test$template <- ifelse(test$template=="o", "method 1", "method 2")
test$time <- "Jan.-July 2020 pooled"
slice <- subset(test, date==end.date.test)
slice$time <- "24-Apr.-2020"
lapply(split(slice, slice$template), function(x) cor(x[c("val","ind2","StringencyIndex")]))
 

slices <- rbind(test, slice)
pic <- ggplot(slices) + 
  geom_point(aes(x=val, y=ind2)) + 
  facet_grid(template ~ time) +
  labs(x="Total Protective Policy Index", y="IRT-based index")
ggsave(file.path(outdir, "index_xv_by_state.png"), pic, width=6, height=5)


pic <- ggplot(slices) + 
  geom_point(aes(x=val, y=StringencyIndex)) + 
  facet_grid(template ~ time) +
  labs(x="Total Protective Policy Index", y="Oxford Stringency Index")
pic
ggsave(file.path(outdir, "index_ox_by_state.png"), pic, width=6, height=5)


## compare cross-nationally
w <- subset(geo.grid, include=="Y", select=c("ccode","rcode","population"))
w$popw <- as.numeric(w$population)/10000
w1 <- unique(subset(w, !is.na(population), select=c("ccode","rcode","popw")))
w2 <- unique(subset(w, is.na(population), select=c("ccode")))
w2$rcode <- paste0(countrycode::countrycode(w2$ccode, "iso3n", "iso2c"), "99")
w2$popw <- 1
w <- rbind(w1, w2)
bulk <- dplyr::inner_join(est, w, by="rcode")
bulk <- within(bulk, {
  ind2 <- ind2*popw
})
agg <- dplyr::summarize(.data=dplyr::group_by(.data=bulk, ccode, date), ind2=sum(ind2), den=sum(popw))
agg$ind2 <- agg$ind2/agg$den
test <- subset(ppi.country, category=="ppi" & group=="all", select=c("ccode","date","template","score.s","score.n","val"))
test <- dplyr::inner_join(agg, test, by=c("ccode","date"))
test <- dplyr::left_join(test, ox.li$xn, by=c("ccode","date"))
lapply(split(test, test$template), function(x) cor(x[c("val","ind2","StringencyIndex")]))

test$template <- ifelse(test$template=="o", "method 1", "method 2")
test$time <- "Jan.-July 2020 pooled"
slice <- subset(test, date==end.date.test)
slice$time <- "24-Apr.-2020"
lapply(split(slice, slice$template), function(x) cor(x[c("val","ind2","StringencyIndex")]))

slices <- rbind(test, slice)
pic <- ggplot(slices) + 
  geom_point(aes(x=val, y=ind2)) + 
  facet_grid(template ~ time) +
  labs(x="Average Total Protective Policy Index", y="Average IRT-based index")
ggsave(file.path(outdir,"index_xv_by_country.png"), pic, width=6, height=5)

pic <- ggplot(slices) + 
  geom_point(aes(x=val, y=StringencyIndex)) + 
  facet_grid(template ~ time) +
  labs(x="Average Total Protective Policy Index", y="Oxford Stringency Index")
ggsave(file.path(outdir, "index_ox_by_country.png"), pic, width=6, height=5)






