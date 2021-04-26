# PPI-data

This is an original dataset of stringency of public health policy measures that were adopted in response to COVID-19 worldwide by governments at different levels.

Developed by SUNY Binghamton COVID-19 Policy Response Lab.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r , echo=TRUE }
df <- data.frame(x=1:10, y=21:30)
library(ggplot2)
ggplot(df, aes(x=x,y=y) + geom_point()
```


## Team:

#### Binghamton University
* Olga Shvetsova, shvetso@binghamton.edu
* Abdul Basit Adeel, aadeel1@binghamton.edu
* Mert Can Bayar, mbayar1@binghamton.edu
* Onsel Gurel Bayrali, obayral1@binghamton.edu
* Michael Catalano, mcatala4@binghamton.edu
* Ezgi Muftuoglu, emuftuo1@binghamton.edu
* Tara Riggs, triggs1@binghamton.edu
* Didem Seyis, dseyis1@binghamton.edu 
* Mehmet Halit Sezgin, halitsezgin@gmail.com
* Tianyi Zhao, tzhao19@binghamton.edu

#### Beyond Binghamton University
* Olivia Catalano, olivia.catalano15@gmail.com
* Frank Giannelli, Rutgers, The State University of New Jersey, giannefr@shp.rutgers.edu
* Dina Rosenberg, Higher School of Economics, balalaeva@gmail.com
* Julie VanDusky-Allen, Boise State University, julievanduskyallen@boisestate.edu
* Andrei Zhirnov, University of Exeter, A.Zhirnov@exeter.ac.uk

### Former members
* Naveed Tahir, Syracuse University, ntahir@syr.edu
* Almira ...., 

## A Brief Description

To measure COVID-19 mitigation policy responses, we gathered data on policies that national and subnational policymakers adopted within fifteen public health categories: state of emergency, self-isolation and quarantine, border closures, limits on social gatherings, school closings, closure of entertainment venues, closure of restaurants, closure of non-essential businesses, closure of government offices, work from home requirements, lockdowns and curfews, public transportation closures, and mandatory wearing of PPE. We identify and code national and subnational public health policies for each subnational unit in 73 countries (subnational aggregates are presently published of USA and Canada only), including countries in North America, Central America, South America, Europe, the Middle East, and Asia. We rely primarily on government resources, press releases, and news sources, dating policies based on first announcement. Note that between and within the policy categories, there is variation on stringency, with some policy adoptions being more stringent than others (i.e. self-isolation versus lockdowns, partial school closings versus full school closings). To this end, we weighed more stringent policies in each category in the index more heavily. 

Based on coded public health policy responses to COVID-19, we calculate the Public Health Protective Policy Indices (PPI): Regional PPI for each subnational unit on each day; National PPI for a country on each day, based on national level policies; and Total PPI for each subnational unit on each day. The Total PPI reflects the strictest between the national and subnational policies adopted within each category for that unit for that day. The indices are scaled to range between 0 and 1. The Average Total PPI for each country-day is computed by weighing the different units' Total PPI values by the units' population shares. The indices apply solely to the measurable subnational and national public-health COVID-19 mitigation policy responses.
