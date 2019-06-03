### male first for testing
## TODO analyse both genders later 
## tidy data would be better

library(devtools)


## start with one disease, then do a loop for all diseases by age and sex.  
## inputs are rates
## example for crc (colon and rectum cancer) for females. We need a loop to create: sex and outcomes (diseases) input tables.
## may be better to have something that looksup for the inputs, e.g. incidence_rate_crc
## here only females

inc <- read.csv("MSLT/data/city regions/bristol/dismod/input_data.csv", nrow=20, header=TRUE)[,c('sex', 'age', 'incidence_rate_crc')]
mort <- read.csv("MSLT/data/city regions/bristol/dismod/input_data.csv", nrow=20, header=TRUE)[,c('deaths_rate_crc')]
prev <- read.csv("MSLT/data/city regions/bristol/dismod/input_data.csv", nrow=20, header=TRUE)[,c('prevalence_rate_crc')]
pop <- read.csv("MSLT/data/city regions/bristol/dismod/input_data.csv", nrow=20, header=TRUE)[,c('population_number')]
# names(inc) <- c("agegrp","incmale","incfemale")
# names(mort) <- c("mortmale","mortfemale")
# names(prev) <- c("prevmale","prevfemale")
# names(pop) <- c("popmale","popfemale")
inputs <- cbind(inc, prev, mort, pop)
# inputs$remmale <- 0
inputs$rem <- 0
inputs$prevdenom <- c(100,100,500,500,500,500,500,500,500,500,500,500,500,500,500,500,200,200,100,100) / 10 # total sample size 3910, generous for London (from CJ)
## Added age group (Belen)
inputs$agegrp <- seq(0,95, by=5)

## change column name incidence_rate_crc to inc to match disbayes code

colnames(inputs)[colnames(inputs)=="incidence_rate_crc"] <- "inc"


## Stretch data from 5-year age groups to by year of age 
outage <- 0:100  # assume inc/prev/mort same in each year within a five-year age group
ind <- findInterval(outage, inputs$agegr)
inputs <- inputs[ind,]
inputs$age <- outage

View(inputs)

## Adjusted do create data base for females only
## Just doing the first 20 years

inputs <- within(inputs, {
    ningrp <- rep(table(agegrp), table(agegrp))
    # popmale <- round(popmale/ningrp) ## assume population uniform between years within age group.
    pop <- round(pop/ningrp) ## assume population uniform between years within age group.
    # ndieddismale <- round(popmale * (1 - exp(-mortmale)))
    ndieddis <- round(pop * (1 - exp(-mort)))
    # prevnmale <- round(prevdenom * prevmale)
    prevn <- round(prevdenom * prev)
})
crc_females <- as.data.frame(inputs)
save(crc_females, file="MSLT/data/city regions/bristol/dismod/crc_females.rda")


### The below seems to be to compare results. But, unsure whether I am missing inputs to run stan..

use_data(crc_females)

outputs <- read.csv("second/outputs.csv", skip=103, nrow=101)[,-1]
names(outputs) <- c("age","inc","prev","rem","mort","incout","prevout","remout","cfout","durout","mortout","rrout","ageonset")

## Dismod outputs from my run
## No smoothing, using inc and mort data only, no prev data. 
outmine <- read.csv("second/outputs_mine.csv", skip=4, nrows=21, header=FALSE)[,1:13]
names(outmine) <- c("agegrp", "inc","rem","mort","inc2","prev","rem2","cf","dur","mort","rrmort","rrmortn","ageonset")
outmine$agegrp <- seq(0, 100, by=5)
points(outmine$agegrp, outmine$cf, col="purple", pch=19)
