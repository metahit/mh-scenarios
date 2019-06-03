### male first for testing
## TODO analyse both genders later 
## tidy data would be better

library(devtools)

inc <- read.csv("second/GreaterLondon/Collection/IHDIncidence.csv", skip=4, nrow=20, header=FALSE)[,c(1,2,6)]
mort <- read.csv("second/GreaterLondon/Collection/IHDMortality.csv", skip=4, nrow=20, header=FALSE)[,c(2,6)]
prev <- read.csv("second/GreaterLondon/Collection/IHDprevalence.csv", skip=4, nrow=20, header=FALSE)[,c(2,6)]
pop <- read.csv("second/GreaterLondon/Collection/Population.csv", skip=4, nrow=20, header=FALSE)[,c(2,3)]
names(inc) <- c("agegrp","incmale","incfemale")
names(mort) <- c("mortmale","mortfemale")
names(prev) <- c("prevmale","prevfemale")
names(pop) <- c("popmale","popfemale")
inputs <- cbind(inc, prev, mort, pop)
inputs$remmale <- 0
inputs$remfemale <- 0
inputs$prevdenom <- c(100,100,500,500,500,500,500,500,500,500,500,500,500,500,500,500,200,200,100,100) / 10 # total sample size 3910, generous for London

## Stretch data from 5-year age groups to by year of age 
outage <- 0:100  # assume inc/prev/mort same in each year within a five-year age group
ind <- findInterval(outage, inputs$agegrp)
inputs <- inputs[ind,]
inputs$age <- outage
inputs <- within(inputs, {
    ningrp <- rep(table(agegrp), table(agegrp))
    popmale <- round(popmale/ningrp) ## assume population uniform between years within age group.
    popfemale <- round(popfemale/ningrp) ## assume population uniform between years within age group.
    ndieddismale <- round(popmale * (1 - exp(-mortmale)))
    ndieddisfemale <- round(popfemale * (1 - exp(-mortfemale)))
    prevnmale <- round(prevdenom * prevmale)
    prevnfemale <- round(prevdenom * prevfemale)
})
ihdlondon <- inputs
save(ihdlondon, file="disbayes/ihdlondon.rda")

use_data(ihdlondon)

outputs <- read.csv("second/outputs.csv", skip=103, nrow=101)[,-1]
names(outputs) <- c("age","inc","prev","rem","mort","incout","prevout","remout","cfout","durout","mortout","rrout","ageonset")

## Dismod outputs from my run
## No smoothing, using inc and mort data only, no prev data. 
outmine <- read.csv("second/outputs_mine.csv", skip=4, nrows=21, header=FALSE)[,1:13]
names(outmine) <- c("agegrp", "inc","rem","mort","inc2","prev","rem2","cf","dur","mort","rrmort","rrmortn","ageonset")
outmine$agegrp <- seq(0, 100, by=5)
points(outmine$agegrp, outmine$cf, col="purple", pch=19)
