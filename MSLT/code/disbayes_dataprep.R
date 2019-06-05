## Adjustment of CJ script to generate data input for disbayes

library(devtools)

## Belen Comments: 

## start with one disease, then do a loop for all diseases by age and sex.  
## example for crc (colon and rectum cancer) for females.
## I commented script lines from the original CJ script that I did not use. 
inputs <- read.csv("MSLT/data/city regions/bristol/dismod/input_data.csv", nrow=20, header=TRUE)[,c('age', 'incidence_rate_crc', 
                                                                                                    'deaths_rate_crc', 
                                                                                                    'prevalence_rate_crc', 
                                                                                                    'population_number')]
# mort <- read.csv("MSLT/data/city regions/bristol/dismod/input_data.csv", nrow=20, header=TRUE)[,c('sex', 'age', 'deaths_rate_crc')]
# prev <- read.csv("MSLT/data/city regions/bristol/dismod/input_data.csv", nrow=20, header=TRUE)[,c('sex', 'age', 'prevalence_rate_crc')]
# pop <- read.csv("MSLT/data/city regions/bristol/dismod/input_data.csv", nrow=20, header=TRUE)[,c('sex', 'age', 'population_number')]
# names(inc) <- c("agegrp","incmale","incfemale")
# names(mort) <- c("mortmale","mortfemale")
# # names(prev) <- c("prevmale","prevfemale")
# # names(pop) <- c("popmale","popfemale")
# inputs <- cbind(inc, prev, mort, pop)
# # inputs$remmale <- 0
inputs$rem <- 0

## Belen comment: do we need to change this to adjust to the Bristol populatoin?

inputs$prevdenom <- c(100,100,500,500,500,500,500,500,500,500,500,500,500,500,500,500,200,200,100,100) / 10 # total sample size 3910, generous for London (from CJ)
## Added age group (Belen)
inputs$agegrp <- as.integer(seq(0,95, by=5))


## change column name incidence_rate_crc to inc to match disbayes code
## try to change this hard code

colnames(inputs)[colnames(inputs)=="incidence_rate_crc"] <- "inc"
colnames(inputs)[colnames(inputs)=="prevalence_rate_crc"] <- "prev"
colnames(inputs)[colnames(inputs)=="deaths_rate_crc"] <- "mort"
colnames(inputs)[colnames(inputs)=="population_number"] <- "pop"

## If incidence == 0, give a tiny number

## Stretch data from 5-year age groups to by year of age 
outage <- 0:100  # assume inc/prev/mort same in each year within a five-year age group
ind <- findInterval(outage, inputs$agegr)
inputs <- inputs[ind,]
inputs$age <- outage



View(inputs)

## Adjusted do create data base for females only


inputs <- within(inputs, {
    ningrp <- rep(table(agegrp), table(agegrp))
    # popmale <- round(popmale/ningrp) ## assume population uniform between years within age group.
    pop <- round(pop/ningrp) ## assume population uniform between years within age group.
    # ndieddismale <- round(popmale * (1 - exp(-mortmale)))
    ndieddis <- round(pop * (1 - exp(-mort)))
    # prevnmale <- round(prevdenom * prevmale)
    prevn <- round(prevdenom * prev)
})

crc_females <- inputs

crc_females$inc[1:15] <- 1e-08

save(inputs, file="MSLT/data/city regions/bristol/dismod/crc_females.rda")

View(crc_females)


### Checking differences to try to find out why crc_females is not working with CJ stan developments. 
class(crc_females)
class(ihdlondon)
typeof(crc_females)
typeof(ihdlondon)
length(crc_females)
length(ihdlondon)

## Belen: Difference in column increments (column 0 in data frame, does this have anything to do with it?). 
attributes(crc_females)
attributes(ihdlondon)

### Belen: I have not done anything with the below code

use_data(crc_females)

outputs <- read.csv("second/outputs.csv", skip=103, nrow=101)[,-1]
names(outputs) <- c("age","inc","prev","rem","mort","incout","prevout","remout","cfout","durout","mortout","rrout","ageonset")

## Dismod outputs from my run
## No smoothing, using inc and mort data only, no prev data. 
outmine <- read.csv("second/outputs_mine.csv", skip=4, nrows=21, header=FALSE)[,1:13]
names(outmine) <- c("agegrp", "inc","rem","mort","inc2","prev","rem2","cf","dur","mort","rrmort","rrmortn","ageonset")
outmine$agegrp <- seq(0, 100, by=5)
points(outmine$agegrp, outmine$cf, col="purple", pch=19)
