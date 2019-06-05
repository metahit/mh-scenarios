## Adjustment of CJ script to generate data input for disbayes

library(devtools)

## Belen Comments: 
## The code below generates age, sex and disease specific data frames to process with disbayes. 
## Input data is generated with script in mslt_code.r (may be best to have all data prep in one script or all in mslt_code. r)

in_data <- read.csv("MSLT/data/city regions/bristol/dismod/input_data.csv")
disbayes_input_list <- list()
index <- 1

for (d_index in i_disease) {
  for (sex_index in i_sex){
    
    var_name <- paste0("rate_", d_index)

    disbayes_input_list[[index]] <- filter(in_data, sex == sex_index) %>% select(age, sex, ends_with(var_name), population_number)
    
    ## Add column to show disease
    
    disbayes_input_list[[index]]$disease <- d_index
    
    ## Change column names to match disbayes code
    
    colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== paste0("incidence_rate_", d_index)] <- "inc"
    colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== paste0("deaths_rate_", d_index)] <- "mort"
    colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== paste0("prevalence_rate_", d_index)] <- "prev"
    colnames(disbayes_input_list[[index]])[colnames(disbayes_input_list[[index]])== paste0("population_number")] <- "pop"
    
    ## We assume remission is 0 
    
    disbayes_input_list[[index]]$rem <- 0
    
    ## create denominator for disbayes code
    
    disbayes_input_list[[index]]$prevdenom <- c(100,100,500,500,500,500,500,500,500,500,500,500,500,500,500,500,200,200,100,100) / 10 # total sample size 3910, generous for London (from CJ)
    
    ## Added agegroups to derive age groups by 1
    
    disbayes_input_list[[index]]$agegrp <- as.integer(seq(0,95, by=5))
    
    ## Replace 0 with small numbers for incidence, otherwise, disbayes does not work.
    
    disbayes_input_list[[index]]$inc <- ifelse(disbayes_input_list[[index]]$inc  == 0, 1e-08, disbayes_input_list[[index]]$inc)
    

    ## Convert 5 year data file to 100 year age intervals    
    
    
    outage <- 0:100  # assume inc/prev/mort same in each year within a five-year age group
    ind <- findInterval(outage, disbayes_input_list[[index]]$agegr)
    disbayes_input_list[[index]] <- disbayes_input_list[[index]][ind,]
    disbayes_input_list[[index]]$age <- outage
    
    disbayes_input_list[[index]] <- within(disbayes_input_list[[index]], {
      ningrp <- rep(table(agegrp), table(agegrp))
      # popmale <- round(popmale/ningrp) ## assume population uniform between years within age group.
      pop <- round(pop/ningrp) ## assume population uniform between years within age group.
      # ndieddismale <- round(popmale * (1 - exp(-mortmale)))
      ndieddis <- round(pop * (1 - exp(-mort)))
      # prevnmale <- round(prevdenom * prevmale)
      prevn <- round(prevdenom * prev)
    })
    
    

    index <-  index +1
    
    
  }
}


## Loop to save each data frame within disbayes_list

index <- 1 

for (d_index in i_disease){
    for (sex_index in i_sex){
      
      ##Save to csv
      write_csv(disbayes_input_list[[index]], paste("MSLT/data/city regions/bristol/dismod/", d_index, "_", sex_index, ".csv"))
      
      # ##Save to rda
      # 
      save(disbayes_input_list, file = paste("MSLT/data/city regions/bristol/dismod/",d_index, sex_index,".rda"))
      
      index <- index +1
    }
}



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
