##### To do

# Add remission if we decide to use it for cancers
# Use disbayes to test dismod alternative, also mslt alternative? same principle. 
# move all parameters to the top of the code (e.g. disease life table generation)
# move disbayes to mslt.code
# move all parameters to the top of the table
# Include uncertainty parameters from disbayes estimates in model
# Prevalence estimates in the GBD assume remission after 10 years. If we do not assume remission, then we need to: 1) use only incidence and mortlaity
# for cancers estimates in Dismod; 2) Deflate DW by the ratio of Dismod estimated prevalence and GBD prevalence. 
# From GBD appendix: "Prevalence for all cancers is estimated for a maximum of ten years after incidence, as in GBD 2013-2016. Prevalence beyond the
# ten yera period is only estimated for permanent sequelae resulting from procedures. 
# GBD reference for data: James, S. L., et al. (2018). "Global, regional, and national incidence, prevalence, and years lived with disability for 354 diseases and injuries for 195 countries and territories, 1990&#x2013;2017: a systematic analysis for the Global Burden of Disease Study 2017." The Lancet 392(10159): 1789-1858.
# For the UK (GBD Ref): For the UK the analyses is by Local Administrative Area (level 6 in GBD hierarchy). 
# Do a set of Dismod outputs with remission for cancers to test the difference in results. 
# use yml to define variables to facilitate reuse of code. See Carls suggestions.config.yml
# Get all data from the GBD
getwd()
# Change to own wd

# ---- chunk-intro ----
require(dplyr)
require(tidyverse)
require(knitr)
require(kableExtra)
require(citr)
require(gridExtra)
require(cowplot)
require(ggpubr)
require(grid)
require(ggplot2)
require(gridExtra)
require(pillar)
require(devtools)

# rm (list = ls())
options(scipen=999)

# ---- chunk-1 ----

source("MSLT/code/functions.R")

## AVOID HARDCODING NAMES

# ---- chunk 2 ----

# ---------------------- Creating MSLT df ---------------------------

# Start with gbd dataframe generated in dataprep.R script

# ------------------- YLDs ---------------------------

all_ylds_df <- dplyr::select(gbd_df, starts_with("ylds (years lived with disability)_number"))


## Adjust all cause ylds for included diseases and injuries (exclude all cause )

gbd_df[["allc_ylds_adj_rate_1"]] <- (gbd_df$`ylds (years lived with disability)_number_allc`  - rowSums(select(all_ylds_df, -`ylds (years lived with disability)_number_allc`))) / 
  gbd_df$population_number

# ------------------- DWs ---------------------------#

disease_short_names <- mutate_all(disease_short_names, funs(tolower))

for (i in 1:nrow(disease_short_names)){
  gbd_df[[paste0("dw_adj_", disease_short_names$sname[i])]] <- 
    (gbd_df[[paste0("ylds (years lived with disability)_number_", disease_short_names$sname[i])]] /
       gbd_df[[paste0("prevalence_number_", disease_short_names$sname[i])]]) /
    ( 1 - gbd_df[["allc_ylds_adj_rate_1"]])
}

# Check that dws were created
# names(gbd_df)
# View(gbd_df)

# ------------------- Replace Nan and Inf numbers -------------------- #

gbd_df[mapply(is.infinite, gbd_df)] <- 0
gbd_df <- replace(gbd_df, is.na(gbd_df), 0)

# ------------------- MSLT frame --------------------------- #

mslt_df <- data.frame(age = rep(c(0:100), 2), sex = append(rep("male", 101), 
                                                           rep("female", 101)))

# ------------------- Add population numbers --------------------------- #

## Model in five-year age cohorts, simulated from mid-age in cohort

mslt_df$age_cat [mslt_df$age == 2] <- 2
mslt_df$age_cat [mslt_df$age == 7] <- 7
mslt_df$age_cat [mslt_df$age == 12] <- 12
mslt_df$age_cat [mslt_df$age == 17] <- 17
mslt_df$age_cat [mslt_df$age == 22] <- 22
mslt_df$age_cat [mslt_df$age == 27] <- 27
mslt_df$age_cat [mslt_df$age == 32] <- 32
mslt_df$age_cat [mslt_df$age == 37] <- 37
mslt_df$age_cat [mslt_df$age == 42] <- 42
mslt_df$age_cat [mslt_df$age == 47] <- 47
mslt_df$age_cat [mslt_df$age == 52] <- 52
mslt_df$age_cat [mslt_df$age == 57] <- 57
mslt_df$age_cat [mslt_df$age == 62] <- 62
mslt_df$age_cat [mslt_df$age == 67] <- 67
mslt_df$age_cat [mslt_df$age == 72] <- 72
mslt_df$age_cat [mslt_df$age == 77] <- 77
mslt_df$age_cat [mslt_df$age == 82] <- 82
mslt_df$age_cat [mslt_df$age == 87] <- 87
mslt_df$age_cat [mslt_df$age == 92] <- 92
mslt_df$age_cat [mslt_df$age == 97] <- 97

mslt_df$sex_age_cat <- paste(mslt_df$sex,mslt_df$age, sep = "_"  )

gbd_popn_df <- select(gbd_df, population_number, sex_age_cat)

mslt_df <- left_join(mslt_df, gbd_popn_df, by = "sex_age_cat")

#### Interpolate rates

mslt_df[["mx"]] <- NA
mslt_df[["pyld_rate"]] <- NA


# View(mslt_df)

### Interpolate all-cause mortality rate from 5-yr rates to 1-yr rates

for(sex_index in i_sex) {
  # sex_index <- "female"
  # measure_index <- "deaths_rate_allc"
  
  data <- filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, deaths_rate_allc)
  x <- data$age_cat
  y <- log(data$deaths_rate_allc)
  
  interpolation_func <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
  
  interpolated <- as.data.frame(interpolation_func(seq(0, 100, 1)))
  age <- seq(0, 100, by = 1)
  interpolated <- cbind(interpolated, age)
  interpolated[,1] <- exp(interpolated[,1])
  ## Add column with sex to create age_sex category to then merge with input_life table
  interpolated$sex <- paste(sex_index)
  interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
  ## Change name of column death to mx and ylds to pyld_rate to then merge
  ## with input_life table
  
  colnames(interpolated)[1] <- paste("mx")
  
  mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat 
          & mslt_df$sex == sex_index, ]$mx <- interpolated$mx

### Interpolate all-cause ylds rate from 5-yr rates to 1-yr rates

  
  data <- filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, `ylds (years lived with disability)_rate_allc`)
  x <- data$age_cat
  y <- log(data$`ylds (years lived with disability)_rate_allc`)
  
  interpolation_func <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
  
  interpolated <- as.data.frame(interpolation_func(seq(0, 100, 1)))
  age <- seq(0, 100, by = 1)
  interpolated <- cbind(interpolated, age)
  interpolated[,1] <- exp(interpolated[,1])
  ## Add column with sex to create age_sex category to then merge with input_life table
  interpolated$sex <- paste(sex_index)
  interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
  ## Change name of column death to mx and ylds to pyld_rate to then merge
  ## with input_life table
  
  colnames(interpolated)[1] <- paste("pyld_rate")
  
  mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat 
          & mslt_df$sex == sex_index, ]$pyld_rate <- interpolated$pyld_rate

}

# names(gbd_df)

### Interpolate road injuries deaths rate from 5-yr rates to 1-yr rates

mslt_df[["pyld_rate_rdin"]] <- NA
mslt_df[["deaths_rate_rdin"]] <- NA

for(sex_index in i_sex) {
  # sex_index <- "female"
  # measure_index <- "deaths_rate_ri"
  
  data <- filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, deaths_rate_rdin)
  x <- data$age_cat
  y <- log(data$deaths_rate_rdin)
  
  interpolation_func <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
  
  interpolated <- as.data.frame(interpolation_func(seq(0, 100, 1)))
  age <- seq(0, 100, by = 1)
  interpolated <- cbind(interpolated, age)
  interpolated[,1] <- exp(interpolated[,1])
  ## Add column with sex to create age_sex category to then merge with input_life table
  interpolated$sex <- paste(sex_index)
  interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")

  colnames(interpolated)[1] <- paste("deaths_rate_rdin")
  
  mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat 
          & mslt_df$sex == sex_index, ]$deaths_rate_rdin <- interpolated$deaths_rate_rdin

### Interpolate road injuries ylds rate from 5-yr rates to 1-yr rates

# for(sex_index in i_sex) {
#   # sex_index <- "female"
#   # measure_index <- "pyld_rate_ri"
  
  data <- filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, `ylds (years lived with disability)_rate_rdin`)
  x <- data$age_cat
  y <- log(data$`ylds (years lived with disability)_rate_rdin`)
  
  interpolation_func <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
  
  interpolated <- as.data.frame(interpolation_func(seq(0, 100, 1)))
  age <- seq(0, 100, by = 1)
  interpolated <- cbind(interpolated, age)
  interpolated[,1] <- exp(interpolated[,1])
  ## Add column with sex to create age_sex category to then merge with input_life table
  interpolated$sex <- paste(sex_index)
  interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
  
  colnames(interpolated)[1] <- paste("pyld_rate_rdin")
  
  mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat 
          & mslt_df$sex == sex_index, ]$pyld_rate_rdin <- interpolated$pyld_rate_rdin
}


### Interpolate dws rate from 5-yr rates to 1-yr rates

for (i in 1:nrow(disease_short_names)){
  
  var_name <- paste0("dw_adj_", disease_short_names$sname[i])

  mslt_df[, var_name] <- 1

}

for (i in 1:nrow(disease_short_names)){
  for(sex_index in i_sex) {
    
    # i <- 2
    # sex_index <- "female"
    var_name <- paste0("dw_adj_", disease_short_names$sname[i])
    
    data <- filter(gbd_df, sex == sex_index) %>% select(age, sex, age_cat, starts_with(var_name))

    x <- data$age_cat
    y <- log(data[[var_name]])
    
    interpolation_func <- stats::splinefun(x, y, method = "monoH.FC", ties = mean)
    
    interpolated <- as.data.frame(interpolation_func(seq(0, 100, 1)))
    age <- seq(0, 100, by = 1)
    interpolated <- cbind(interpolated, age)
    interpolated[,1] <- exp(interpolated[,1])
    ## Add column with sex to create age_sex category to then merge with input_life table
    interpolated$sex <- paste(sex_index)
    interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
    ## Change name of column death to mx and ylds to pyld_rate to then merge
    ## with input_life table
    colnames(interpolated)[1] <- var_name

    
    mslt_df[mslt_df$sex_age_cat == interpolated$sex_age_cat 
            & mslt_df$sex == sex_index, ][[var_name]] <- interpolated[[var_name]]
    
  }
}


## Uncomment to check df
# View(mslt_df)

# names(mslt_df)
# ------------------- Replace Nan and Inf numbers  -------------------- #

mslt_df[mapply(is.infinite, mslt_df)] <- 0
mslt_df <- replace(mslt_df, is.na(mslt_df), 0)

# --------------------add incidence and case fatality ----------------- #


# ---- chunk-6 ---- CODE TO PICK UP DIRECTLY FROM dismod OUTPUT EXCEL, temporarly I copied and paste. Discuss with Alan and Carl

## Use dismod output and add to mslt_df (UPDATE)
## using Overall English data only for ihd, is, dm, bc and cc to show how it works to Carl and Alan

idata <- disease_life_table_input

# # Add age_sex category to match with mslt_df
# 
# idata$sex_age_cat <- paste(idata$sex,idata$age, sep = "_"  )

idata <- select(idata, -c(age, sex))

mslt_df <- left_join(mslt_df, idata, by = "sex_age_cat")

# View(mslt_df)

# ---- chunk-7 ----

# Create baseline life tables

general_life_table_list_bl <- list()

index <- 1

for (age in i_age_cohort){
  for (sex in i_sex){
    # cat("age ", age, " and sex ", sex, "\n") #Uncomment to see index
    general_life_table_list_bl[[index]] <- RunLifeTable(in_idata = mslt_df,
                                                          in_sex = sex, in_mid_age = age)
    index <- index + 1
  }
}

## Uncommnet to check life table list
View(general_life_table_list_bl[[2]])

# ---- chunk-8 ----

## Run disease life (here issue with diseases not generated in disbayes)

## WE HAVE TO DO THIS FOR NOW, AS DISBAYES IS NOT GENERATING OUTPUTS FOR ALL DISEASES, SO THE DISEASE_SHORT_NAMES VARIABLE IS NOT SUITABLE
i_disease <- c(tolower(unique(diseaseMeasures$disease)))
i_disease

## ALSO NEED TO DROP COLUMNS IN DATA FRAME FOR VARIABLES NOT INCLUDED IN DISEASE INPUT TABLE. USE idata
  
## Develop a code to just keep DW included in i_disease

mslt_df <- select(mslt_df, -c("dw_adj_rdin", "dw_adj_lwri", "dw_adj_allc", "dw_adj_adaod", "dw_adj_dmt2", "dw_adj_tbalc", "dw_adj_brsc", "dw_adj_utrc"))

idata <- select(idata, -c(age, sex))

disease_life_table_list_bl <- list()
index <- 1

for (age in i_age_cohort){
  for (sex in i_sex){
    for (disease in i_disease){
      
      # Exclude bc for Males
      # if (sex == "male" && disease == "bc"){
      #   # cat("\n") #Uncomment to see list
      # # }
      # # if (sex == "male" && disease == "uc"){
      # #   # cat("\n") #Uncomment to see list
      # # }
      # # if (sex == "female" && disease == "pc"){
      # #   # cat("\n") #Uncomment to see list
      # }
      # else {
        # cat("age ", age, " sex ", sex, "and disease", disease, "\n") #Uncomment to see list
        disease_life_table_list_bl[[index]] <- RunDisease(in_idata = mslt_df, in_sex = sex, in_mid_age = age, in_disease = disease)
        index <- index + 1
      
    }
  }
}
## Uncommnet to check disease life table list
# View(disease_life_table_list_bl[[224]])


# ---- chunk-9 ----

## Create value to use as factor changing incidence rates. REPLACE with (1-PIF) and use multiplicative PIF for common disease risk factors. 

## Rob's comment about PIFs calcs: see function "health burden" in metahit_functions.R

incidence_change <- 0.95

## Generate scenario incidence (for each disease)

incidence_sc <- list()
index <- 1

for (age in i_age_cohort){
  for (sex in i_sex){
    for (disease in i_disease) {
      
      # # Exclude bc for Males
      # if (sex == "male" && disease == "bc"){
      #   # cat("\n") # Uncomment to see list
      # }
      # else {
        
        incidence_sc[[index]] <- disease_life_table_list_bl[[index]]$incidence_disease *
          incidence_change
        index <- index + 1
        
      # }
    }
  }
}

## Uncommnet to check scenario incidence
# View(incidence_sc[[1]])


# ---- chunk-10 ----

disease_life_table_list_sc <- list()
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    for (disease in i_disease) {
      # # Exclude bc for Males
      # if (sex == "male" && disease == "bc"){
      #   # cat("\n") #Uncomment to see list
      # }
      # if (sex == "male" && disease == "uc"){
      #   # cat("\n") #Uncomment to see list
      # }
      # if (sex == "female" && disease == "pc"){
      #   # cat("\n") #Uncomment to see list
      # }
      # else {
        # cat("age ", age, " sex ", sex, "and disease", disease, "\n")
        # modify mslt_df's incidence for the said scenario
        td1 <- mslt_df
        td1[td1$age >= age & td1$sex == sex,][[paste("incidence", disease, sep = "_")]] <- incidence_sc[[index]]
        
        # Instead of idata, feed td to run scenarios
        disease_life_table_list_sc[[index]] <- RunDisease(in_idata = td1, in_sex = sex,
                                                           in_mid_age = age, in_disease = disease)
        disease_life_table_list_sc[[index]]$diff_inc_disease <-
          disease_life_table_list_sc[[index]]$incidence_disease -   disease_life_table_list_bl[[index]]$incidence_disease
        disease_life_table_list_sc[[index]]$diff_prev_disease <-
          disease_life_table_list_sc[[index]]$px  - disease_life_table_list_bl[[index]]$px
        disease_life_table_list_sc[[index]]$diff_mort_disease <-
          disease_life_table_list_sc[[index]]$mx - disease_life_table_list_bl[[index]]$mx
        disease_life_table_list_sc[[index]]$diff_pylds_disease <-
          (disease_life_table_list_sc[[index]]$px - disease_life_table_list_bl[[index]]$px) * disease_life_table_list_bl[[index]]$dw_disease
        index <- index + 1
      # }
    }
  }
}
## Uncommnet to check scenario life tables
# View(disease_life_table_list_sc[[3]])

# ---- chunk-11 ----

## Generate total change in mortality rate

## Sum mortality rate change scenarios (mx_sc_total) (ONLY DOING MALES)

mx_sc_total <- list()
l_index <- 1
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    mortality_sum <- NULL
    create_new <- T
    
    for (disease in i_disease) {
      if (sex == "male" && disease == "bc"){
        # cat("\n")
      }else{
        
        if (create_new){
          mortality_sum <- select(disease_life_table_list_sc[[index]],
                                  c('age', 'sex'))
          mortality_sum$total <- 0
          create_new <- F
          mortality_sum$total <- mortality_sum$total +
            (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }else{
          mortality_sum$total <- mortality_sum$total +
            (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
    }
    mx_sc_total[[l_index]] <- mortality_sum
    
    l_index <- l_index + 1
  }
}

## Uncommnet to check sceanrio mortality and changes
# View(mx_sc_total[[16]])

## Generate total change in prevalent yld rates
## Total ylds rate= sum (change prevalence disease*dw)

pylds_sc_total <- list()
l_index <- 1
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    pylds_sum <- NULL
    create_new <- T
    
    for (disease in i_disease) {
      if (sex == "male" && disease == "bc"){
        # cat("\n") #Uncomment to see list
      }
      if (sex == "male" && disease == "uc"){
        # cat("\n") #Uncomment to see list
      }
      if (sex == "female" && disease == "pc"){
        # cat("\n") #Uncomment to see list
      }
      else {
        
        if (create_new){
          pylds_sum <- select(disease_life_table_list_sc[[index]], c('age', 'sex'))
          pylds_sum$total <- 0
          create_new <- F
          pylds_sum$total <- pylds_sum$total +
            (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }else{
          pylds_sum$total <- pylds_sum$total +
            (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    pylds_sc_total[[l_index]] <- pylds_sum
    l_index <- l_index + 1
  }
}

## Uncommnet to check scenario pyld change
# View(pylds_sc_total[[2]])

## Calculate general life tables with modified mortality and pylds total
## Original mortality rate is modified by the mx_sc_total (total change in mortality from diseases)
## Original pyld rate is modified by the change in each disease pylds



general_life_table_list_sc <- list()
index <- 1


for (age in i_age_cohort){
  for (sex in i_sex){
    
    
    # cat("age ", age, " and sex ", sex, "\n")
    # modify idata's mortality and pyld total for the said scenario
    td2 <- mslt_df
    # td2 <- subset(td2, select = -c(mx, pyld_rate))
    td2[td2$age >= age & td2$sex == sex,][[paste("mx")]] <- general_life_table_list_bl[[index]]$mx + mx_sc_total[[index]]$total
    td2[td2$age >= age & td2$sex == sex,][[paste("pyld_rate")]] <- general_life_table_list_bl[[index]]$pyld_rate + pylds_sc_total[[index]]$total
    
    
    # Instead of idata, feed td to run scenarios
    general_life_table_list_sc[[index]] <- RunLifeTable(in_idata = td2, in_sex = sex, in_mid_age = age)
    #
    
    
    index <- index + 1
  }
}
## Uncommnet to check scenario life tables
# View(general_life_table_list_sc[[32]])
# View(general_life_table_list_bl[[1]])

## Check difference life table baseline and scenario (uncomment to check)
# general_life_table_list_bl[[1]]$Lx - general_life_table_list_sc[[1]]$Lx
# general_life_table_list_bl[[1]]$Lwx - general_life_table_list_sc[[1]]$Lwx




# ---- chunk-12 ----

## In the following list "output_life_table", 32 data frames are nested per age and sex cohort

output_burden <- list()
l_index <- 1
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    
    # Males do not have breast cancer, that is why we need the if/else.
    # We create a TRUE/FALSE variable for the loop to move into the next disease
    
    create_new <- T
    for (disease in i_disease) {
      if (sex == "male" && disease == "bc"){
        # cat("\n") #Uncomment to see list
      }
      if (sex == "male" && disease == "uc"){
        # cat("\n") #Uncomment to see list
      }
      if (sex == "female" && disease == "pc"){
        # cat("\n") #Uncomment to see list
      }
      else {
        
        if (create_new){
          output_burden_sc <- select(disease_life_table_list_sc[[index]],
                                     c('age', 'sex', 'incidence_disease', 'mx', 'px'))
          names(output_burden_sc)[names(output_burden_sc) == 'incidence_disease'] <-
            paste('incidence_disease', disease, "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'mx'] <-
            paste('mx', disease, "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'px'] <-
            paste('px', disease, "sc", sep = "_")
          output_burden_bl <- select(disease_life_table_list_bl[[index]],
                                     c('incidence_disease', 'mx', 'px'))
          names(output_burden_bl)[names(output_burden_bl) == 'incidence_disease'] <-
            paste('incidence_disease', disease, "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'mx'] <-
            paste('mx', disease, "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'px'] <-
            paste('px', disease, "bl", sep = "_")
          
          ## New list to add calculations for changes in burden of disease (incidence and mortality numbers)
          
          output_burden_change <- list()
          
          output_burden_change$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease *
            (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease *
            (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$inc_num_diff <- (disease_life_table_list_sc[[index]]$incidence_disease *
                                                  (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px)
                                                                                                                                              * general_life_table_list_bl[[l_index]]$Lx)
          
          output_burden_change$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$mx_num_diff <- (disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx)
          
          names(output_burden_change)[names(output_burden_change) == 'inc_num_bl'] <-
            paste('inc_num_bl', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_sc'] <-
            paste('inc_num_sc', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_diff'] <-
            paste('inc_num_diff', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_bl'] <-
            paste('mx_num_bl', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_sc'] <-
            paste('mx_num_sc', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_diff'] <-
            paste('mx_num_diff', disease, sep = "_")
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, output_burden_bl)
          output_burden_sc <- cbind(output_burden_sc, output_burden_change)
          
          create_new <- F
          
          ## Here the calculations above are repeated, here is where the F is telling to move into the next disease
          
        }else{
          
          td3 <- select(disease_life_table_list_sc[[index]],
                        c('incidence_disease', 'mx', 'px'))
          names(td3)[names(td3) == 'incidence_disease'] <-
            paste('incidence_disease', disease, "sc", sep = "_")
          names(td3)[names(td3) == 'mx'] <-
            paste('mx', disease, "sc", sep = "_")
          names(td3)[names(td3) == 'px'] <-
            paste('px', disease, "sc", sep = "_")
          
          td4 <- select(disease_life_table_list_bl[[index]],
                        c('incidence_disease', 'mx', 'px'))
          names(td4)[names(td4) == 'incidence_disease'] <-
            paste('incidence_disease', disease, "bl", sep = "_")
          names(td4)[names(td4) == 'mx'] <-
            paste('mx', disease, "bl", sep = "_")
          names(td4)[names(td4) == 'px'] <-
            paste('px', disease, "bl", sep = "_")
          
          output_burden_change2 <- list()
          
          output_burden_change2$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$inc_num_diff <- (disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx)
          
          output_burden_change2$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$mx_num_diff <- (disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx)
          
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_bl'] <-
            paste('inc_num_bl', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_sc'] <-
            paste('inc_num_sc', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_diff'] <-
            paste('inc_num_diff', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_bl'] <-
            paste('mx_num_bl', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_sc'] <-
            paste('mx_num_sc', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_diff'] <-
            paste('mx_num_diff', disease, sep = "_")
          
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, td3)
          output_burden_sc <- cbind(output_burden_sc, td4)
          output_burden_sc$age_cohort <- age
          output_burden_sc <- cbind(output_burden_sc, output_burden_change2)
          
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    
    ## general_life_table_list_sc and general_life_table_list_bl (Lx)
    output_burden_lf_sc <- select(general_life_table_list_sc[[l_index]], c('Lx', 'Lwx'))
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lx'] <- paste('Lx', "sc", sep = "_")
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lwx'] <- paste('Lwx', "sc", sep = "_")
    
    output_burden_lf_bl <- select(general_life_table_list_bl[[l_index]], c('Lx', 'Lwx'))
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lx'] <- paste('Lx', "bl", sep = "_")
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lwx'] <- paste('Lwx', "bl", sep = "_")
    
    
    output_burden_lf_sc$Lx_diff <- general_life_table_list_bl[[l_index]]$Lx - general_life_table_list_sc[[l_index]]$Lx
    output_burden_lf_sc$Lwx_diff <- general_life_table_list_bl[[l_index]]$Lwx - general_life_table_list_sc[[l_index]]$Lwx
    
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_sc)
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_bl)
    
    
    output_burden[[l_index]] <- output_burden_sc
    l_index <- l_index + 1
    
  }
}

## Uncomment to check

# View(output_burden[[3]])


# ---- chunk-13 ----

#####Generate a data frame for all results and create function to get outcomes.

output_df <- plyr::ldply(output_burden, rbind)

# View(output_df)


# ---- chunk- 14 ----  TO DO, presentaiton of results. 

# Generate graphs change in mortality and incidence numbers by age/sex. (NEED TO ADJUST THIS CODE TO MORE DISEASES)
# The code is only set up for the 5 original PA related diseases. 

output_dir = "MSLT/output"

# i_age_cohort <- c(22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)
# i_sex <- c("male", "female")
i_measure <- c("deaths", "ylds") #" (years lived with disability)")
i_outcome <- c("mx", "inc")
output_dir <- "MSLT/output"


i_outcome <- c("mx", "inc")
p_list_male <- list()
p_list_female <- list()
male_index <- 1
female_index <- 1
for (age in i_age_cohort){
  for (sex in i_sex) {
    for (outcome in i_outcome) {
      for (disease in i_disease){
        
        if (sex == "male" && disease == "bc"){
          # cat("\n") #Uncomment to see list
        }
        if (sex == "male" && disease == "uc"){
          # cat("\n") #Uncomment to see list
        }
        if (sex == "female" && disease == "pc"){
          # cat("\n") #Uncomment to see list
        }
        else {
          
          p_index  <- PlotOutput(in_data = output_df, in_age = age, in_population = sex, in_outcomes = c("age", paste(outcome, "num", "bl", disease, sep = "_"), paste(outcome, "num", "sc", disease, sep = "_"), paste(outcome, "num", "diff", disease, sep = "_")), in_disease = GetQualifiedDiseaseName(disease))
          
          if (sex == "male"){
            
            p_list_male[[male_index]] <- p_index
            
            if (male_index %% 4 == 0 && male_index > 0){
              
              p1 <- p_list_male[[male_index - 3]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p2 <- p_list_male[[male_index - 2]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p3 <- p_list_male[[male_index - 1]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p4 <- p_index + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              
              jpeg(paste0(output_dir, "/", paste(age, sex, outcome, sep="_"), ".jpeg"))
              GridArrangSharedLegend (p1, p2, p3, p4, ncol = 2, nrow = 2, mainTitle = paste(ifelse(outcome == "mx", "Deaths", "Incidence"), sex, "cohort mid age", age),
                                          mainLeft = 'Cases', mainBottom = 'Age')
              dev.off()
              
            }
            
            male_index <- male_index + 1
            
          }
          
          if (sex == "female" && female_index > 0){
            p_list_female[[female_index]] <- p_index
            
            if (female_index %% 5 == 0){
              p1 <- p_list_female[[female_index - 4]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p2 <- p_list_female[[female_index - 3]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p3 <- p_list_female[[female_index - 2]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p4 <- p_list_female[[female_index - 1]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p5 <- p_index + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              
              jpeg(paste0(output_dir, "/", paste(age, sex, outcome, sep="_"), ".jpeg"))
              GridArrangSharedLegend (p1, p2, p3, p4, p5, ncol = 2, nrow = 3, mainTitle = paste(ifelse(outcome == "mx", "Deaths", "Incidence"), sex, "cohort mid age", age), mainLeft = 'Cases', mainBottom = 'Age')
              dev.off()
              
            }
            female_index <- female_index + 1
          }
          
        }
      }
    }
  }
}





## Loop to include graphs in the document(CHECK WHAT IS THE USE OF THIS CODE)

# graphs_doc <- list()
# index <- 1
# for (age in i.age.cohort) {
#   for (sex in i.sex)  {
#     for (outcome in i_outcome) {
# 
#       graphs_doc [[index]] <- c(paste(output_dir, "/",age,"_",sex,"_", outcome,".jpeg", sep = ""))
#       knitr::include_graphics(graphs_doc [[index]])
# 
#       index <- index + 1
#     }
#   }
# }






# ---- chunk-15 ----

aggregate_frame_males <- list()
aggregate_frame_females <- list()

index <- 1

for (outcome in i_outcome) {
  for (disease in i_disease) {
    
    aggregate_frame_males[[index]] <- GenAggregate(in_data = output_df, in_cohorts = 16, in_population = "male", in_outcomes = c(paste(outcome, "num", "bl", disease, sep = "_"), paste(outcome, "num", "sc", disease, sep = "_"), paste(outcome, "num", "diff", disease, sep = "_")))
    
    aggregate_frame_females[[index]] <- GenAggregate(in_data = output_df, in_cohorts = 16, in_population = "female", in_outcomes = c(paste(outcome, "num", "bl", disease, sep = "_"), paste(outcome, "num", "sc", disease, sep = "_"), paste(outcome, "num", "diff", disease, sep = "_")))
    
    # Remove non-numeric columns starting with age and sex
    aggregate_frame_males[[index]] <- aggregate_frame_males[[index]] %>% select(-starts_with("age"), -starts_with("sex"))
    
    aggregate_frame_females[[index]] <- aggregate_frame_females[[index]] %>% select(-starts_with("age"), -starts_with("sex"))
    
    index <- index + 1
  }
}


## Loop for life years (Lx) and health adjusted life years (Lwx) to then add to total aggregated data.

## RUN ANOTHER LOOP FOR LX AND LWX

i_outcome2 <- c("Lx", "Lwx")

aggregate_frame_males2 <- list()
aggregate_frame_females2 <- list()

for (i in i_outcome2){
  
  aggregate_frame_males2[[i]] <- GenAggregate(in_data = output_df, in_cohorts = 16, in_population = "male", in_outcomes = c(paste(i, "bl", sep = "_"), paste(i, "sc", sep = "_"), paste(i, "diff",sep = "_")))
  
  aggregate_frame_females2[[i]] <- GenAggregate(in_data = output_df, in_cohorts = 16, in_population = "female", in_outcomes = c(paste(i, "bl", sep = "_"), paste(i,  "sc", sep = "_"), paste(i, "diff",sep = "_")))
  
  
  aggregate_frame_males2[[i]] <- aggregate_frame_males2[[i]] %>% select(-starts_with("age"), -starts_with("sex"))
  
  aggregate_frame_females2[[i]] <- aggregate_frame_females2[[i]] %>% select(-starts_with("age"), -starts_with("sex"))
  
}

## Uncomment to check data frames (list correspond to a disease and outcome combination, for example,
## incidence breast cancer)

# View(aggregate_frame_females[[2]])

## Transform list to data frame (this is not working after the first disease)

aggregate_frame_females <- do.call(cbind, aggregate_frame_females)
aggregate_frame_males <- do.call(cbind, aggregate_frame_males)
aggregate_frame_females2 <- do.call(cbind, aggregate_frame_females2)
aggregate_frame_males2 <- do.call(cbind, aggregate_frame_males2)

View(aggregate_frame_females2)

## Drop ending of variables names _males/_females to add up all outcomes in next step.

names(aggregate_frame_females) = gsub(pattern = "_female", replacement = "", x = names(aggregate_frame_females))

names(aggregate_frame_males) = gsub(pattern = "_male", replacement = "", x = names(aggregate_frame_males))

names(aggregate_frame_females2) = gsub(pattern = "_female", replacement = "", x = names(aggregate_frame_females2))

names(aggregate_frame_males2) = gsub(pattern = "_male", replacement = "", x = names(aggregate_frame_males2))


## Uncomment to check that the code is dropping the male/female ending.
# View(aggregate_frame_males)

## Create a copy of aggregate_frame_females.
total_aggr1 <- aggregate_frame_females
## Add aggregate_frame_males values to it
for (i in 1:ncol(aggregate_frame_females)){
  total_aggr1[i] <- total_aggr1[i] + aggregate_frame_males[i]
}

## Add data frames 2 with life years (check adds Lx and Lwx at the beginning of the variable name)
total_aggr2 <- aggregate_frame_females2
## Add aggregate_frame_males values to it
for (i in 1:ncol(aggregate_frame_females2)){
  total_aggr2[i] <- total_aggr2[i] + aggregate_frame_males2[i]
}

## Combine data frames

total_aggr <- cbind.data.frame(total_aggr1, total_aggr2)

total_aggr$sim_year <- seq.int(nrow(total_aggr))


## Uncomment to see total data frame
View(total_aggr)

## Test that total_aggr is adding males and females dataframes
#
# (aggregate_frame_females2$Lx.Lx_bl_22 + aggregate_frame_males2$Lx.Lx_bl_22) -total_aggr$Lx.Lx_bl_22




# ---- chunk-16 ----


####This plot has to be customised to in_outcomes, here, only totals shown, but specifications are up to the user. ADD LOOP for all outcomes over time and total TABLE.

####[] is used here to indicate the number of simulation years into the future.
####Disease outcomes has to be changed to the outcome of interest

#### Test code with loops for aggregated outcomes diseases burden. NOT WORKING.

### Compare with loops for age and sex cohort outcomes.

p_aggr_list <- list()
index <- 1

for (outcome in i_outcome) {
  for (disease in i_disease) {
    # outcome <- i_outcome[1]
    # disease <- i_disease[1]
    
    p_aggr_list_index <- ggplot(total_aggr[1:79,], aes(x = total_aggr[["sim_year"]])) +
      
      geom_line(mapping = aes(y = total_aggr[[paste("total", outcome, "num_bl", disease, sep = "_")]], colour = paste("total", outcome, "num_bl", disease, sep = "_"))) +
      theme_classic() +
      geom_hline(yintercept=0, linetype="dashed", color = "black") +
      geom_line(mapping = aes(y = total_aggr[[paste("total", outcome, "num_sc", disease, sep = "_")]], colour = paste("total", outcome, "num_sc", disease, sep = "_"))) +
      geom_line(mapping = aes(y = total_aggr[[paste("total", outcome, "num_diff", disease, sep = "_")]], colour = paste("total", outcome, "num_diff", disease, sep = "_"))) +
      xlab ("Simulation years") + ylab ("Cases") + labs (title = paste(disease, outcome)) +
      theme(plot.title = element_text(hjust = 0.5, size = 12)) +
      scale_color_discrete(name = paste(""), labels = c("Baseline", "Difference", "Scenario")) +
      theme(plot.title = element_text(hjust = 0.5))
    p_aggr_list[[index]] <- p_aggr_list_index
    index <- index + 1
    
    
  }
}

index <- 1

interpolation_index <- 1
for (outcome in i_outcome) {
  for (disease in i_disease) {
    file_name = paste("output/graphs", "Aggregated Outcomes", outcome, disease, ".jpeg", sep=" ")
    jpeg(file_name)
    print(p_aggr_list[[index]])
    index <- index + 1
    dev.off()
  }
}


p_aggregated <- do.call(marrangeGrob, list(grobs=p_aggr_list, nrow = 2, ncol = 2))
p_aggregated






