
### Disbayes (Chris Jackson Bayesian Dismod mode, https://chjackson.github.io/disbayes/vignette.html) (this should be in the mslt code)

### test with data base inputs prepared for crc females

# install.packages("rstan")

require(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## LOOP for all diseases? (data frames for all diseases generated already)
## Need to fix that I cannot read rds files
## test one at the time, but then do loop to generate all outcomes for case fatality

## Can loop the list here. 




input_disbayes <- data.frame(read.csv("MSLT/data/city regions/bristol/dismod/input/tblc_male.csv"))


datstan <- c(as.list(input_disbayes), nage=nrow(input_disbayes))
inits <- list(
  list(cf=rep(0.0101, datstan$nage)),
  list(cf=rep(0.0201, datstan$nage)),
  list(cf=rep(0.0056, datstan$nage)),
  list(cf=rep(0.0071, datstan$nage))
)
gbdcf <- stan("MSLT/disbayes-master/gbdcf-unsmoothed.stan", data=datstan, init=inits)

## Extract Summary statistics

gbd_cf_summ <- summary(gbdcf)$summary



#### Try loop
#### Check with Alan and Carl to loop the inputs list
#### need to save results that indentify disease and sex

disbayes_output_list <- list()
index <- 1

for (d_index in i_disease) {
  for (sex_index in i_sex){
    
    data <- disbayes_input_list[[index]]
    
      ## The saved data for input may be deleted or not, perhaps useful to check errors. 
      
      # data.frame(read.csv(paste0("MSLT/data/city regions/bristol/dismod/input/", d_index, "_", sex_index, ".csv")))
    
    
    datstan <- c(as.list(data), nage=nrow(data))
    inits <- list(
      list(cf=rep(0.0101, datstan$nage)),
      list(cf=rep(0.0201, datstan$nage)),
      list(cf=rep(0.0056, datstan$nage)),
      list(cf=rep(0.0071, datstan$nage))
    )
    gbdcf <- stan("MSLT/disbayes-master/gbdcf-unsmoothed.stan", data=datstan, init=inits)
    
    ## Extract Summary statistics
    
    disbayes_output_list[[index]] <- summary(gbdcf)$summary
    disbayes_output_list[[index]]["sex"] <- sex_index
    disbayes_output_list[[index]]["disease"] <- d_index

  index <- index + 1
  
  }
}
  
View(disbayes_output_list[[1]])

### Data preps Disbayes ready.

## Inputs from gbd_df (CHECK THIS WORKS WITH CHRIS): 
### incidence and mortality rates per one by age and sex.
### one-year population derived from dividing 5-year age groups by 5 (pop in the disbayes data input)
### ndiediss is the product of population number by the rate of mortlaity (5-year age rate?) Same for incidence per one rate?
### prevalence: prevdenom and prev (numberator). "The numerators and denominators are not provided in the source data, so we assume values for the 
### denominators, given in the variable prevdenom.The corresponding numberators prevn are then obtained by multiplying prevdenom by the provided
### prevalence estimates, and rounding to the nearest integer. 
### Inputs (i, r, n): 
#### Age: 1:100
#### inc (incidence): population by one year age group by incidence rate by 5-year age group (check with Chris)
#### cfout (case fatality): dm(disease specific mortality), counts of individuals alive at age a and the corresponding number who die before age a + 1
#### check whe



