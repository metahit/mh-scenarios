
### Disbayes (Chris Jackson Bayesian Dismod mode, https://chjackson.github.io/disbayes/vignette.html) (this should be in the mslt code)

### test with data base inputs prepared for crc females

# install.packages("rstan")

require(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## LOOP for all diseases? (data frames for all diseases generated already)
datstan <- c(as.list(crc_females), nage=nrow(crc_females))
inits <- list(
  list(cf=rep(0.0101, datstan$nage)),
  list(cf=rep(0.0201, datstan$nage)),
  list(cf=rep(0.0056, datstan$nage)),
  list(cf=rep(0.0071, datstan$nage))
)
gbdcf <- stan("MSLT/disbayes-master/gbdcf-unsmoothed.stan", data=datstan, init=inits)

## Extract Summary statistics

gbd_cf_summ <- summary(gbdcf)$summary

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



