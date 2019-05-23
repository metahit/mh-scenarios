# ---- This code is to organise the the data downloaded from GBD (http://ghdx.healthdata.org/gbd-results-tool) ----

# ---- GBD data ----
#  Format GBD raw data: 5-year age groups by sex for localities within city regions
#  Data needs for mslt-life table and mslt-disease life table 
##  Life table: population numbers, all-cause mortality rates and total YLDs rates
##  Disease life tables: disease ylds rates (calculated), incidence rates and case fatality rates
###   dismod or disbayes: population numbers and all-cause mortality rates (Dismod) and incidence and mortality numbers

# ---- City regions (We starts with Bristol) ----

# Sheffield City Region Combined Authority: Barnsley, Doncaster, Rotherham, Sheffield.
# North East Combined Authority: County Durham, Gateshead, Newcastle upon Tyne, North Tyneside, Northumberland, South Tyneside, Sunderland.
# Greater Manchester Combined Authority: Bolton, Bury, Manchester, Oldham, Rochdale, Salford, Stockport, Tameside, Trafford, Wigan.
# Liverpool City Region Combined Authority: Halton, Knowsley, Liverpool, St. Helens, Sefton, Wirral.
# West Yorkshire Combined Authority: Bradford, Calderdale, Kirklees, Leeds, Wakefield.
# Bristol: Bath and North East Somerset, City of Bristol, North Somerset, South Gloucestershire.
# Nottingham (Only GBD data for Nottingham): Ashfield, Bassetlaw, Broxtowe, Gedling, Mansfield, Nottingham, Newark and Sherwood, Rushcliffe.
# West Midlands Combined Authority: Birmingham, Coventry, Dudley, Sandwell, Solihull, Walsall, Wolverhampton.
# London: all London boroughs and the City of London.


### Disbayes (Chris Jackson Bayesian Dismod mode, https://chjackson.github.io/disbayes/vignette.html)

## My installation testing to get the build tools

install.packages("rstan")
# install.packages("C:/Rtools/mingw_64/bin", repos = NULL, type="source") not workin installing from source. 
pkgbuild::has_build_tools(debug = TRUE)
pkgbuild::find_rtools(debug = TRUE)

require(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## test ihd example london
datstan <- c(as.list(ihdlondon), nage=nrow(ihdlondon))
inits <- list(
  list(cf=rep(0.0101, datstan$nage)),
  list(cf=rep(0.0201, datstan$nage)),
  list(cf=rep(0.0056, datstan$nage)),
  list(cf=rep(0.0071, datstan$nage))
)
gbdcf <- stan("MSLT/disbayes-master/gbdcf-unsmoothed.stan", data=datstan, init=inits)

## Extract Summary statistics

gbd_cf_summ <- summary(gbdcf)$summary




### Try to get Rtools
install.packages("devtools")
install.packages("pkgbuild")
require(pkbuild)
require(devtools)
pkgbuild::has_build_tools(debug = TRUE)


# tl;dr: If you want to explicitly set up Rtools, you need to update both the PATH and BINPREF environment variables.

# NEED TO GET BUILDING TOOLS

# ---- Prepare data for Bristol City Region ---- (got data from 2007 to 2017 for trends, here only use 2017 data) 
# Baseline year data (2017)
# Get data ready for processing in mslt_code.r line 33 (need to do per locality and then add up to region, can add number 
# but  not rates, so population calculations need to be at localities level)

gbd_input <- read.csv(file="MSLT/data/city regions/bristol/gbd_data_bristol.csv")

gbd_input$location <- as.character(gbd_input$location) # to match with localities characters vector. 

localities <- c('Bristol, City of', 'Bath and North East Somerset', 'North Somerset', 'South Gloucestershire')

year <- 2017

### Loop to create a raw data set for 2017 for each of the localities to calculate population numbers

gbd_data_localities_raw <- list()
index <- 1

for (l in localities){
  for (y in year){
    
    gbd_data_localities_raw[[index]] <- sort_gbd_input(in_data = gbd_input, in_year = y, in_locality = l)
    
    index <- index + 1
  }
}

View(gbd_data_localities_raw[[1]]) 

### Calculate population numbers per locality in baseline year (2017), we need this to derive city region population

levels(gbd_input$cause) # check causes for below data frame

## Causes list is to make calculations

disease_short_names <- data.frame(disease = c("All causes", 
                                              "Alzheimer's disease and other dementias", 
                                              "Bladder cancer", 
                                              "Breast cancer", 
                                              "Chronic myeloid leukemia", 
                                              "Colon and rectum cancer", 
                                              "Kidney cancer", 
                                              "Prostate cancer", 
                                              "Diabetes mellitus type 2", 
                                              "Esophageal cancer", 
                                              "Hypertensive heart disease", 
                                              "Ischemic heart disease", 
                                              "Ischemic stroke", 
                                              "Liver cancer", 
                                              "Major depressive disorder", 
                                              "Malignant skin melanoma", 
                                              "Multiple myeloma", 
                                              "Parkinson's disease", 
                                              "Stomach cancer", 
                                              "Tracheal, bronchus, and lung cancer", 
                                              "Uterine cancer"),
                                  sname = c("ac", "adod", "blc", "bc", "cml", "crc", 
                                            "kc", "pc", "dmt2", "ec", "hhd", "ihd", 
                                            "is","lc","mdd", "msm", "mm", "pd", "sc", 
                                            "tblc", "uc"))

disease_short_names$disease <- as.character(disease_short_names$disease)
disease_short_names$sname <- as.character(disease_short_names$sname)
disease_short_names

levels(gbd_input$measure) # check measures for below data frame

disease_measures <- list("Prevalence", "Incidence", "Deaths", "YLDs (Years Lived with Disability)")

#### Prepare data set per locality to calculate population numbers using run_loc_df (I used lapply, may change for loop)

gbd_loc_data_processed <- lapply(gbd_data_localities_raw, run_loc_df)

## remove incidence and prevalence all cause (ideally this should be part of the above, using if)


View(gbd_loc_data_processed[[1]])

#### Add up all localities in a data frame for Bristol City Region: population, causes-measures rates and numbers. 

## add up number and then calculate rates from numbers and population numbers. 

gbd_Bristol_all_loc <- bind_rows(gbd_loc_data_processed, .id = 'number')

### Delete columns with rates (we will recalculate them at the city region level)

gbd_Bristol_all_loc <-  select(gbd_Bristol_all_loc,-contains("rate"))

### Crearte dataframe with added values (may delete this one, not necessary as goup by can create new dataframe)

gbd_Bristol_all_loc$age <- as.character(gbd_Bristol_all_loc$age)
gbd_Bristol_all_loc$sex <- as.character(gbd_Bristol_all_loc$sex)
gbd_Bristol_all_loc$sex_age_cat <- paste(gbd_Bristol_all_loc$sex, gbd_Bristol_all_loc$age, sep = "_")
gbd_Bristol_all_loc <- select(gbd_Bristol_all_loc, -c(age, sex, location, number, prevalence_number_ac, incidence_number_ac))

## Check dataset values in excel

write_csv(gbd_Bristol_all_loc, "MSLT/data/city regions/bristol/test/all_localities.csv")

### This data set has population and numbers data for all cause and diseases prevalence, incidence and mortlaity for
### the the Bristol city Region. THIS IS NOT WORKING, NOT ADDING OBSERVATIONS. USE AGGREGATE?

gbd_Bristol <- gbd_Bristol_all_loc %>%
  group_by(sex_age_cat) %>%
  summarise_all(funs(sum))



### Create two new columns for age and seX

gbd_Bristol_2017 <- gbd_Bristol %>%
  separate(sex_age_cat, c("sex", "age"), "_")


View(gbd_Bristol_2017)
View(gbd_Bristol_2017[,"ylds (years lived with disability)_number_lc"])
View(gbd_Bristol_2017[,"prevalence_number_lc"])

write_csv(gbd_Bristol_2017, "MSLT/data/city regions/bristol/test/gbd_Bristol_2017.csv")

# Trends data (2007 to 2017)