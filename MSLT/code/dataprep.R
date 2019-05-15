# ---- This code is to organise the the data downloaded from GBD (http://ghdx.healthdata.org/gbd-results-tool) ----

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

# ---- GBD data ----
#  Format GBD raw data: 5-year age groups by sex for localities within city regions
#  Data needs for mslt-life table and mslt-disease life table 
##  Life table: population numbers, all-cause mortality rates and total YLDs rates
##  Disease life tables: disease ylds rates (calculated), incidence rates and case fatality rates
###   dismod or disbayes: population numbers and all-cause mortality rates (Dismod) and incidence and mortality numbers

### Disbayes (Chris Jackson Bayesian Dismod mode, https://chjackson.github.io/disbayes/vignette.html)
install.packages("rstan")
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# ---- Prepare data for Bristol City Region ---- (got data from 2007 to 2017 for trends, here only use 2017 data) 
# Baseline year data (2017)
# Get data ready for processing in mslt_code.r line 33 (need to do per locality and then add up to region, can add number 
# but  not rates, so population calculations need to be at localities level)

gbd_input <- read.csv(file="MSLT/data/city regions/bristol/gbd_data_bristol.csv")


gbd_input$location <- as.character(gbd_input$location)


localities <- c('Bristol, City of', 'Bath and North East Somerset', 'North Somerset', 'South Gloucestershire')
year <- 2017 # use for trends calculations, we can go back to 1990
 
### Loop to create a data set for 2017 for each of the localities to calculate population numbers

gbd_data_localities <- list()
index <- 1


for (l in localities){
  for (y in year){
    
    gbd_data_localities[[index]] <- sort_gbd_input(in_data = gbd_input, in_year = y, in_locality = l)
    
    index <- index + 1
  }
}
 
# View(gbd_data_localities[[1]])

### Calculate population numbers per locality in baseline year (2017) and then add up for City Region

levels(gbd_input$cause) # check causes for below data frame

## Move to mslt code

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

levels(gbd_input$measure) # check measures for below data frame

disease_measures <- list("Prevalence", "Incidence", "Deaths", "YLDs (Years Lived with Disability)")

# Prepare data set per locality to calculate population numbers using run_loc_df

## I think issue is with data defintion in function



  

# Trends data (2007 to 2017)