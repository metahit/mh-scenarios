# SET UP
rm(list = ls())
library(foreign)
library(car)
library(sqldf)
library(dplyr)
memory.limit(size=1000000)

## INPUT DATA AND PARAMETERS

# Load data
sp <-read.dta("1_InputData/1_LA14trip-level/SPtrip_CensusNTSAPS_E06000001temp.dta")
distspeed <- read.csv("1_InputData/2_OtherInput/NTS_distance_speed.csv")

# Scenario target % cycle
targetpcycle <- 0.15

# MET values
met_cycle <- 5.63
met_walk <- 3.53


## STEP 1: IDENTIFY NEW CYCLISTS

# Baseline probability near market [coefficients from a logit model of all adults in England]
sp$logpcyclist_scen <- (-0.944 * sp$female) + (-0.061 * (sp$agecat==2)) + (-0.032 * (sp$agecat==3)) + (-0.340 * (sp$agecat==4)) + 
  (-0.879 * (sp$agecat==5)) + (-1.175 * (sp$agecat==6)) + (-0.560 * sp$nonwhite) + (0.267 * sp$nocar) + (0.228 * (sp$ecactivity==1)) +
  (0.036 * sp$urban) + (0.008 * (sp$home_gor==2)) + (0.195 * (sp$home_gor==3)) + (0.170 * (sp$home_gor==4)) + (0.080 * (sp$home_gor==5)) +
  (0.205 * (sp$home_gor==6)) + (0.157 *(sp$home_gor==7)) + (0.267 * (sp$home_gor==8)) + (0.361 * (sp$home_gor==9)) + 
  (-0.020 * sp$slope_ew) + -2.019

sp$pcyclist_scen <- exp(sp$logpcyclist_scen) / (1 + (exp(sp$logpcyclist_scen)))

# How many cyclists to switch?
nextracyclist <- round((targetpcycle * length(unique(sp$census_id))) - length(unique(sp$census_id[sp$cyclist==1])))

# Randomly turn N=sp$nextracyclist non-cyclists to new cyclists, with weight equal to sp$pcyclist_scen
sp$scen_newcyclist <- 0
set.seed(2018)
## Q: how??!
sp$scen_newcyclist[sp$cyclist==0] <- 1 # temp hack where everyone is new cyclist!


## STEP 2: SWITCH TRIPS AMONG NEW CYCLISTS & UPDATE SCENARIO VALUES

# Merge in probability of cycling a trip by distance age and sex, and cycle speed by age and sex 
sp$trip_distcat_km=round(sp$trip_distraw_km)
sp$trip_distcat_km <- car::recode(sp$trip_distcat_km,"0:1=0;2:4=2;5:7=5;8:12=8;13:19=13;20:29=20;30:hi=30")  # Q: COULD DO THIS IN DPLYR INSTEAD?
sp$older <- sp$agecat
sp$older <- car::recode(sp$older,"1:3=0;4:6=1") 
sp <- left_join(sp, distspeed, by=c("trip_distcat_km", "female", "older"))

# Randomly switch some trips to cycling in new cyclists
sp$cyclerandom <- runif(nrow(sp), min = 0, max = 1)
sp$scen_newmaincycletrip <- (sp$cyclist==0 & sp$scen_newcyclist==1 & sp$trip_pcycle>sp$cyclerandom & !is.na(sp$trip_pcycle))

# Convert minutes to hours
sp$trip_durationraw_hr <- sp$trip_durationraw_min/60
sp$trip_cycletime_hr <- sp$trip_cycletime_min/60
sp$trip_walktime_hr <- sp$trip_walktime_min/60

# Initially set scenario values equal to baseline
sp$scen_trip_mainmode <- sp$trip_mainmode
#sp$scen_trip_distraw_km <- sp$trip_distraw_km
sp$scen_trip_durationraw_hr <- sp$trip_durationraw_hr
sp$scen_trip_cycletime_hr <- sp$trip_cycletime_hr
sp$scen_trip_walktime_hr <- sp$trip_walktime_hr
sp$scen_trip_cycledist_km <- sp$trip_cycledist_km
sp$scen_trip_walkdist_km <- sp$trip_walkdist_km

# Update scenario values for the new cycle trips (distance unchanged)
sp$scen_trip_mainmode[sp$scen_newmaincycletrip==1] <- 2 
sp$scen_trip_durationraw_hr[sp$scen_newmaincycletrip==1] <- round(sp$trip_distraw_km[sp$scen_newmaincycletrip==1] / sp$trip_cyclespeed_kmhr[sp$scen_newmaincycletrip==1])
sp$scen_trip_cycletime_hr[sp$scen_newmaincycletrip==1] <- sp$scen_trip_durationraw_hr[sp$scen_newmaincycletrip==1] 
sp$scen_trip_walktime_hr[sp$scen_newmaincycletrip==1] <- 0 
sp$scen_trip_cycledist_km[sp$scen_newmaincycletrip==1] <- sp$trip_distraw_km[sp$scen_newmaincycletrip==1] 
sp$scen_trip_walkdist_km[sp$scen_newmaincycletrip==1] <- 0 

## STEP 3: AGGREGATE TO INDIVIDUAL LEVEL
# Make individual dataset
sp_ind <- unique(sp[,names(sp) %in% c("census_id", "home_lsoa", "home_laname", "home_gor", "urban", "female", "agecat", "nonwhite", "zerotrips", "sport_wkmmets")])

# Distance + Duration summaries by mode
sp$trip_cardrivedist_km  <- sp$trip_distraw_km
sp$trip_cardrivedist_km[!(sp$trip_mainmode %in% c(3))]  <- 0
sp$trip_carpassdist_km  <- sp$trip_distraw_km
sp$trip_carpassdist_km[!(sp$trip_mainmode %in% c(4))]  <- 0
sp$trip_mbikedist_km  <- sp$trip_distraw_km
sp$trip_mbikedist_km[!(sp$trip_mainmode %in% c(5))]  <- 0
sp$trip_busdist_km  <- sp$trip_distraw_km
sp$trip_busdist_km[!(sp$trip_mainmode %in% c(7:9))]  <- 0
sp$trip_taxidist_km  <- sp$trip_distraw_km
sp$trip_taxidist_km[!(sp$trip_mainmode %in% c(12))]  <- 0

sp$trip_cardist_km  <- sp$trip_distraw_km
sp$trip_cardist_km[!(sp$trip_mainmode %in% c(3:4))]  <- 0
sp$trip_mbikedist_km  <- sp$trip_distraw_km
sp$trip_mbikedist_km[!(sp$trip_mainmode %in% c(5))]  <- 0
sp$trip_busdist_km  <- sp$trip_distraw_km
sp$trip_busdist_km[!(sp$trip_mainmode %in% c(7:9))]  <- 0
sp$trip_taxidist_km  <- sp$trip_distraw_km
sp$trip_tubedist_km[!(sp$trip_mainmode %in% c(10))]  <- 0
sp$trip_traindist_km  <- sp$trip_distraw_km
sp$trip_traindist_km[!(sp$trip_mainmode %in% c(11))]  <- 0
sp$trip_taxidist_km  <- sp$trip_distraw_km
sp$trip_taxidist_km[!(sp$trip_mainmode %in% c(12))]  <- 0

# Function to aggregate to individual level: Q = HELP!
agg_to_individ <- function(variable, aggregatedata){
  aggregatedata <- sqldf('select f.census_id, sum(f.variable) as aggregatedata FROM sp as f GROUP BY f.census_id')
  aggregatedata[is.na(aggregatedata)] <- 0
  sp_ind <- left_join(sp_ind,aggregatedata, by="census_id")
  remove(aggregatedata)
}


# Walk/cycle/car driver/car passenger/bus/motorbike distance per week, individuals living in la - base + scenario
###ADD IN WITH FUNCTION


# Walk/cycle/driving/ motorbike/bus/train/tube duration per week, individual level - at baseline and scenario

#agg_to_individ(trip_walktime_hr, base_walk_wkhr)
base_walk_wkhr <- sqldf('select f.census_id, sum(f.trip_walktime_hr) as base_walk_wkhr FROM sp as f GROUP BY f.census_id')
base_walk_wkhr[is.na(base_walk_wkhr)] <- 0
sp_ind <- left_join(sp_ind,base_walk_wkhr, by="census_id")
remove(base_walk_wkhr)

scen_walk_wkhr <- sqldf('select f.census_id, sum(f.scen_trip_walktime_hr) as scen_walk_wkhr FROM sp as f GROUP BY f.census_id')
scen_walk_wkhr[is.na(scen_walk_wkhr)] <- 0
sp_ind <- left_join(sp_ind,scen_walk_wkhr, by="census_id")
remove(scen_walk_wkhr)

base_cycle_wkhr <- sqldf('select f.census_id, sum(f.trip_cycletime_hr) as base_cycle_wkhr FROM sp as f GROUP BY f.census_id')
base_cycle_wkhr[is.na(base_cycle_wkhr)] <- 0
sp_ind <- left_join(sp_ind,base_cycle_wkhr, by="census_id")
remove(base_cycle_wkhr)

scen_cycle_wkhr <- sqldf('select f.census_id, sum(f.scen_trip_cycletime_hr) as scen_cycle_wkhr FROM sp as f GROUP BY f.census_id')
scen_cycle_wkhr[is.na(scen_cycle_wkhr)] <- 0
sp_ind <- left_join(sp_ind,scen_cycle_wkhr, by="census_id")
remove(scen_cycle_wkhr)

# Marginal METs per week, individual
sp_ind$base_mmetwk <- ((met_cycle - 1) *  sp_ind$base_cycle_wkhr) + ((met_walk - 1) * sp_ind$base_walk_wkhr) + sp_ind$sport_wkmmets
sp_ind$scen_mmetwk <- ((met_cycle - 1) *  sp_ind$scen_cycle_wkhr) + ((met_walk - 1) * sp_ind$scen_walk_wkhr) + sp_ind$sport_wkmmets

# Save dataset
