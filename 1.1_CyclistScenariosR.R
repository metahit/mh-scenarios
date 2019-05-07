# SET UP
rm(list = ls())
library(foreign)
library(tidyverse)
memory.limit(size=1000000)


## STEP 0: INPUT DATA AND PARAMETERS 

# Load data
lad14cd <- "E06000022"
sp <-read.dta(paste0("1_InputData/1_LA14trip-level/SPtrip_CensusNTSAPS_", lad14cd, ".dta"))
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
unique_non_cyclists_df <- sp %>% distinct(census_id, .keep_all = TRUE) %>% filter(cyclist != 1)
sampled_non_cyclists_df <- sample_n(unique_non_cyclists_df, size = nextracyclist, weight = unique_non_cyclists_df$pcyclist_scen)
sp$scen_newcyclist[sp$census_id %in% sampled_non_cyclists_df$census_id] <- 1 


## STEP 2: SWITCH TRIPS AMONG NEW CYCLISTS & UPDATE SCENARIO VALUES

# Merge in probability of cycling a trip by distance age and sex, and cycle speed by age and sex 
sp$trip_distcat_km=round(sp$trip_distraw_km)

sp <-  sp %>% dplyr::mutate(trip_distcat_km = case_when(trip_distcat_km %in% c(0:1) ~ 0,
                                                        trip_distcat_km %in% c(2:4) ~ 2,
                                                        trip_distcat_km %in% c(5:7) ~ 5,
                                                        trip_distcat_km %in% c(8:12) ~ 8,
                                                        trip_distcat_km %in% c(13:19) ~ 13,
                                                        trip_distcat_km %in% c(20:29) ~ 20,
                                                        trip_distcat_km >= 30 ~ 30))
sp <-  sp %>% dplyr::mutate(older = case_when(agecat %in% c(1:3) ~ 0,
                                              agecat %in% c(4:6) ~ 1))
sp <- left_join(sp, distspeed, by=c("trip_distcat_km", "female", "older"))

# Randomly switch some trips to cycling in new cyclists
sp$cyclerandom <- runif(nrow(sp), min = 0, max = 1)
sp$scen_newmaincycletrip <- (sp$cyclist==0 & sp$scen_newcyclist==1 & sp$trip_pcycle>sp$cyclerandom & !is.na(sp$trip_pcycle))

# Convert minutes to hours
sp$trip_durationraw_hr <- sp$trip_durationraw_min/60
sp$trip_cycletime_hr <- sp$trip_cycletime_min/60
sp$trip_walktime_hr <- sp$trip_walktime_min/60

# Initially set scenario values equal to baseline
sp$scen_trip_mainmode_det <- sp$trip_mainmode_det
sp$scen_trip_durationraw_hr <- sp$trip_durationraw_hr
sp$scen_trip_cycletime_hr <- sp$trip_cycletime_hr
sp$scen_trip_walktime_hr <- sp$trip_walktime_hr
sp$scen_trip_cycledist_km <- sp$trip_cycledist_km
sp$scen_trip_walkdist_km <- sp$trip_walkdist_km

# Update scenario values for the new cycle trips (distance unchanged)
sp$scen_trip_mainmode_det[sp$scen_newmaincycletrip==1] <- 3 
sp$scen_trip_durationraw_hr[sp$scen_newmaincycletrip==1] <- round(sp$trip_distraw_km[sp$scen_newmaincycletrip==1] / sp$trip_cyclespeed_kmhr[sp$scen_newmaincycletrip==1])
sp$scen_trip_cycletime_hr[sp$scen_newmaincycletrip==1] <- sp$scen_trip_durationraw_hr[sp$scen_newmaincycletrip==1] 
sp$scen_trip_walktime_hr[sp$scen_newmaincycletrip==1] <- 0 
sp$scen_trip_cycledist_km[sp$scen_newmaincycletrip==1] <- sp$trip_distraw_km[sp$scen_newmaincycletrip==1] 
sp$scen_trip_walkdist_km[sp$scen_newmaincycletrip==1] <- 0 


## STEP 3: AGGREGATE TO INDIVIDUAL LEVEL
# Make individual dataset
sp$demogindex <- (sp$female*100)+(sp$agecat_det)
sp_ind <- unique(sp[,names(sp) %in% c("census_id", "home_lsoa", "home_laname", "home_gor", "urban", "female", "agecat_det", "demogindex", "zerotrips", "sport_wkmmets")])

# Distance + Duration summaries by mode, baseline + scenario
sp$trip_cardrivedist_km  <- sp$trip_distraw_km
sp$trip_cardrivedist_km[!(sp$trip_mainmode_det %in% c(5:6))]  <- 0
sp$trip_carpassdist_km  <- sp$trip_distraw_km
sp$trip_carpassdist_km[!(sp$trip_mainmode_det %in% c(7:8))]  <- 0
sp$trip_mbikedrivedist_km  <- sp$trip_distraw_km
sp$trip_mbikedrivedist_km[!(sp$trip_mainmode_det %in% c(9:10))]  <- 0
sp$trip_mbikepassdist_km  <- sp$trip_distraw_km
sp$trip_mbikepassdist_km[!(sp$trip_mainmode_det %in% c(11:12))]  <- 0
sp$trip_busdist_km  <- sp$trip_distraw_km
sp$trip_busdist_km[!(sp$trip_mainmode_det %in% c(18:20))]  <- 0
sp$trip_taxidist_km  <- sp$trip_distraw_km
sp$trip_taxidist_km[!(sp$trip_mainmode_det %in% c(26:27))]  <- 0

sp$scen_trip_cardrivedist_km  <- sp$trip_distraw_km
sp$scen_trip_cardrivedist_km[!(sp$scen_trip_mainmode_det %in% c(5:6))]  <- 0
sp$scen_trip_carpassdist_km  <- sp$trip_distraw_km
sp$scen_trip_carpassdist_km[!(sp$scen_trip_mainmode_det %in% c(7:8))]  <- 0
sp$scen_trip_mbikedrivedist_km  <- sp$trip_distraw_km
sp$scen_trip_mbikedrivedist_km[!(sp$scen_trip_mainmode_det %in% c(9:10))]  <- 0
sp$scen_trip_mbikepassdist_km  <- sp$trip_distraw_km
sp$scen_trip_mbikepassdist_km[!(sp$scen_trip_mainmode_det %in% c(11:12))]  <- 0
sp$scen_trip_busdist_km  <- sp$trip_distraw_km
sp$scen_trip_busdist_km[!(sp$scen_trip_mainmode_det %in% c(18:20))]  <- 0
sp$scen_trip_taxidist_km  <- sp$trip_distraw_km
sp$scen_trip_taxidist_km[!(sp$scen_trip_mainmode_det %in% c(26:27))]  <- 0

sp$trip_cartime_hr  <- sp$trip_durationraw_hr
sp$trip_cartime_hr[!(sp$trip_mainmode_det %in% c(5:8))]  <- 0
sp$trip_mbiketime_hr  <- sp$trip_durationraw_hr
sp$trip_mbiketime_hr[!(sp$trip_mainmode_det %in% c(9:12))]  <- 0
sp$trip_bustime_hr  <- sp$trip_durationraw_hr
sp$trip_bustime_hr[!(sp$trip_mainmode_det %in% c(18:20))]  <- 0
sp$trip_tubetime_hr  <- sp$trip_durationraw_hr
sp$trip_tubetime_hr[!(sp$trip_mainmode_det %in% c(22))]  <- 0
sp$trip_traintime_hr  <- sp$trip_durationraw_hr
sp$trip_traintime_hr[!(sp$trip_mainmode_det %in% c(23:24))]  <- 0
sp$trip_taxitime_hr  <- sp$trip_durationraw_hr
sp$trip_taxitime_hr[!(sp$trip_mainmode_det %in% c(26:27))]  <- 0

sp$scen_trip_cartime_hr  <- sp$scen_trip_durationraw_hr
sp$scen_trip_cartime_hr[!(sp$scen_trip_mainmode_det %in% c(5:8))]  <- 0
sp$scen_trip_mbiketime_hr  <- sp$scen_trip_durationraw_hr
sp$scen_trip_mbiketime_hr[!(sp$scen_trip_mainmode_det %in% c(9:12))]  <- 0
sp$scen_trip_bustime_hr  <- sp$scen_trip_durationraw_hr
sp$scen_trip_bustime_hr[!(sp$scen_trip_mainmode_det %in% c(18:20))]  <- 0
sp$scen_trip_tubetime_hr  <- sp$scen_trip_durationraw_hr
sp$scen_trip_tubetime_hr[!(sp$scen_trip_mainmode_det %in% c(22))]  <- 0
sp$scen_trip_traintime_hr  <- sp$scen_trip_durationraw_hr
sp$scen_trip_traintime_hr[!(sp$scen_trip_mainmode_det %in% c(23:24))]  <- 0
sp$scen_trip_taxitime_hr  <- sp$scen_trip_durationraw_hr
sp$scen_trip_taxitime_hr[!(sp$scen_trip_mainmode_det %in% c(26:27))]  <- 0

# Function to aggregate to individual level
agg_to_individ <- function(trip_level_dataset, individual_dataset, variable, aggregatedata){
  df <- trip_level_dataset %>% group_by(census_id) %>% summarise (sum(UQ(as.name(variable))))
  names(df)[2] <- aggregatedata
  df[is.na(df)] <- 0
  individual_dataset <- left_join(individual_dataset, df, by="census_id")
  individual_dataset
}

# Walk/cycle/car driver/car passenger/bus/motorbike distance per week, individuals living in la - base + scenario
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_walkdist_km', 'base_walk_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_walkdist_km', 'scen_walk_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_cycledist_km', 'base_cycle_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_cycledist_km', 'scen_cycle_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_cardrivedist_km', 'base_cardrive_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_cardrivedist_km', 'scen_cardrive_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_carpassdist_km', 'base_carpass_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_carpassdist_km', 'scen_carpass_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_mbikedrivedist_km', 'base_mbikedrive_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_mbikedrivedist_km', 'scen_mbikedrive_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_mbikepassdist_km', 'base_mbikepass_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_mbikepassdist_km', 'scen_mbikepass_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_busdist_km', 'base_bus_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_busdist_km', 'scen_bus_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_taxidist_km', 'base_taxi_wkkm')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_taxidist_km', 'scen_taxi_wkkm')

# Walk/cycle/driving/ motorbike/bus/train/tube duration per week, individual level - at baseline and scenario
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_walktime_hr', 'base_walk_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_walktime_hr', 'scen_walk_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_cycletime_hr', 'base_cycle_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_cycletime_hr', 'scen_cycle_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_cartime_hr', 'base_car_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_cartime_hr', 'scen_car_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_mbiketime_hr', 'base_mbike_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_mbiketime_hr', 'scen_mbike_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_bustime_hr', 'base_bus_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_bustime_hr', 'scen_bus_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_tubetime_hr', 'base_tube_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_tubetime_hr', 'scen_tube_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_traintime_hr', 'base_train_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_traintime_hr', 'scen_train_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'trip_taxitime_hr', 'base_taxi_wkhr')
sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_taxitime_hr', 'scen_taxi_wkhr')

# Marginal METs per week, individual
sp_ind$base_mmetwk <- ((met_cycle - 1) *  sp_ind$base_cycle_wkhr) + ((met_walk - 1) * sp_ind$base_walk_wkhr) + sp_ind$sport_wkmmets
sp_ind$scen_mmetwk <- ((met_cycle - 1) *  sp_ind$scen_cycle_wkhr) + ((met_walk - 1) * sp_ind$scen_walk_wkhr) + sp_ind$sport_wkmmets

# Save dataset
saveRDS(sp_ind, file.path("2_OutputData", paste0("SPind_", lad14cd, ".Rds")),version=2)

