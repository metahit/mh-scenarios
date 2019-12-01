# SET UP
rm(list = ls())
library(foreign)
library(tidyverse)
memory.limit(size=1000000)

## STEP 0: INPUT DATA AND PARAMETERS 

# Load data
# Define LA list
lad14 <- read.csv("../mh-route-commutes/01_DataInput/lad14cd.csv")
rtswt <- read.csv("1_InputData/2_CityRegion_scaling/RTS_NTS_weights.csv")
cityregion <- lad14[,c("lad14cd", "cityregion")]

for(j in 1:length(lad14$lad14cd)){
  lahome <- as.character(lad14$lad14cd[j])
  lafull <- as.numeric(lad14$lahome[j]) ## doing city region ones in full
  sp <-read.dta(paste0("1_InputData/1_LA14trip-level/SPtrip_CensusNTSAPS_", lahome, ".dta"))
  #sp <- sp[1:10000,]
  distspeed <- read.csv("1_InputData/2_OtherInput/NTS_distance_speed.csv")

  # Convert minutes to hours
  sp$trip_durationraw_hr <- sp$trip_durationraw_min/60
  sp$trip_cycletime_hr <- sp$trip_cycletime_min/60
  sp$trip_walktime_hr <- sp$trip_walktime_min/60

  # Merge in scaling weights, and multiply them by the basic trip weight
  sp <- left_join(sp, cityregion, by = c("home_lad14cd"="lad14cd"))
  sp <- left_join(sp, rtswt, by = c("cityregion"="home_cityregion"))
  sp$weight_walkcycle <- sp$weight_tripXhh * sp$rtswt_cycle
  sp$weight_cartaxi <- sp$weight_tripXhh * sp$rtswt_cartaxi
  sp$weight_pt <- sp$weight_tripXhh * sp$rtswt_bus
  sp$weight_mc <- sp$weight_tripXhh * sp$rtswt_mc
  sp$weight_lgv <- sp$weight_tripXhh * sp$rtswt_van
  
  ##### SCENARIO #####
  
  if(lafull==1) {
  # Scenario target % cycle
  targetpcycle <- 0.15
  
  ## STEP 1: IDENTIFY NEW CYCLISTS
  
  # Baseline probability near market [coefficients from a logit model of all adults in England]
  sp$logpcyclist_scen <- (-0.944 * sp$female) + (-0.061 * (sp$agecat==2)) + (-0.032 * (sp$agecat==3)) + (-0.340 * (sp$agecat==4)) + 
    (-0.879 * (sp$agecat==5)) + (-1.175 * (sp$agecat==6)) + (-0.560 * sp$nonwhite) + (0.267 * sp$nocar) + (0.228 * (sp$ecactivity==1)) +
    (0.036 * sp$urban) + (0.008 * (sp$home_gor==2)) + (0.195 * (sp$home_gor==3)) + (0.170 * (sp$home_gor==4)) + (0.080 * (sp$home_gor==5)) +
    (0.205 * (sp$home_gor==6)) + (0.157 *(sp$home_gor==7)) + (0.267 * (sp$home_gor==8)) + (0.361 * (sp$home_gor==9)) + 
    (-0.020 * sp$slope_ew) + -2.019
  
  sp$pcyclist_scen <- exp(sp$logpcyclist_scen) / (1 + (exp(sp$logpcyclist_scen)))
  
  # How many cyclists to switch? (none if mode share already higher)
  nextracyclist <- round((targetpcycle * length(unique(sp$census_id))) - length(unique(sp$census_id[sp$cyclist==1])))
  if(nextracyclist<0) {
    nextracyclist <- 0
  }
  
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
  }
  
  
  ## STEP 3: AGGREGATE TO INDIVIDUAL LEVEL
  # Make individual dataset
  sp$demogindex <- (sp$female*100)+(sp$agecat_det)
  if(lafull==1) {
    sp_ind <- unique(sp[,names(sp) %in% c("census_id", "home_lsoa", "home_laname", "home_gor", "urban", "urbanmatch", "female", "agecat_det", "demogindex", "zerotrips", "sport_wkmmets")])
  }
  if(lafull==0) {
    sp$census_id <- (sp$urbanmatch*1000)+(sp$demogindex)
    sp_ind <- unique(sp[,names(sp) %in% c("census_id", "urbanmatch", "female", "agecat_det", "demogindex")])
  } 
  
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
  sp$trip_vandrivedist_km  <- sp$trip_distraw_km
  sp$trip_vandrivedist_km[!(sp$trip_mainmode_det %in% c(13:14))]  <- 0
  sp$trip_vanpassdist_km  <- sp$trip_distraw_km
  sp$trip_vanpassdist_km[!(sp$trip_mainmode_det %in% c(15:16))]  <- 0
  
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
  sp$trip_vantime_hr  <- sp$trip_durationraw_hr
  sp$trip_vantime_hr[!(sp$trip_mainmode_det %in% c(13:16))]  <- 0
  
  if(lafull==1) {
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
  sp$scen_trip_vandrivedist_km  <- sp$trip_distraw_km
  sp$scen_trip_vandrivedist_km[!(sp$scen_trip_mainmode_det %in% c(13:14))]  <- 0
  sp$scen_trip_vanpassdist_km  <- sp$trip_distraw_km
  sp$scen_trip_vanpassdist_km[!(sp$scen_trip_mainmode_det %in% c(15:16))]  <- 0
  
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
  sp$scen_trip_vantime_hr  <- sp$scen_trip_durationraw_hr
  sp$scen_trip_vantime_hr[!(sp$scen_trip_mainmode_det %in% c(13:16))]  <- 0
  }
  
  # Define distance categories for applying matrices
  sp$distcat <-as.numeric(cut(sp$trip_distraw_km, c(-0.1,5,15,40,100000), labels=c(1:4)))
  sp$distcat[is.na(sp$trip_distraw_km)] <- NA
  sp$distcat[sp$trip_mainmode_det>=1 & sp$trip_mainmode_det<=2] <- 1 # walk at most level 1
  sp$distcat[sp$distcat==4 & sp$trip_mainmode_det==3] <- 3 # bike at most level 3
  sp$distcat[sp$distcat==4 & sp$trip_mainmode_det>=18 & sp$trip_mainmode_det<=20] <- 3 # bus at most level 3
  sp$distcat[is.na(sp$distcat)] <- 0 # give 0 value if missing trip distance, i.e. no travel
  sp$distcat_cycle <- sp$distcat
  if(lafull==1) {
    sp$distcat_cycle[sp$trip_mainmode_det!=3 & sp$scen_newmaincycletrip==0] <- 1 # cycle at most level 1 if in a baseline multimodal trip
  } else {
    sp$distcat_cycle[sp$trip_mainmode_det!=3] <- 1 # cycle at most level 1 if in a baseline multimodal trip
  }

  # Function to aggregate to individual level
  agg_to_individ <- function(trip_level_dataset, individual_dataset, variable, aggregatedata, weight, cycle = F, numdistcat = 1){
   if (numdistcat==1) {
     trip_level_dataset$variable1 <- trip_level_dataset[variable] * trip_level_dataset[weight]
     trip_level_dataset$variable1[is.na(trip_level_dataset$variable1),] <- 0
     df <- trip_level_dataset %>% group_by(census_id) %>% summarise (total = sum(variable1))
     names(df)[2] <- aggregatedata
    } else {
     trip_level_dataset$variable_d1 <- trip_level_dataset[variable] * trip_level_dataset[weight]
     trip_level_dataset$variable_d2 <- trip_level_dataset[variable] * trip_level_dataset[weight]
     trip_level_dataset$variable_d3 <- trip_level_dataset[variable] * trip_level_dataset[weight]
     trip_level_dataset$variable_d4 <- trip_level_dataset[variable] * trip_level_dataset[weight]
     if (cycle==F) {
        trip_level_dataset$variable_d1[trip_level_dataset$distcat!=1 | is.na(trip_level_dataset$variable_d1),] <- 0
        trip_level_dataset$variable_d2[trip_level_dataset$distcat!=2 | is.na(trip_level_dataset$variable_d2),] <- 0    
        trip_level_dataset$variable_d3[trip_level_dataset$distcat!=3 | is.na(trip_level_dataset$variable_d3),] <- 0    
        trip_level_dataset$variable_d4[trip_level_dataset$distcat!=4 | is.na(trip_level_dataset$variable_d4),] <- 0
      } else {
        trip_level_dataset$variable_d1[trip_level_dataset$distcat_cycle!=1 | is.na(trip_level_dataset$variable_d1),] <- 0
        trip_level_dataset$variable_d2[trip_level_dataset$distcat_cycle!=2 | is.na(trip_level_dataset$variable_d2),] <- 0    
        trip_level_dataset$variable_d3[trip_level_dataset$distcat_cycle!=3 | is.na(trip_level_dataset$variable_d3),] <- 0    
      }
        df_d1 <- trip_level_dataset %>% group_by(census_id) %>% summarise(total = sum(variable_d1))
        names(df_d1)[2] <- paste0(aggregatedata,"_d1")
        df_d2 <- trip_level_dataset %>% group_by(census_id) %>% summarise(total = sum(variable_d2))
        names(df_d2)[2] <- paste0(aggregatedata,"_d2")
        df_d3 <- trip_level_dataset %>% group_by(census_id) %>% summarise(total = sum(variable_d3))
        names(df_d3)[2] <- paste0(aggregatedata,"_d3")
        df12 <- full_join(df_d1, df_d2, by="census_id")
        if (numdistcat==3) {
        df <- full_join(df12, df_d3, by="census_id")
         } else {
        df_d4 <- trip_level_dataset %>% group_by(census_id) %>% summarise(total = sum(variable_d4))
        names(df_d4)[2] <- paste0(aggregatedata,"_d4")
        df34 <- full_join(df_d3, df_d4, by="census_id")
        df <- full_join(df12, df34, by="census_id")
       }
      }
    df[is.na(df)] <- 0
    individual_dataset <- left_join(individual_dataset, df, by="census_id")
    individual_dataset  
  }
    
  # Baseline distance 
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_walkdist_km', 'base_walk_wkkm_d1', 'weight_walkcycle')
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_cycledist_km', 'base_cycle_wkkm', 'weight_walkcycle', cycle = T, numdistcat = 3)
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_cardrivedist_km', 'base_cardrive_wkkm', 'weight_cartaxi', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_carpassdist_km', 'base_carpass_wkkm', 'weight_cartaxi', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_mbikedrivedist_km', 'base_mbikedrive_wkkm', 'weight_mc', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_mbikepassdist_km', 'base_mbikepass_wkkm', 'weight_mc', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_busdist_km', 'base_bus_wkkm', 'weight_pt', numdistcat = 3)
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_taxidist_km', 'base_taxi_wkkm', 'weight_cartaxi', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_vandrivedist_km', 'base_vandrive_wkkm', 'weight_lgv', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_vanpassdist_km', 'base_vanpass_wkkm', 'weight_lgv', numdistcat = 4)
  
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_walktime_hr', 'base_walk_wkhr', 'weight_walkcycle')
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_cycletime_hr', 'base_cycle_wkhr', 'weight_walkcycle')
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_cartime_hr', 'base_car_wkhr', 'weight_cartaxi')
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_mbiketime_hr', 'base_mbike_wkhr', 'weight_mc')
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_bustime_hr', 'base_bus_wkhr', 'weight_pt')
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_tubetime_hr', 'base_tube_wkhr', 'weight_pt')
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_traintime_hr', 'base_train_wkhr', 'weight_pt')
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_taxitime_hr', 'base_taxi_wkhr', 'weight_cartaxi')
  sp_ind <- agg_to_individ(sp, sp_ind, 'trip_vantime_hr', 'base_van_wkhr', 'weight_lgv')
  
  # Scenario distance 
  if(lafull==1) {
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_walkdist_km', 'scen_walk_wkkm_d1', 'weight_walkcycle')
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_cycledist_km', 'scen_cycle_wkkm', 'weight_walkcycle', cycle = T,  numdistcat = 3)
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_cardrivedist_km', 'scen_cardrive_wkkm', 'weight_cartaxi', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_carpassdist_km', 'scen_carpass_wkkm', 'weight_cartaxi', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_mbikedrivedist_km', 'scen_mbikedrive_wkkm', 'weight_mc', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_mbikepassdist_km', 'scen_mbikepass_wkkm', 'weight_mc', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_busdist_km', 'scen_bus_wkkm', 'weight_pt', numdistcat = 3)
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_taxidist_km', 'scen_taxi_wkkm', 'weight_cartaxi', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_vandrivedist_km', 'scen_vandrive_wkkm', 'weight_lgv', numdistcat = 4)
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_vanpassdist_km', 'scen_vanpass_wkkm', 'weight_lgv', numdistcat = 4)
  
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_walktime_hr', 'scen_walk_wkhr', 'weight_walkcycle')
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_cycletime_hr', 'scen_cycle_wkhr', 'weight_walkcycle')
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_cartime_hr', 'scen_car_wkhr', 'weight_cartaxi')
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_mbiketime_hr', 'scen_mbike_wkhr', 'weight_mc')
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_bustime_hr', 'scen_bus_wkhr', 'weight_pt')
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_tubetime_hr', 'scen_tube_wkhr', 'weight_pt')
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_traintime_hr', 'scen_train_wkhr', 'weight_pt')
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_taxitime_hr', 'scen_taxi_wkhr', 'weight_cartaxi')
  sp_ind <- agg_to_individ(sp, sp_ind, 'scen_trip_vantime_hr', 'scen_van_wkhr', 'weight_lgv')
  }
  
  # Marginal METs per week, individual [not created her as met_cycle/walk random variables]
  #sp_ind$base_mmetwk <- ((met_cycle - 1) *  sp_ind$base_cycle_wkhr) + ((met_walk - 1) * sp_ind$base_walk_wkhr) + sp_ind$sport_wkmmets
  #sp_ind$scen_mmetwk <- ((met_cycle - 1) *  sp_ind$scen_cycle_wkhr) + ((met_walk - 1) * sp_ind$scen_walk_wkhr) + sp_ind$sport_wkmmets
  
  # Save dataset
  if(lafull==1) {
    saveRDS(sp_ind, file.path("../mh-execute/inputs/scenarios", paste0("SPind_", lahome, ".Rds")),version=2)
  } else {
    saveRDS(sp_ind, file.path("../mh-execute/inputs/scenarios-mini", paste0("SPind_", lahome, ".Rds")),version=2)
  }
    
  print(paste0("Scenario run for home LA ",lahome, " (", j , ") at ",Sys.time()))
}