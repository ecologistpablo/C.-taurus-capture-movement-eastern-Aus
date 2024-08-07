#10 September 2023
  #we have our environmental values: daily, monthly average and the anomaly
    #lets clean our df up and add a few more environmental variables
  
  
rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/240806_cur_det.csv")

# REMORA ------------------------------------------------------------------

#for bathymetry and distance to land
dat1 <- 
  extractEnv(df = dat, #[row:number, col:number] 
            X = "receiver_deployment_longitude", 
            Y = "receiver_deployment_latitude",
            datetime = "detection_datetime", #datetime variable to be formatted correctly
             env_var = "dist_to_land", #change to what you need
             #'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land'
             cache_layers = F,
             crop_layers = TRUE,
            fill_gaps = TRUE,
            full_timeperiod = F,
           #nrt = T, #for branched version only at the moment (thanks to my stubborn butt)
            folder_name = "VP2 AUG detections",
             .parallel = parallel::detectCores() - 2) #parallel::detectCores() - 2

# La luna -----------------------------------------------------------------

#lunar illumination is measured on a scale of 0 - 1, it omits the need to model a continuous variable cyclically 
#lunar cycle can be utilised, however it is harder to pick up ecological anomalies

dat2 <- dat

dat2$lunar.illumination <- lunar.illumination(dat2$detection_datetime, shift = 10) #10 hrs shifted from UTM
dat2$lunar.phase <- lunar.phase(dat2$detection_datetime, shift = 10) #10 hrs shifted from UTM

#turn radians into degrees to plot circularly
rad2deg <- function(rad) return(180*rad/pi)

# Add a column for lunar.phase in degrees
dat2$lunar.phase.deg <- rad2deg(dat2$lunar.phase)

# save --------------------------------------------------------------------

write_csv(dat2, "Inputs/240806_complete_det_enviro.csv")

