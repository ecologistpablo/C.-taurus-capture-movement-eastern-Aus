#10 September 2023
  #we have our environmental values: daily, monthly average and the anomaly
    #lets clean our df up and add a few more environmental variables
  
rm(list=ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(lunar)

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/250701_cur_det.csv")

# REMORA ------------------------------------------------------------------

#' #for bathymetry and distance to land
#' dat1 <- 
#'   extractEnv(df = dat, #[row:number, col:number] 
#'             X = "receiver_deployment_longitude", 
#'             Y = "receiver_deployment_latitude",
#'             datetime = "detection_datetime", #datetime variable to be formatted correctly
#'              env_var = "dist_to_land", #change to what you need
#'              #'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land'
#'              cache_layers = F,
#'              crop_layers = TRUE,
#'             fill_gaps = TRUE,
#'             full_timeperiod = F,
#'            #nrt = T, #for branched version only at the moment (thanks to my stubborn butt)
#'             folder_name = "VP2 AUG detections",
#'              .parallel = parallel::detectCores() - 2) #parallel::detectCores() - 2

# we don't use packages that can't work with temporal anomalies :) 
# If I had time I would push a workflow for this into REMORA, but I don't : ( 

# La luna -----------------------------------------------------------------

#lunar illumination is measured on a scale of 0 - 1, it omits the need to model a continuous variable cyclically 
#lunar cycle can be utilised, however it is harder to pick up ecological anomalies

dat1 <- dat

dat1$lunar.illumination <- lunar.illumination(dat1$date, shift = 10) #10 hrs shifted from UTM

# if you want to work with circular data, you can use lunar phase
# it outputs data in radians, and you can convert it to degrees
# you can make cute rose / circular plots with this, but it's hard to model
# because 0-1 can be binomial, 0 - 360 is circular and requires 
# a different type of manipulation in statistical models to fit relationships
# correctly

#dat2$lunar.phase <- lunar.phase(dat2$detection_datetime, shift = 10) #10 hrs shifted from UTM

# #turn radians into degrees to plot circularly
# rad2deg <- function(rad) return(180*rad/pi)
# 
# # Add a column for lunar.phase in degrees
# dat2$lunar.phase.deg <- rad2deg(dat2$lunar.phase)

colnames(dat1)
str(dat1)

# final clean -------------------------------------------------------------

dat2 <- dat1 %>% 
  select(-vcur, -vcur_anomaly, -original_id) %>% 
  mutate(tag_id = as.character(tag_id))

colnames(dat2)
str(dat2)


# plot it -----------------------------------------------------------------

datxy <- dat2 %>%
  group_by(location, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

datxy_sf <- sf::st_as_sf(datxy, coords = c("longitude", "latitude"),
                        crs = 4326, agr = "constant")

mapview::mapview(datxy_sf, cex = "num_det", zcol = "location", fbg = FALSE)

# save --------------------------------------------------------------------

write_csv(dat2, "Inputs/250701_det_enviro_complete.csv")

