#10 September 2023
  #we have our environmental values: daily, monthly average and the anomaly
    #lets clean our df up and add a few more environmental variables
  
  
rm(list=ls())
#setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

dat <- read_csv("Inputs/230910_cur_det.csv")



# REMORA ------------------------------------------------------------------

#for bathymetry and distance to land
dat1 <- 
  extractEnv(df = dat, #[row:number, col:number] 
            X = "receiver_deployment_longitude", 
            Y = "receiver_deployment_latitude",
            datetime = "detection_datetime", #datetime variable to be formatted correctly
             env_var = "bathy", 
             #'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land'
             cache_layers = F,
             crop_layers = TRUE,
            fill_gaps = TRUE,
            full_timeperiod = F,
           #nrt = T, #for branched version only at the moment
            folder_name = "VP2 AUG detections",
             .parallel = parallel::detectCores() - 2) #parallel::detectCores() - 2



# La luna -----------------------------------------------------------------

dat2 <- dat1

dat2$lunar.illumination <- lunar.illumination(dat2$detection_datetime, shift = 10) #10 hrs shifted from UTM
dat2$lunar.phase <- lunar.phase(dat2$detection_datetime, shift = 10) #10 hrs shifted from UTM

#turn radians into degrees to plot circularly
rad2deg <- function(rad) return(180*rad/pi)

# Add a column for lunar.phase in degrees
dat2$lunar.phase.deg <- rad2deg(dat2$lunar.phase)


dat3 <- dat2 %>% 
  dplyr::select(-receiver_deployment_latitude, -receiver_deployment_longitude)


# where do the NAs go ? ---------------------------------------------------

# Create a new data frame with rows where SST is NA
NAs <- dat3 %>% filter(is.na(SST)) %>% 
  group_by(Location) %>%
  summarise(na_count = sum(is.na(SST)))

unique(NAs$Location)

dat4 <- dat3 %>% 
  group_by(Location) %>%
  summarise(sum = n())

# Join det3 and NAsst by Location and calculate the difference
diff <- dat4 %>%
  left_join(NAs, by = "Location") %>% # Join on Location
  mutate(difference = sum - ifelse(is.na(na_count), 0, na_count)) # Calculate the difference

#Montague Isl & Port Macq, gone :(

# save --------------------------------------------------------------------

write_csv(dat3, "Inputs/230910_complete_det_enviro.csv")

