#10 September 2023
  #we have our environmental values: daily, monthly average and the anomaly
    #lets clean our df up and add a few more environmental variables


rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

dat <- read_csv("Inputs/230912_SCP_enviro.csv")

str(dat)

dat$Date <- as.Date(dat$Date)

# REMORA ------------------------------------------------------------------

#for bathymetry and distance to land
dat1 <- 
  extractEnv(df = dat, #[row:number, col:number] 
             X = "Longitude", 
             Y = "Latitude",
             datetime = "Date", #datetime variable to be formatted correctly
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

dat2$lunar.illumination <- lunar.illumination(dat2$Date, shift = 10) #10 hrs shifted from UTM
dat2$lunar.phase <- lunar.phase(dat2$Date, shift = 10) #10 hrs shifted from UTM

#turn radians into degrees to plot circularly
rad2deg <- function(rad) return(180*rad/pi)

# Add a column for lunar.phase in degrees
dat2$lunar.phase.deg <- rad2deg(dat2$lunar.phase)

colnames(dat2) #what can we drop? we will just be modelling and visualling from now onwards

dat3 <- dat2 %>% 
  dplyr::select(-Latitude, -Longitude)

# save --------------------------------------------------------------------

write_csv(dat2, "Inputs/230924_complete_SCP_enviro.csv")



# map ---------------------------------------------------------------------

# Calculate the number of detections at each station
IMOSxy <- dat2 %>%
  group_by(Area, Latitude, Longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("Longitude", "Latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "Area", fbg = F)

