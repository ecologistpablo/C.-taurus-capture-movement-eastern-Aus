# 04 September 2023
  # Who needs remora anyway
    # Interpolating NAs in SST data

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

rcs <- read_csv("Inputs/receiver_station_XY_230822.csv")
head(rcs) #its all there

pts.sp <- st_as_sf(rcs, coords = c("receiver_deployment_longitude", #convert to an SF object
                                   "receiver_deployment_latitude")) 

st_crs(pts.sp) <- crs(UTM56S) #remember to assign crs
pts.sp

pts.UTM <- st_transform(pts.sp, UTM56S) #reproject our data
pts.UTM

# plotting ----------------------------------------------------------------

plot(rstack[[1]], col = viridis(255))

