# 04 September 2023
  # Who needs REMORA anyway
    # extracting SST values

#script by D. Schoeman

# disclaimer, REMORA works beautifully, sometimes...
# we needed to generate temporal anomalies to remove seasonality out of our psuedo-absences
# we will now connect our environmental data with our data points
# we do this by making a csv with all of our xy coordinates for each data point
# we then punch through every data point through 11 years of data
# and return a enviro value for every day, if it returns as NA we have some tricks up our sleeve

rm(list=ls())

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")
library(terra)
library(sf)
library(sp)

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")
rcs <- read_csv("Inputs/250301_xy_coordinates.csv") #this should be a csv with your XY coordinates for your receivers
WGS84 <- terra::crs("EPSG:4326")# Coordinate reference systems

head(rcs) #its all there

pts.UTM <- st_as_sf(rcs, coords = c("longitude", #convert to an SF object
                                   "latitude")) 

st_crs(pts.UTM) <- crs(WGS84) #remember to assign crs
crs(pts.UTM) # did it assign correctly?
head(pts.UTM) # do we have an orderly dataframe?

# plot --------------------------------------------------------------------

# Calculate the number of detections at each station
IMOSxy <- rcs %>%
  group_by(location, latitude, longitude) %>% #location
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude", "latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = F) #colour by LOCATION

# rstack ------------------------------------------------------------------

rstack <- rast("IMOS/SST/GHRSST_12-22.tif") #our enviro data

# plotting ----------------------------------------------------------------

plot(rstack[[19]], col = viridis(255)) #did it work as we hoped for?
plot(pts.UTM, add = T) #our do our points plot ontop of our enviro data? 

# extract -----------------------------------------------------------------

sst.pts <- extract(rstack, pts.UTM, ID = F) # ID = FALSE otherwise it creates a column with a number for each spoint

sum(is.na(sst.pts)) #how many values were filled in the 2km resolution direct extraction
3871 * 114 #obs * columns = total obs
(307253 / 441294) * 100 

# nearest temporal neighbour ----------------------------------------------

#Supervisors K. Scales, R. Dwyer & D. Schoeman recommend travelling in time THEN space is more appropriate when filling NAs

#so we shall travel one day, then average over five days, then increase spatial resolution of enviro data

sst.pts1 <- fill1dneighbour(sst.pts) #find the function in the helpers script

sum(is.na(sst.pts1))
(181977 / 441294) * 100

# 5 d mean ----------------------------------------------------------------

# Apply the function to each row and save the new values in sst.pts2
sst.pts2 <- t(apply(sst.pts1, 1, mean_5d)) #mean_5d is in our helpers script we called in the beginning

sst.pts2 <- as.data.frame(sst.pts2)
colnames(sst.pts2) <- colnames(sst.pts1)

sum(is.na(sst.pts1)) - sum(is.na(sst.pts2))  #only 24 were filled :o

sum(is.na(sst.pts2)) 
(181953 / 441294) * 100 #41.231% are NA at a 2km resolution

#with nearest neighbour from single day & 5 d mean

# read and write ----------------------------------------------------------

#write_csv(sst.pts2, file = "Inputs/230911_SST_vals_12-22_pts2.csv")

#sst.pts2 <- read_csv("Inputs/230909_SST_vals_12-22_pts2.csv")

# increase coarseness -----------------------------------------------------

rstack #res at 0.02 by 0.02, 2km x 2km

0.09 / 0.02 #0.02 is our res, but we want 0.09 (10km)

rstack10km <- aggregate(rstack, fact = 4.5, #factor of whatever your resolution is in the OG raster / stack
                        fun = mean, na.rm = TRUE) #na.rm = T means it will interpolate into land
#this is aprox. 10km resolution now

# resample ----------------------------------------------------------------

sst.pts10km <- extract(rstack10km, pts.UTM, ID = F) 

# nearest temporal neighbour ----------------------------------------------

sst.pts10km1 <- fill1dneighbour(sst.pts10km) #find the function in the helpers script

sum(is.na(sst.pts10km1))
(11615 / 69678) * 100

# 5 d mean ----------------------------------------------------------------

# Apply the function to each row and save the new values in sst.pts2
sst.pts10km2 <- t(apply(sst.pts10km1, 1, mean_5d))

#more munging
sst.pts10km2 <- as.data.frame(sst.pts10km2)
colnames(sst.pts10km2) <- colnames(sst.pts10km1)

sum(is.na(sst.pts10km2))
(46464 / 441294) * 100

#fill values of 10km res into our 2km res
sst.pts3 <- fill_vals(sst.pts2, sst.pts10km2) #fill_vals can be found in helpers

sum(is.na(sst.pts3))

(11613 / 69678) * 100
#10.53% of our data is NA which is manageable for 18 unique coordinates across 4015 days 

# fill gaps bilinear ------------------------------------------------------

bl <- extract(rstack10km, pts.UTM, method = "bilinear", ID = FALSE)

#fill vals of bilinear interpolation into our data
sst.pts4 <- fill_vals(sst.pts3, bl)

sum(is.na(sst.pts3)) - sum(is.na(sst.pts4)) #how many did the bilinear interpolation fill ?
# one row filled, nice. Montague Island and Jervis bay still don't seem to want to be filled...


# # 25 km resolution --------------------------------------------------------
# 
# 0.09 * 2.5 #0.09 is 10km so we want 25km
# 0.225 / 0.02 #this value = the multiplication of base reso to 25km
# 
# rstack25km <- aggregate(rstack, fact = 11.25, #factor of whatever your resolution is in the OG raster / stack
#                         fun = mean, na.rm = TRUE) #na.rm = T means it will interpolate into land
# #this is aprox. 10km resolution now
# 
# sst.pts25km <- extract(rstack25km, pts.UTM, ID = F)
# 
# #fill vals of bilinear interpolation into our data
# sst.pts4 <- fill_vals(sst.pts3, bl)
# 
# sum(is.na(sst.pts3)) - sum(is.na(sst.pts4)) #how many did the bilinear interpolation fill ?
# # one row filled, nice. Montague Island and Jervis bay still don't seem to want to be filled...
# 25km did nothing
rm(sst.pts25km)

# add station name --------------------------------------------------------

rcs <-  rcs %>% mutate(RowNumber = row_number()) #make a row number 
sst.pts3 <-  sst.pts3 %>% mutate(RowNumber = row_number()) #make a row number 


sst.pts4 <- left_join(sst.pts3, rcs 
                      %>% dplyr::select(RowNumber, location), by = "RowNumber")

#re-order it
sst.pts5 <- sst.pts4 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(location, everything())

# summary -----------------------------------------------------------------

# so, what we have done here is slightly confusing, but sensible 
# we have extracted values from 2km resolution, then filled 1 d neighbours, then 5 d means
# we have then increased coarseness to 10km resolution, filled 1 d neighbours then 5 d means
# then, we said if there are any values in 10km res dataframe that are NA in the 2km dataframe, fill it
# we did the same thing with bilinear interpolation, which was conducted solely on the 2km res level

# save --------------------------------------------------------------------

write_csv(sst.pts5, file = "Inputs/250211_SST_vals_12-22.csv")

#finish script

