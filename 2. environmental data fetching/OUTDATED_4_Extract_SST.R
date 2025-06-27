# 04 September 2023
  # Who needs REMORA anyway
    # extracting SST values

#script begun by Prof. D. Schoeman

# disclaimer, REMORA works beautifully, sometimes...
# we needed to generate temporal anomalies to remove seasonality out of our psuedo-absences
# we will now connect our environmental data with our data points
# we do this by making a csv with all of our xy coordinates for each data point
# we then punch through every data point through 12 years of data
# and return a enviro value for every day, if it returns as NA we have some tricks up our sleeve

rm(list=ls())

# Packages ----------------------------------------------------------------

library(terra)
library(sf)
library(sp)
library(viridis)
library(lubridate)
library(tidyr)

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")
rcs <- read_csv("Inputs/250626_xy_coordinates.csv") #this should be a csv with your XY coordinates for your receivers
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
  group_by(station_name, latitude, longitude) %>% #location
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude", "latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, zcol = "station_name", fbg = F) #colour by LOCATION

# rstack ------------------------------------------------------------------

rstack <- rast("IMOS/SST/GHRSST_12-22.tif") #our enviro data

# plotting ----------------------------------------------------------------

plot(rstack[[19]], col = viridis(255)) #did it work as we hoped for?
plot(pts.UTM, add = T) #our do our points plot ontop of our enviro data? 

# increase coarseness -----------------------------------------------------

rstack #res at 0.02 by 0.02, 2km x 2km
0.09 / 0.02 #0.02 is our res, but we want 0.09 (10km)
rstack10km <- aggregate(rstack, fact = 4.5, #factor of whatever your resolution is in the OG raster / stack
                        fun = mean, na.rm = TRUE) #na.rm = T means it will interpolate into land
#this is 10km resolution now

# sample ------------------------------------------------------------------

sst.pts10km <- extract(rstack10km, pts.UTM, ID = F) 

# 7 day averages ----------------------------------------------------------

# Step 2: Add location names and layer dates
sst.pts10km$station_name <- pts.UTM$station_name

layer_dates <- as.Date(gsub("SST_", "", names(rstack10km)), format="%Y%m%d")

# Convert from wide (one col per date) to long format
sst.pts10km1 <- sst.pts10km %>%
  pivot_longer(cols = -station_name,
              names_to = "date",
              values_to = "value") %>%
  mutate(date = as.Date(gsub("SST_", "", date), format="%Y%m%d"))

sst.pts10km2 <- sst.pts10km1 %>%
  arrange(station_name, date) %>%
  group_by(station_name) %>%
  mutate(value_7d = zoo::rollapply(
      value, width = 7, FUN = mean,
      align = "center", fill = NA, na.rm = TRUE)) %>%
  ungroup()

sst.pts10km2 <- sst.pts10km1 %>%
  mutate(week_start = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(station_name, week_start) %>%
  mutate(value_7d = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE)) %>%
  ungroup()

# 4. Bilinear interpolation ----------------------------------------------

sst.bilinear <- extract(rstack10km, pts.UTM, method = "bilinear", ID = FALSE) %>%
  mutate(location = pts.UTM$location)

sst.bilinear_long <- sst.bilinear %>%
  setNames(c(paste0("SST_", format(layer_dates, "%Y%m%d")), "location")) %>%
  pivot_longer(cols = starts_with("SST_"),
               names_to = "date", values_to = "value_bl") %>%
  mutate(date = as.Date(gsub("SST_", "", date), format = "%Y%m%d"))


# fill using BL -----------------------------------------------------------

sst.pts10km3 <- left_join(sst.pts10km2,
                          sst.bilinear_long, by = c("location", "date")) %>%
  mutate(value_filled = ifelse(is.na(value), value_bl, value)) %>%
  group_by(location) %>% # Weekly gap fill (after bilinear)
  mutate(value_filled = fill_weekly_mean(date, value_filled),
    value_filled_7d = zoo::rollapply(value_filled,
                7, mean, fill = NA, align = "center", na.rm = TRUE)) %>%
  ungroup() # Then apply 7-day rolling average
  
# # 25 km resolution --------------------------------------------------------

0.09 * 2.5 #0.09 is 10km so we want 25km
0.225 / 0.02 #this value = the multiplication of base reso to 25km

rstack25km <- aggregate(rstack, fact = 11.25, #factor of whatever your resolution is in the OG raster / stack
                        fun = mean, na.rm = TRUE) #na.rm = T means it will interpolate into land
#this is aprox. 10km resolution now

sst.pts25km <- extract(rstack25km, pts.UTM, ID = F)
sst.pts25km$location <- pts.UTM$location

# fill values from 25km res into 10km df and make a new df
sst.pts <- fill_vals(sst.pts10km, sst.pts25km)

layer_dates <- as.Date(gsub("SST_", "", names(sst.pts)), format="%Y%m%d")

# Convert from wide (one col per date) to long format
sst.pts1 <- sst.pts %>%
  pivot_longer(cols = -location,
               names_to = "date",
               values_to = "value") %>%
  mutate(date = as.Date(gsub("SST_", "", date), format="%Y%m%d"))

sst.pts2 <- sst.pts1 %>%
  arrange(location, date) %>%
  group_by(location) %>%
  mutate(value_7d = zoo::rollapply(
    value, width = 7, FUN = mean,
    align = "center", fill = NA, na.rm = TRUE)) %>%
  ungroup()


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

