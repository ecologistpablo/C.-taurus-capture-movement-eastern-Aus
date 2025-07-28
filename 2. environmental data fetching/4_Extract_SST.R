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

pacman::p_load("terra", "sf", "sp", "viridis", "tidyverse")

# pts ---------------------------------------------------------------------

setwd("~/Documents/USC/Honours/R/data")
rcs <- read_csv("Inputs/250728_step9_coordinates.csv") #this should be a csv with your XY coordinates for your receivers
WGS84 <- terra::crs("EPSG:4326")# Coordinate reference systems
head(rcs) #its all there
unique(rcs$station_name) # should be the same size as your df

# data prep ---------------------------------------------------------------

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

rstack <- rast("IMOS/SST/GHRSST_12-24.tif") #our enviro data

# plotting ----------------------------------------------------------------

plot(rstack[[19]], col = viridis(255)) #did it work as we hoped for?
plot(pts.UTM, add = T) #our do our points plot ontop of our enviro data? 

# increase coarseness -----------------------------------------------------

rstack #res at 0.02 by 0.02, 2km x 2km
0.09 / 0.02 #0.02 is our res, but we want 0.09 (10km)
rstack10km <- aggregate(rstack, fact = 4.5, #factor of whatever your resolution is in the OG raster / stack
                        fun = mean, na.rm = TRUE) #na.rm = T means it will interpolate into land
#this is 10km resolution now
plot(rstack10km[[19]], col = viridis(255)) # plot to see how it changed
# interesting what changing resolution does, hey ?
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

#weekly average
sst.pts10km2 <- sst.pts10km1 %>%
  mutate(week_start = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(station_name, week_start) %>%
  mutate(value_7d = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE)) %>%
  ungroup()

# let's find out how many stations are full of NAs
# our data is coastal, so it sometimes happens that environmental data
# does not land on the coordinates close to land

# calculate number of stations with only NAs
sst.pts10km2 %>%
  group_by(station_name) %>%
   summarise(total_rows = n(),
    num_na = sum(is.na(value_7d)),
    all_na = all(is.na(value_7d)),
    prop_na = mean(is.na(value_7d))) %>%
   arrange(desc(all_na), desc(prop_na)) %>% print(n = 258)

# 15 / 253, great : ) 

# bi-linear interpolation -------------------------------------------------

# Extract bilinear-interpolated SST
bl <- extract(rstack10km, pts.UTM, method = "bilinear", ID = FALSE)
bl$station_name <- pts.UTM$station_name

# Long format conversion
bl1 <- bl %>%
  setNames(c(as.character(layer_dates), "station_name")) %>%
  pivot_longer(cols = -station_name,
               names_to = "date",
               values_to = "value") %>%
  mutate(date = as.Date(date))

bl2 <- bl1 %>% # averaging across weeks
  mutate(week_start = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(station_name, week_start) %>%
  mutate(bl_value_7d = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE)) %>%
  ungroup()

# join both ---------------------------------------------------------------

sst.pts10km3 <- sst.pts10km2 %>%
  left_join(bl2 %>% select(station_name, date, bl_value_7d),
            by = c("station_name", "date")) %>%
  mutate(value_7d_filled = if_else(is.na(value_7d), bl_value_7d, value_7d)) %>%
  select(station_name, date, value, value_7d, bl_value_7d, value_7d_filled)

# how many NAs are still around

sst.pts10km3 %>%
  group_by(station_name) %>%
  summarise(total_rows = n(),
            num_na = sum(is.na(value_7d_filled)),
            all_na = all(is.na(value_7d_filled)),
            prop_na = mean(is.na(value_7d_filled))) %>%
  arrange(desc(all_na), desc(prop_na)) %>% print(n = 237)

# 0 stations, out of 258 stations, are completely NA

# clean - up --------------------------------------------------------------

sst.pts10km4 <- sst.pts10km3 %>% 
  distinct(station_name, date, value_7d_filled) %>% 
  rename(SST = value_7d_filled)

# summary -----------------------------------------------------------------

# so we extracted data at a 10km resolution
# then averaged it across 7 days
# then extracted data using bilinear interpolation for receivers w 100% NA
# averaged it across 7 days also
# and filled in original df

# save --------------------------------------------------------------------

write_csv(sst.pts10km4, file = "Inputs/250728_SST_vals_12-24.csv")

