#11.07.23
  #step 3
    # categorizing what categorises a 'focal location' in our analysis
  
rm(list=ls()) # to clear workspace

# load library & data ----------------------------------------------------------

pacman::p_load("tidyverse")
setwd("~/Documents/USC/Honours/R/data")
IMOS <- read_csv("Inputs/250708_step2.csv")

# conceptualisation of script ---------------------------------------------

# we have raw detections
# we will turn this into movements from different places
# lets group our aggregations, curtains and bays into one location
# this will allow us to detect movement between locations that hold large amounts of data

# begin with an interactive plot, where are our data? ---------------------

IMOS$station_name <- ifelse(IMOS$station_name == "Flat Rock","FR",IMOS$station_name)
IMOS$station_name <- ifelse(IMOS$station_name == "Hawks Nest","Hawks Nest1",IMOS$station_name)
IMOS$station_name <- ifelse(IMOS$station_name == "Coffs Harbour","CH",IMOS$station_name) #it doesn't work well when they're the same name
IMOS$station_name <- ifelse(IMOS$station_name == "Ballina","Ballina1",IMOS$station_name) #it doesn't work well when they're the same name

# the function below doesn't deal well 
# when you re-name a location to the same name as the station
# so change names to something different pls

# Calculate the number of detections at each station: station_name
IMOSxy <- IMOS %>%
  group_by(station_name, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude","latitude"),
                          crs= 4326, agr = "constant")

# Interactive map
mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "station_name", fbg = F)

# Location function ------------------------------------------------------------

# now that we have the central receivers (receivers with most detections)
# and receivers at beginning of curtains, we will turn this central receivers into locations

#create location function
add_location_group <- function(df, central_station_name, location_name) {
  central_lat <- df %>%  # Get latitude of the central station
    filter(station_name == central_station_name) %>%
    pull(latitude) %>%
    first()
  if (!"location" %in% colnames(df)) {  # If Location column doesn't exist, create it
    df <- df %>%
      mutate(location = station_name)
  }
  df <- df %>%  # Update Location column
    mutate(location = case_when(
      (is.na(location) | location == station_name) &
        between(latitude, 
                central_lat - 0.2478, # 0.2478 * 111 / 1 = 27.5 km each way
                central_lat + 0.2478) ~ location_name, # 55km total  lat boundary
      TRUE ~ location # Default case, keep existing values
    ))
  return(df) # Return the modified data frame
}

# adjust central lat / lon for how broad you want to generate your locations
# if you group aggregation sites together that are spatially close, they may not be ecologically similar
# we don't know yet, so I targeted specific sites

# run --------------------------------------------------------------------------

IMOS <- add_location_group(IMOS,"ST02 WR", # station name
                                  "Wolf Rock") # new Location name

IMOS <- add_location_group(IMOS, "Wobbie Rock",
                                  "Sunshine Coast")

IMOS <- add_location_group(IMOS,"FtR TC M1 2022/2023 101919", # station name
                                  "Flat Rock") # new Location name

IMOS <- add_location_group(IMOS, "FAD 12",
                                  "Gold Coast")

IMOS <- add_location_group(IMOS,"Lennox Point", #station name
                                  "Ballina") #new location name
IMOS <- add_location_group(IMOS,
                                  "CHS 13", # station name
                                  "Coffs Harbour") # new Location name

IMOS <- add_location_group(IMOS,
                                  "MB-5", # station name
                                  "Hawks Nest") # new Location name

IMOS <- add_location_group(IMOS,"BL 1", # station name
                                  "Sydney") # new Location name

# filtering to degrees for other sites
IMOS1 <- IMOS %>% #all other receivers shall be named after the degree they are in 
  mutate(location = ifelse(station_name == location,
                           paste("deg_", floor(latitude), sep = ""),
                           location))

#did it work? -----------------------------------------------------------------

# Calculate the number of detections at each station
IMOSxy <- IMOS1 %>%
  group_by(location, latitude, longitude) %>% #location
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude", "latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = F) #colour by LOCATION

# save it ----------------------------------------------------------------------

write_csv(IMOS1, "Inputs/250708_step3.csv")


