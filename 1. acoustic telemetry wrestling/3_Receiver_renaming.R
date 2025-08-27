#11.07.23
  #step 3
    # categorizing what categorises a 'focal location' in our analysis
  
rm(list=ls()) # to clear workspace

# load library & data ----------------------------------------------------------

pacman::p_load("tidyverse")
setwd("~/Documents/USC/Honours/R/data")
IMOS <- read_csv("Inputs/250723_step2.csv") 

IMOS <- IMOS %>% 
  filter(!format(datetime, "%Y") %in% c("2025"))

summary(IMOS$datetime)

# conceptualisation of script ---------------------------------------------

# we have raw detections
# we will turn this into movements from different places
# lets group our aggregations, curtains and bays into one location
# this will allow us to detect movement between locations that hold large amounts of data

# china repairing ---------------------------------------------------------

IMOS <- IMOS %>%
  mutate(latitude = replace(latitude, 
                            station_name == 'China Wall', -27.10886),
         longitude = replace(longitude, 
                             station_name == 'China Wall', 153.50100))

# begin with an interactive plot, where are our data? ---------------------

IMOS$station_name <- ifelse(IMOS$station_name == "Flat Rock","FR",IMOS$station_name)
IMOS$station_name <- ifelse(IMOS$station_name == "Hawks Nest","Hawks Nest1",IMOS$station_name)
IMOS$station_name <- ifelse(IMOS$station_name == "Coffs Harbour","CH",IMOS$station_name) 
IMOS$station_name <- ifelse(IMOS$station_name == "Ballina","Ballina1",IMOS$station_name) 
IMOS$station_name <- ifelse(IMOS$station_name == "Forster", "Forster1", IMOS$station_name)
IMOS$station_name <- ifelse(IMOS$station_name == "Newcastle", "Newcastle1", IMOS$station_name)
IMOS$station_name <- ifelse(IMOS$station_name == "Yamba", "Yamba1", IMOS$station_name)
IMOS$station_name <- ifelse(IMOS$station_name == "Merimbula", "Merimbula1", IMOS$station_name)
IMOS$station_name <- ifelse(IMOS$station_name == "Evans Head", "Evans Head1", IMOS$station_name)

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
                                  "Wide Bay") # new Location name

IMOS <- add_location_group(IMOS, "Wobbie Rock", # Station name
                           "Sunshine Coast") # new location name

IMOS <- add_location_group(IMOS,"FtR TC M1 2022/2023 101919", # station name
                                  "North Stradbroke Island") # new Location name

IMOS <- add_location_group(IMOS, "Cherubs Cave", "Moreton Island")

IMOS <- add_location_group(IMOS, "FAD 12", "Gold Coast")

IMOS <- add_location_group(IMOS,"Lennox Point", "Ballina") 
IMOS <- add_location_group(IMOS,"Evans Head1", "Evans Head") 
IMOS <- add_location_group(IMOS,"Yamba1", "Yamba") 
IMOS <- add_location_group(IMOS, "CHS 13", "Coffs Harbour")

IMOS <- add_location_group(IMOS, "Cod Grounds", "Port Macquarie")

IMOS <- add_location_group(IMOS, "Forster1", "Forster")

IMOS <- add_location_group(IMOS,"MB-5", "Hawks Nest")

IMOS <- add_location_group(IMOS,"BiB 02.1", "Hawks Nest")

IMOS <- add_location_group(IMOS,"Avoca", "Central Coast")

IMOS <- add_location_group(IMOS,"BL 1", "Sydney") 

IMOS <- add_location_group(IMOS, "Wollongong OAR", "Illawarra")
IMOS <- add_location_group(IMOS,"Merimbula1", "Merimbula") 

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

write_rds(IMOS1, "Inputs/250827_step3.rds")

table(IMOS1$location)


