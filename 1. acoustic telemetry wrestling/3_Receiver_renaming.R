#11.07.23
  #step 3
    # categorizing what categorises a 'focal location' in our analysis
  
rm(list=ls()) # to clear workspace

# load library & data ----------------------------------------------------------

setwd("~/Documents/USC/Honours/R/data")
IMOS <- read_csv("Inputs/241116_step2.csv")

# conceptualisation of script ---------------------------------------------

# we have raw detections
# we will turn this into movements from different places
# lets group our aggregations, curtains and bays into one location
# this will allow us to detect movement between locations that hold large amounts of data

# begin with an interactive plot, where are our data? ---------------------

# Calculate the number of detections at each station: station_name
IMOSxy <- IMOS %>%
  group_by(station_name, receiver_deployment_latitude, receiver_deployment_longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("receiver_deployment_longitude",
                                             "receiver_deployment_latitude"), crs= 4326, agr = "constant")

# Interactive map
mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "station_name", fbg = F)

# Location function ------------------------------------------------------------

# now that we have the central receivers (receivers with most detections)
# and receivers at beginning of curtains, we will turn this central receivers into locations

#create location function
add_location_group <- function(df, central_station_name, location_name) {
  # Get latitude of the central station
  central_lat <- df %>%
    filter(station_name == central_station_name) %>%
    pull(receiver_deployment_latitude) %>%
    first()
  
  # If Location column doesn't exist, create it
  if (!"Location" %in% colnames(df)) {
    df <- df %>%
      mutate(Location = station_name)
  }
  
  # Update Location column
  df <- df %>%
    mutate(Location = case_when(
      (is.na(Location) | Location == station_name) &
        between(receiver_deployment_latitude, 
                central_lat - 0.045, # 0.09 degrees * 111 km/degree = 10 km So 0.045  = 5km
                central_lat + 0.045) ~ location_name,
      TRUE ~ Location # Default case, keep existing values
    ))
  
  return(df) # Return the modified data frame
}

# adjust central lat / lon for how broad you want to generate your locations
# we played with 5 / 10 km either side
# if you group aggregation sites together that are spatially close, they may not be ecologically similar
# we don't know yet, so I targeted specific sites

# run --------------------------------------------------------------------------

#to select receivers as the 'central receiver', we chose receivers that held the most detections in a known aggregation site
#or the base of IMOS curtains that sites along the coastline

IMOS <- add_location_group(IMOS,
                                  "ST02 WR", # station name
                                  "Wolf Rock") # new Location name

IMOS$station_name <- ifelse(IMOS$station_name == "Flat Rock","FR",IMOS$station_name)
IMOS <- add_location_group(IMOS,
                                  "FtR TC M1 2022/2023 101919", # station name
                                  "Flat Rock") # new Location name

IMOS <- add_location_group(IMOS,
                                  "CHS 13", # station name
                                  "Coffs Harbour") # new Location name
IMOS <- add_location_group(IMOS,
                                  "MB-5", # station name
                                  "Hawks Nest") # new Location name

IMOS <- add_location_group(IMOS,
                                  "BL 1", # station name
                                  "Sydney") # new Location name
IMOS$Location <- ifelse(IMOS$Location == "BL 15","Sydney",IMOS$Location)

# filtering to degrees for other sites
IMOS1 <- IMOS %>% #all other receivers shall be named after the degree they are in 
  mutate(Location = ifelse(station_name == Location,
                           paste("deg_", floor(receiver_deployment_latitude), sep = ""),
                           Location))

#did it work? -----------------------------------------------------------------

# Calculate the number of detections at each station
IMOSxy <- IMOS1 %>%
  group_by(Location, receiver_deployment_latitude, receiver_deployment_longitude) %>% #location
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("receiver_deployment_longitude", "receiver_deployment_latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "Location", fbg = F) #colour by LOCATION



# Unique detections -------------------------------------------------------

# now that we have 'locations' instead of receivers, we need to filter again
# because we only want 1 detection per tag, per 'location', per day
# to simplify the analysis to long-term patterns

IMOS2 <- IMOS1 %>% 
  distinct(Location, Tag_ID, detection_datetime, .keep_all = T)

#that's better!

# save it ----------------------------------------------------------------------

write_csv(IMOS2, "Inputs/250211_step3.csv")

# results for detections --------------------------------------------------
# this code is for the results section of the paper

# Calculating min, max datetime and duration for each Tag ID
duration_data <- IMOS %>%
  group_by(Tag_ID) %>%
  summarise(
    min_datetime = min(detection_datetime, na.rm = TRUE),
    max_datetime = max(detection_datetime, na.rm = TRUE),
    duration = max_datetime - min_datetime
  ) %>%
  ungroup()

write_csv(duration_data, "Outputs/250211_tag_duration_data.csv")

# Calculating mean and standard deviation of durations
mean_duration <- mean(duration_data$duration, na.rm = TRUE)
sd_duration <- sd(duration_data$duration, na.rm = TRUE)

# Print results
print(duration_data)
print(paste("Mean duration: ", mean_duration))
print(paste("SD of duration: ", sd_duration))

