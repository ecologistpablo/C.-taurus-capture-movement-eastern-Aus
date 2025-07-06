#23.06.23
  #connecting movement with detections

rm(list=ls())

pacman::p_load("tidyverse", "purrr")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
IMOS <- read_csv("Inputs/250705_step3.csv") #after receiver renaming but before VTrack
combined_data <- read_csv("Inputs/250705_step5.csv") #movements

IMOS1 <- IMOS %>%
  distinct(detection_datetime, Tag_ID, animal_sex, Location,
           station_name, receiver_deployment_latitude, receiver_deployment_longitude)

# description -------------------------------------------------------------

# we now have a movement df and a df with raw detections
# we need to add metadata to our movement rows (sex, station, etc.)
# to do this we match movement rows with detection rows using matching rows
# our data also has departures and arrivals in one row
# we need to separate them, they will be individual data

# pre-munging -------------------------------------------------------------

# Assign a unique ID to each row before the split
combined_data <- combined_data %>%
  mutate(original_id = row_number())

# find detections---------------------------------------------------------------

# find detections in 'raw data' and in movement data columns, and merge them
split_cols <- function(IMOS_data, movement_dates) {
  print(paste0("Initial row count: ", nrow(movement_dates)))
  
  # Convert necessary columns to character for matching
  IMOS_data <- IMOS_data %>%
    mutate(Tag_ID = as.character(Tag_ID),
           Location = as.character(Location))
  
  movement_dates <- movement_dates %>%
    mutate(Tag_ID = as.character(Tag_ID),
           Arrival_location = as.character(Arrival_location),
           Departure_location = as.character(Departure_location))
  
  # Split into arrival and departure dataframes
  arrival_data <- movement_dates %>%
    dplyr::select(Tag_ID, Arrival_date, Arrival_location, everything()) %>%
    mutate(movement_type = "Arrival") %>%
    rename(Location = Arrival_location)  # Keep Arrival_location as Location
  
  departure_data <- movement_dates %>%
    dplyr::select(Tag_ID, Departure_date, Departure_location, everything()) %>%
    mutate(movement_type = "Departure") %>%
    rename(Location = Departure_location)  # Keep Departure_location as Location
  
  # Combine arrival and departure data
  combined_data <- bind_rows(arrival_data, departure_data)
  
  # Add Sex information
  combined_data <- combined_data %>%
    mutate(Sex = ifelse(Tag_ID %in% IMOS_data$Tag_ID, IMOS_data$animal_sex[match(Tag_ID, IMOS_data$Tag_ID)], NA))
  
  # Arrange by original_id
  combined_data <- combined_data %>%
    arrange(original_id)
  
  # Print the final row count
  print(paste0("Rows after adding metadata: ", nrow(combined_data)))
  
  # Return the final combined data
  return(combined_data)
}

# run --------------------------------------------------------------------------

fdat <- split_cols(IMOS, combined_data)

#save it -----------------------------------------------------------------------

write_csv(fdat, file = "Inputs/250705_step6.csv")
