#23.06.23
  #connecting movement with detections

rm(list=ls())

source("/Users/owuss/Documents/USC/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
IMOS <- read_csv("Inputs/250301_step3.csv") #after receiver renaming but before VTrack
combined_data <- read_csv("Inputs/250301_step5.csv") #movements

# description -------------------------------------------------------------

#we now have a movement df and a df with raw detections
#we need to add metadata to our movement rows (sex, station, etc.)
#to do this we match movement rows with detection rows using the data we have

# pre-munging -------------------------------------------------------------

# Assign a unique ID to each row before the split
combined_data <- combined_data %>%
  mutate(original_id = row_number())

str(IMOS)
str(combined_data)

# find detections---------------------------------------------------------------

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
  
  # Add latitude and longitude based on Location
  combined_data <- combined_data %>%
    mutate(latitude = ifelse(Location %in% IMOS_data$Location, 
                             IMOS_data$receiver_deployment_latitude[match(Location, IMOS_data$Location)], NA),
           longitude = ifelse(Location %in% IMOS_data$Location, 
                              IMOS_data$receiver_deployment_longitude[match(Location, IMOS_data$Location)], NA))
  
  # Remove the Location column
  combined_data <- combined_data %>%
    dplyr::select(-Location)
  
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


# distance metric --------------------------------------------------------------

# let's calculate how far each movement was in kilometers using the harvesine method
# this accounts for earth being a sphere, so it's a little better than direct dist
calculate_distance <- function(df) {
  df <- df %>%
    arrange(Tag_ID, original_id) %>%
    group_by(Tag_ID, original_id) %>%
    mutate(
      # Calculate the distance between two rows with the same original_id
      distance = geosphere::distHaversine(
        c(longitude[1], latitude[1]),
        c(lead(longitude)[1], lead(latitude)[1])
      ) / 1000 # Convert meters to km
    ) %>%
    # Ensure the distance is applied to both rows and round to 2 decimal places
    mutate(
      distance = if_else(!is.na(distance), round(distance, 2), NA_real_)
    ) %>%
    ungroup()
  
  return(df)
}


# run the function
fdat1 <- calculate_distance(fdat)


fdat2 <- fdat1 %>%
  arrange(original_id, movement_type) %>%
  group_by(original_id) %>%
  mutate(
    Arrival_location = if_else(is.na(Arrival_location), lag(Arrival_location), Arrival_location),
    Departure_location = if_else(is.na(Departure_location), lag(Departure_location), Departure_location)
  ) %>%
  ungroup()

head(fdat2)

fdat3 <- fdat2 %>%
  arrange(original_id, movement_type) %>%
  group_by(original_id) %>%
  mutate(
    Arrival_location = if_else(is.na(Arrival_location), lead(Arrival_location), Arrival_location),
    Departure_location = if_else(is.na(Departure_location), lead(Departure_location), Departure_location)
  ) %>%
  ungroup()

head(fdat3)

# Filter dat movements that go to and from just a degree -----------------------

fdat4 <- fdat3 %>% # we only are interested in focal locations
  filter(!(startsWith(Departure_location, "deg_") & #but this is methods specific
             startsWith(Arrival_location, "deg_")))

fdat5 <- fdat4 %>%
  mutate(
    date = if_else(movement_type == "Arrival", Arrival_date, Departure_date)
  )


fdat6 <- janitor::clean_names(fdat5)

fdat7 <- fdat6[,-7] #don't need original_id anymore

#save it -----------------------------------------------------------------------

write_csv(fdat7, file = "Inputs/250301_step6.csv")
