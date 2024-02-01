#23.06.23
  #connecting movement with detections

rm(list=ls())
#listen to your supervisors - helpers:
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

setwd("~/University/2023/Honours/R/data") 
IMOS <- read_csv("Inputs/240114_step3.csv") #after receiver renaming but before VTrack
combined_data <- read_csv("Inputs/240114_step5.csv") #movements



#we now have a movement df and a df with raw detections
#we need to add metadata to our movement rows
#to do this we match movement rows with detection rows using the data we have

# pre-munging -------------------------------------------------------------

combined_data$Tag_ID <- as.character(combined_data$Tag_ID)

# Assign a unique ID to each row before the split
combined_data <- combined_data %>%
  mutate(original_id = row_number())


# find detections---------------------------------------------------------------

find_correlated_detections <- function(IMOS_data, movement_dates) {
  
  # Rename the Tag_ID column in IMOS_data to match that in movement_dates
  IMOS_data <- IMOS_data %>% 
    rename(Tag_ID = tag_id)
  
  print(paste0("Initial row count: ", nrow(movement_dates)))
  
  # Split the movement df by arrivals / departures
  movement_dates_filtered_arrival <- movement_dates %>% 
    filter(!is.na(Arrival_date))
  
  print(paste0("Rows after filtering Arrival_date: ", nrow(movement_dates_filtered_arrival)))
  
  movement_dates_filtered_departure <- movement_dates %>% 
    filter(!is.na(Departure_date))
  
  print(paste0("Rows after filtering Departure_date: ", nrow(movement_dates_filtered_departure)))
  
  # Join either Arrival or Departure date based on movement type
  joined_data_arrival <- IMOS_data %>% 
    left_join(movement_dates_filtered_arrival, by = "Tag_ID") %>% 
    filter(detection_datetime == Arrival_date &
             Location == Arrival_location & 
             animal_sex == Sex) %>%
    mutate(movement = "Arrival", Movement_date = Arrival_date) # Add "movement" and "Movement_date" column
  
  print(paste0("Rows after joining and filtering Arrival_date: ", nrow(joined_data_arrival)))
  
  joined_data_departure <- IMOS_data %>% 
    left_join(movement_dates_filtered_departure, by = "Tag_ID") %>% 
    filter(detection_datetime == Departure_date &
             Location == Departure_location &
             animal_sex == Sex) %>%
    mutate(movement = "Departure", Movement_date = Departure_date) # Add "movement" and "Movement_date" column
  
  print(paste0("Rows after joining and filtering Departure_date: ", nrow(joined_data_departure)))
  
  # Combine dfs
  joined_data <- bind_rows(joined_data_arrival, joined_data_departure)
  
  return(joined_data)
}

# run --------------------------------------------------------------------------

fdat <- find_correlated_detections(IMOS, combined_data)


# more munging ------------------------------------------------------------

fdat1 <- fdat %>% 
  distinct(Tag_ID, detection_datetime, Arrival_date, Departure_date, Location,
           Arrival_location, Departure_location, Num_days, Sex,
           .keep_all = T) #in-case the function joined duplicate rows, remove 

fdat2 <- fdat1 %>%
  arrange(original_id)

# Create the expected vector with each number twice
expected_ids <- rep(1:641, each = 2)

# Find which numbers are missing from fdat2$orginal_id
missing_ids <- expected_ids[!expected_ids %in% fdat2$original_id]

# Print the missing IDs
print(missing_ids)

#row 33 was missing, we don't even need that one its deg_34 - deg_35! 

# distance metric --------------------------------------------------------------

calculate_distance <- function(df) {
  df <- df %>%
    group_by(Tag_ID, Arrival_location, Arrival_date, Departure_date) %>%
    mutate(distance = c(cbind(receiver_deployment_longitude[1], receiver_deployment_latitude[1]),
                                    cbind(receiver_deployment_longitude[2], receiver_deployment_latitude[2])) / 1000)
  
  return(df)
}

# run the function
fdat2 <- calculate_distance(fdat1)

summary(fdat2$distance)
anyNA(fdat2$distance) #should be no NAs

# Filter dat movements that go to and from just a degree -----------------------

fdat2 <- fdat2 %>%
  filter(!(startsWith(Departure_location, "deg_") &
             startsWith(Arrival_location, "deg_")))

#save it -----------------------------------------------------------------------

write_csv(fdat2, file = "Inputs/230906_step5.csv") #5km
