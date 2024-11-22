#23.06.23
  #connecting movement with detections

rm(list=ls())

source("/Users/owuss/Documents/USC/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
IMOS <- read_csv("Inputs/241116_step3.csv") #after receiver renaming but before VTrack
combined_data <- read_csv("Inputs/241122_step5.csv") #movements


# description -------------------------------------------------------------

#we now have a movement df and a df with raw detections
#we need to add metadata to our movement rows (sex, station, etc.)
#to do this we match movement rows with detection rows using the data we have

# pre-munging -------------------------------------------------------------

combined_data$Tag_ID <- as.character(combined_data$Tag_ID)
IMOS$Tag_ID <- as.character(IMOS$Tag_ID)

# Assign a unique ID to each row before the split
combined_data <- combined_data %>%
  mutate(original_id = row_number())

# find detections---------------------------------------------------------------

find_correlated_detections <- function(IMOS_data, movement_dates) {
  
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
             animal_sex == animal_sex) %>%
    mutate(movement = "Arrival", Movement_date = Arrival_date) # Add "movement" and "Movement_date" column
  
  print(paste0("Rows after joining and filtering Arrival_date: ", nrow(joined_data_arrival)))
  
  joined_data_departure <- IMOS_data %>% 
    left_join(movement_dates_filtered_departure, by = "Tag_ID") %>% 
    filter(detection_datetime == Departure_date &
             Location == Departure_location &
             animal_sex == animal_sex) %>%
    mutate(movement = "Departure", Movement_date = Departure_date) # Add "movement" and "Movement_date" column
  
  print(paste0("Rows after joining and filtering Departure_date: ", nrow(joined_data_departure)))
  
  # Combine dfs
  joined_data <- bind_rows(joined_data_arrival, joined_data_departure)
  
  return(joined_data)
}

# run --------------------------------------------------------------------------

fdat <- find_correlated_detections(IMOS, combined_data)


# lets check our function did what we wanted it to do
# any rows omitted that shouldn't have?

fdat1 <- fdat %>% 
  distinct(Tag_ID, detection_datetime, Arrival_date, Departure_date, Location,
           Arrival_location, Departure_location, Num_days, animal_sex,
           .keep_all = T) #in-case the function joined duplicate rows, remove 

fdat2 <- fdat1 %>%
  arrange(original_id) #order them 

# Create the expected vector with each number twice
expected_ids <- rep(1:653, each = 2)

# Find which numbers are missing from fdat2$orginal_id
missing_ids <- expected_ids[!expected_ids %in% fdat2$original_id]

# Print the missing IDs
print(missing_ids)

# row 33 was missing, we don't even need that one its deg_34 - deg_35! 
# you should manually inspect each function output to ensure R is playing friendly
# cause it doesn't (a lot)...

# distance metric --------------------------------------------------------------

# let's calculate how far each movement was in kilometers using the harvesine method
# this accounts for earth being a sphere, so it's a little better than direct dist
calculate_distance <- function(df) {
  df <- df %>%
    group_by(Tag_ID, Arrival_location, Arrival_date, Departure_date) %>%
    mutate(
      # Use distHaversine function to calculate distance between two points
      distance = distHaversine(
        matrix(c(receiver_deployment_longitude[1], receiver_deployment_latitude[1]), nrow = 1),
        matrix(c(receiver_deployment_longitude[2], receiver_deployment_latitude[2]), nrow = 1)
      ) / 1000 # Convert meters to kilometers
    ) %>%
    mutate(
      distance = round(distance, 0) # Round to 0 decimal places
    )
  return(df)
}
# run the function
fdat3 <- calculate_distance(fdat2)

summary(fdat3$distance)
anyNA(fdat3$distance) #should be no NAs

# Filter dat movements that go to and from just a degree -----------------------

fdat4 <- fdat3 %>% # we only are interested in focal locations
  filter(!(startsWith(Departure_location, "deg_") & #but this is methods specific
             startsWith(Arrival_location, "deg_")))

#save it -----------------------------------------------------------------------

write_csv(fdat4, file = "Inputs/241122_step6.csv")
