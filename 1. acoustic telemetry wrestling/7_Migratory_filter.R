#step 7
  #31.07.2023
   #removing the small movements in our data set

# conceptual background on script -----------------------------------------

# my study wants to look at migratory movements only
# so we will filter small movements out of this
# sharks sometimes will leave a "location" only to return immediately
# this could be seen as a foraging movement or just a little swim outside and back 
# we want to remove these movements
# this script removes movements that leave, get detected outside a location within 50 km 
# only to return back to the beginning location within 24 h 
# I've written it into little functions so you can edit the temporal and spatial thresholds
# because we are open - source scientists who are cool like that 
# each movement is one row
# so we begin by targeting unique tags in multiples of 4 (departure, arrival, departure arrival)


# packages ----------------------------------------------------------------

library(tidyverse)
rm(list=ls()) 
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250827_step6.rds")

# pre-clean ---------------------------------------------------------------

# Build a summary table with arrival/departure locations and dates per movement_id
move_info <- dat %>%
  select(tag_id, movement_id, movement, location, datetime) %>%
  pivot_wider(names_from = movement,
    values_from = c(location, datetime),
    names_glue = "{.value}_{movement}") %>%
  rename(departure_location = location_departure, 
         arrival_location = location_arrival,
          departure_date = datetime_departure,
           arrival_date = datetime_arrival)

# Join back to the original data
dat <- dat %>%
  left_join(move_info, by = c("tag_id", "movement_id"))

# begin wrestling --------------------------------------------------------------

# a departure at A, arrival at B, departure at B and arrival at A = 4 movements
dat1 <- dat %>% 
 arrange(tag_id, datetime) %>%  # sort data by Tag_ID and detection_datetime
  group_by(tag_id) %>%  # group data by Tag_ID
  filter(n() >= 4) %>%  # remove groups with fewer than 4 rows
  dplyr::slice(1:(n() %/% 4 * 4)) %>%  # keep the highest multiple of 4 rows for each group
    #(one Tag_ID can move multiple times)
  ungroup()  # remove grouping

table(dat1$tag_id) # are they all groups of 4 ? If not, there's a problem

#nice, we have a df with ONLY multiples of 4 for unique tag IDs (ie 2 movements in 4 rows)

#new column that numbers rows
dat1 <- dat1 %>%
  mutate(FRC = row_number())

#does row 1 - 4 and 2 - 3 connect in their location name (is it location A - B - A)
  #(ie are the movements connected?)
evaluate_rows <- function(df) {
  df %>%
    group_by(tag_id) %>%
    mutate(keep_row = case_when(
      FRC %% 4 == 1 & departure_location == lag(arrival_location, default = first(arrival_location)) ~ TRUE, # Check if movement 1 departure matches the last arrival location (movement 4)
      FRC %% 4 == 2 & arrival_location == lead(departure_location) ~ TRUE, #TRUE IF row 2 = matching Arrival_location
      FRC %% 4 == 3 & departure_location == lag(arrival_location) ~ TRUE, #TRUE IF row 3 = matching Departure_location
      FRC %% 4 == 0 & arrival_location == lag(departure_location, default = first(departure_location)) ~ TRUE, # Check if movement 4 arrival matches the first departure location (movement 1)
      TRUE ~ FALSE #if it aint true, its false
    )) %>%
    ungroup()
}

dat2 <- evaluate_rows(dat1)

table(dat2$keep_row)

# if false occurred as evenly as true, the sets of 4 would all match in rows 2 & 3
# this means not all groups of 4 rows are connected movements
# this means we have some data to cut

dat3 <- dat2 %>%  # Create a new grouping variable that identifies each set of 4 rows
  mutate(group_of_4 = ceiling(FRC / 4)) %>%
  group_by(tag_id, group_of_4) %>%
  mutate(any_true = any(keep_row, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::filter(any_true) %>%   # Filter the dataframe
  dplyr::select(-group_of_4, -any_true, -keep_row) # rm the helper columns

# Ok, now we have we now an df that is ordered using tag id with movement 2 & 3 in the same place
# But we need to add a temporal filter and a spatial filter
# to find which rows do not meet our requirements
# and rm them accordingly

# Temporal filter --------------------------------------------------------------

date_filter_rows <- function(df, temporal_threshold) {
  df <- df %>% 
    mutate(group_of_4 = ceiling(FRC / 4)) %>%  # Create groups of 4 rows
    group_by(group_of_4) %>%  # Group by the new groups of 4
    mutate(keep_row_date = ifelse(row_number() == 2 & lead(arrival_date, 1) - departure_date <= temporal_threshold, TRUE, FALSE)) %>%  # If the row is 2nd in a group, compare its Departure_date with the Arrival_date of the next row
    ungroup() %>%  # Ungroup to discard groupings by group_of_4
    group_by(group_of_4) %>%  # Group by the groups of 4 again
    filter(any(keep_row_date == TRUE)) %>%  # If in a group of 4, all rows have FALSE in keep_row_date, then remove the group
    ungroup()  # Ungroup to discard groupings by group_of_4
  return(df)
}

temporal_threshold <- 1 # day filter, but you can change this as you wish
dat4 <- date_filter_rows(dat3, temporal_threshold)
#manually inspect, is each group of 4 within 1 d from their beginning to end?

# spatial filter ---------------------------------------------------------------

spatial_filter_rows <- function(df, spatial_threshold) {
  df <- df %>% 
    group_by(group_of_4) %>%  # Group by the existing groups of 4
    mutate(keep_row_distance = ifelse(distance > spatial_threshold, FALSE, TRUE)) %>%  # If the distance exceeds spatial_threshold in any row in a group, assign FALSE to 'keep_row_distance'
    ungroup() %>%  # Ungroup to discard groupings by group_of_4
    group_by(group_of_4) %>%  # Group by the groups of 4 again
    filter(all(keep_row_distance == TRUE)) %>%  # If in a group of 4, any row has FALSE in keep_row_distance, then remove the group
    ungroup()  # Ungroup to discard groupings by group_of_4
  return(df)
}

spatial_threshold <- 50 #the km threshold we want to use
dat5 <- spatial_filter_rows(dat4, spatial_threshold)
head(dat5)

# inspect -----------------------------------------------------------------

#let's see where the data we are removing occur
# Calculate the number of detections at each station
IMOSxy <- dat5 %>%
  group_by(location, latitude, longitude) %>% #location
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude", "latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = F) #colour by LOCATION

# cool, some just outside Flat Rock to Cherubs / henderson and back

dat5 <- dat5 %>% 
  dplyr::select(-FRC, -group_of_4, -keep_row_date, -keep_row_distance)

#filter foraging movements -----------------------------------------------------

OG <- read_rds("Inputs/250730_step8.rds")

columns <- c("tag_id","arrival_date", "departure_date", "departure_location",
             "num_days", "arrival_location", 
             "sex","latitude", "longitude","distance", "date" ) 

# perform the anti_join operation
OG1 <- anti_join(OG, dat5, by = columns)

#only took 4 - 5 days of rattling dplyr against my brain

# we now have x observations of groups of 4 rows, or movements to and from a site 
# The movement to AND from site (a) to (b) has to occur within 1 day
# and each direction has to be under 50km
# anti-join then removed those rows from our master data
# so we have no small-scale movements in our data now
# we can save this and move on, knowing there are no 'small' foraging movements

#save it ----------------------------------------------------------------------------

write_rds(OG1, file = "Inputs/250730_step7.rds")
