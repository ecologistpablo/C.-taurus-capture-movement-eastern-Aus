#step 7
  #31.07.2023
   #removing the small movements in our data set

rm(list=ls()) 
#listen to your supervisors - helpers:
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/230906_step5.csv") #5km

# begin wrestling --------------------------------------------------------------

dat1 <- dat %>% 
  arrange(Tag_ID, detection_datetime) %>%  # sort data by Tag_ID and detection_datetime
  group_by(Tag_ID) %>%  # group data by Tag_ID
  filter(n() >= 4) %>%  # remove groups with fewer than 4 rows
  slice(1:(n() %/% 4 * 4)) %>%  # keep the highest multiple of 4 rows for each group
    #(one Tag_ID can move multiple times)
  ungroup()  # remove grouping

table(dat1$Tag_ID)
#nice, we have a df with ONLY multiples of 4 for unique Tag_IDs (ie 2 movements in 4 rows)

#new column that numbers rows
dat1 <- dat1 %>%
  mutate(FRC = row_number())

#does row 2 connect to row 3 in their departure/arrival location?
  #(ie are the movements connected?)
evaluate_rows <- function(df) {
  df %>%
    group_by(Tag_ID) %>%
    mutate(keep_row = case_when(
      FRC %% 4 == 2 & Arrival_location == lead(Departure_location) ~ TRUE, #TRUE IF row 2 = matching Arrival_location
      FRC %% 4 == 3 & Departure_location == lag(Arrival_location) ~ TRUE, #TRUE IF row 3 = matching Departure_location
      TRUE ~ FALSE #if it aint true, its false
    )) %>%
    ungroup()
}

dat2 <- evaluate_rows(dat1)

table(dat2$keep_row)

#if false occurred as evenly as true, the sets of 4 would all match in rows 2 & 3
#this means not all groups of 4 rows are connected movements
#this means we have some data to cut

dat3 <- dat2 %>%
  # Create a new grouping variable that identifies each set of 4 rows
  mutate(group_of_4 = ceiling(FRC / 4)) %>%
  group_by(Tag_ID, group_of_4) %>%
  mutate(any_true = any(keep_row, na.rm = TRUE)) %>%
  ungroup() %>%
  # Filter the dataframe
  filter(any_true) %>%
  # Drop the helper columns
  dplyr::select(-group_of_4, -any_true, -keep_row)

#dat2 obs - dat3 obs
968 - 884
# Huzzah, 84
904-696
208

#Ok, now we have we now an orderly df with groups of 4 rows that match in time and Tag_ID
#But we need to add a temporal filter and a spatial filter

# Temporal filter --------------------------------------------------------------

#filter out jumping movements that are longer than
date_filter_rows <- function(df, temporal_threshold) {
  df <- df %>% 
    mutate(group_of_4 = ceiling(FRC / 4)) %>%  # Create groups of 4 rows
    group_by(group_of_4) %>%  # Group by the new groups of 4
    mutate(keep_row_date = ifelse(row_number() == 2 & lead(Arrival_date, 1) - Departure_date <= temporal_threshold, TRUE, FALSE)) %>%  # If the row is 2nd in a group, compare its Departure_date with the Arrival_date of the next row
    ungroup() %>%  # Ungroup to discard groupings by group_of_4
    group_by(group_of_4) %>%  # Group by the groups of 4 again
    filter(any(keep_row_date == TRUE)) %>%  # If in a group of 4, all rows have FALSE in keep_row_date, then remove the group
    ungroup()  # Ungroup to discard groupings by group_of_4
  return(df)
}


temporal_threshold <- 1 # day filter
dat4 <- date_filter_rows(dat3, temporal_threshold)
#manually inspect, is each group of 4 within 30 d from their beginning to end?
#nice, down to 52 obs with a 1 d threshold
unique(dat4$Location)

# spatial filter ---------------------------------------------------------------

# Function to filter rows based on a spatial_threshold
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

# Using the function
spatial_threshold <- 30 #the km threshold we want to use
dat5 <- spatial_filter_rows(dat4, spatial_threshold)
summary(dat5)
unique(dat5$Location)


dat5 <- dat5 %>% 
  dplyr::select(-FRC, -group_of_4, -keep_row_date, -keep_row_distance)

#filter foraging movements -----------------------------------------------------

OG <- read_csv("Inputs/230906_step5.csv")

columns <- c("Tag_ID", "station_name", "Location",
             "Arrival_date", "Departure_date",
             "Arrival_location", "Departure_location",
             "detection_datetime", "Num_days", "movement")

# perform the anti_join operation
OG1 <- anti_join(OG, dat5, by = columns)

#only took 4 - 5 days of rattling dplyr against my brain

#we now have x observations of groups of 4 rows, or movements to and from a site 
#The movement to AND from site (a) to (b) has to occur within 1 day
#and each direction has to be under 50km

#we can now save this df

#save it ----------------------------------------------------------------------------

write_csv(OG1, file = "Inputs/230906_step6.csv")
