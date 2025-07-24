
rm(list=ls()) 
setwd("~/Documents/USC/Honours/R/data")
pacman::p_load("tidyverse", "VTrack", "lubridate", 'tictoc')
dat <- read.csv("Inputs/250724_step3.csv")

dat1 <- dat %>% 
  mutate(datetime = with_tz(ymd_hms(datetime, tz = "UTC"), tzone = "Etc/GMT-10")) %>% 
  select(-receiver_name) %>% 
  filter(if_all(everything(), ~ !is.na(.)))


# Sort and generate lead columns
move_base <- dat1 %>%
  arrange(tag_id, datetime) %>%
  group_by(tag_id) %>%
  mutate(next_location = lead(location),
    next_time = lead(datetime),
    next_station_name = lead(location),
    next_latitude = lead(latitude),
    next_longitude = lead(longitude)) %>%
  # Define a movement as a change in location
  filter(!is.na(next_location), location != next_location) %>%
  mutate(movement_id = row_number()) %>%
  ungroup()

# Departure rows
departures <- move_base %>%
  transmute(movement_id,
    tag_id,
    movement = "departure",
    datetime = datetime,
    location = location,
    station_name = station_name,
    latitude = latitude,
    longitude = longitude,
    sex = sex)

# Arrival rows
arrivals <- move_base %>%
  transmute(movement_id,
    tag_id,
    movement = "arrival",
    datetime = next_time,
    location = next_location,
    station_name = next_station_name,
    latitude = next_latitude,
    longitude = next_longitude,
    sex = sex)

# Combine
dat2 <- bind_rows(departures, arrivals) %>%
  arrange(tag_id, movement_id, datetime)

# the size of dat2 should be double of movebase

write_csv(residency_events, "Inputs/250724_residency.csv")
