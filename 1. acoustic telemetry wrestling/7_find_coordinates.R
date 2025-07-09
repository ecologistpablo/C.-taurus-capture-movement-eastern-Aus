#26 June 2025
  # pablo fuenzalida
    # cleaning scripts and workflow

rm(list=ls())

library(tidyverse)

setwd("/Users/owuss/Documents/USC/Honours/R/data")
raw <- read_csv("Inputs/250706_step3.csv") #after receiver renaming but before VTrack
dat <- read_csv("Inputs/250706_step6.csv") #movements


# get munging -------------------------------------------------------------

raw1 <- raw %>%
  distinct(detection_datetime, Tag_ID, animal_sex, Location,
           station_name, receiver_deployment_latitude, receiver_deployment_longitude)

dat1 <- dat %>%
  arrange(original_id, movement_type) %>%
  group_by(original_id) %>%
  mutate(Arrival_location = if_else(is.na(Arrival_location),
                                    lag(Arrival_location), Arrival_location),
         Departure_location = if_else(is.na(Departure_location),
                                      lag(Departure_location), Departure_location)) %>%
  ungroup()

head(dat1)

dat2 <- dat1 %>%
  arrange(original_id, movement_type) %>%
  group_by(original_id) %>%
  mutate(Arrival_location = if_else(is.na(Arrival_location),
                                    lead(Arrival_location), Arrival_location),
         Departure_location = if_else(is.na(Departure_location),
                                      lead(Departure_location), Departure_location)) %>%
  ungroup()

head(dat2)

# Filter dat movements that go to and from just a degree -----------------------

dat3 <- dat2 %>% # we only are interested in focal locations
  filter(!(startsWith(Departure_location, "deg_") & #but this is methods specific
             startsWith(Arrival_location, "deg_"))) %>%
  mutate(date = if_else(movement_type == "Arrival", Arrival_date, Departure_date))


dat4 <- janitor::clean_names(dat3)

# merge -------------------------------------------------------------------

# Ensure consistent column names and classes
raw2 <- raw1 %>%
  rename(tag_id = Tag_ID,
    location = Location) %>%
  mutate(date = as.Date(detection_datetime)) %>% 
  select(tag_id, location, date, station_name,
       receiver_deployment_latitude, receiver_deployment_longitude) %>%
  distinct() %>%  # remove exact duplicates
  group_by(tag_id, location, date) %>%
  slice(1) %>%  # keep only the first match
  ungroup()

dat5 <- dat4 %>%
  mutate(date = as.Date(date))  # ensure same date format for join

# Join by tag_id, location, and date
dat6 <- dat5 %>%
  left_join(raw2 %>%
      select(tag_id, location, date, station_name,
             receiver_deployment_latitude, receiver_deployment_longitude),
    by = c("tag_id", "location", "date"))


# re-tidy -----------------------------------------------------------------

dat7 <- dat6 %>%
  rename(latitude = receiver_deployment_latitude,
         longitude = receiver_deployment_longitude) %>%
  select(tag_id, sex, date, location, station_name, latitude,
         longitude, num_days, movement_type,
         arrival_location, departure_location, arrival_date, departure_date, original_id)


#that's better!
dat7 %>% 
  filter(location == "Ballina") %>% 
  count()

# plot it -----------------------------------------------------------------

# check did we get all our xy coords into our movement df? 
IMOSxy <-  dat7 %>%
  group_by(location, latitude, longitude) %>% #location
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude",
                                             "latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = F) #colour by LOCATION


#save it -----------------------------------------------------------------------

write_csv(dat7, file = "Inputs/250706_step7.csv")
