#26 June 2025
  # pablo fuenzalida
    # cleaning scripts and workflow

rm(list=ls())

library(tidyverse)

setwd("/Users/owuss/Documents/USC/Honours/R/data")
raw <- read_csv("Inputs/250708_step3.csv") #after receiver renaming but before VTrack
dat <- read_csv("Inputs/250709_step6.csv") #movements


# get munging -------------------------------------------------------------

raw1 <- raw %>%
  distinct(datetime, tag_id, sex, location,
           station_name, latitude, longitude)

dat1 <- dat %>%
  arrange(original_id, movement_type) %>%
  group_by(original_id) %>%
  mutate(arrival_location = if_else(is.na(arrival_location),
                                    lag(arrival_location), arrival_location),
         departure_location = if_else(is.na(departure_location),
                                      lag(departure_location), departure_location)) %>%
  ungroup()

head(dat1)

dat2 <- dat1 %>%
  arrange(original_id, movement_type) %>%
  group_by(original_id) %>%
  mutate(arrival_location = if_else(is.na(arrival_location),
                                    lead(arrival_location), arrival_location),
         departure_location = if_else(is.na(departure_location),
                                      lead(departure_location), departure_location)) %>%
  ungroup()

head(dat2)

# Filter dat movements that go to and from just a degree -----------------------

dat3 <- dat2 %>% # we only are interested in focal locations
  filter(!(startsWith(departure_location, "deg_") & #but this is methods specific
             startsWith(arrival_location, "deg_"))) %>%
  mutate(datetime = if_else(movement_type == "Arrival", arrival_date, departure_date),
         datetime = as.POSIXct(datetime))


dat4 <- janitor::clean_names(dat3)
str(dat4)
# merge -------------------------------------------------------------------

# Jo in by tag_id, location, and date
dat5 <- dat4 %>%
  left_join(raw1 %>%
      select(tag_id, location, datetime, station_name,
             latitude, longitude),
    by = c("tag_id", "location", "datetime")) %>% 
  distinct(datetime, tag_id, station_name)


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
