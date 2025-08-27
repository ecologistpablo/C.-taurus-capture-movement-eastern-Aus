# 25 July 2025
  # Pablo Fuenzalida
    # VTrack broken - so we endure without it

# non-residency is just as important as residency
# we can quantify it by arranging detections by tag id and datetime
# and connecting one ping with the next using lead / lag
# to see when an animal moved from one receiver to the next
# we can then add filters in space and time
# but we shall do that later

# load library and data ---------------------------------------------------

pacman::p_load("tidyverse", 'tictoc')
rm(list=ls()) 
setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250827_step3.rds")

dat1 <- dat %>% 
  mutate(datetime = with_tz(ymd_hms(datetime, tz = "UTC"), tzone = "Etc/GMT-10"),
         tag_id = as.character(tag_id)) %>% 
  select(-receiver_name) %>% 
  filter(if_all(everything(), ~ !is.na(.))) # remove NAs and their entire row
anyNA(dat1) # needs to be false
str(dat1)
# tag id = character
# datetime = POSIXct
# location = character
# station name = chr
# lat / lon = numeric
# sex = chr

# non-residency -----------------------------------------------------------

tic()  # lets time it

move_base <- dat1 %>%
  arrange(tag_id, datetime) %>% # arrange by ID and time
  group_by(tag_id) %>% # process data by tag ID 
  mutate(next_location = lead(location), # lead time (lead / lag connects two rows (detections) that have already been arranged by time)
    next_time = lead(datetime), # lead time
    next_station_name = lead(station_name), # lead location name, can be changed to receiver
    next_latitude = lead(latitude), # lat 
    next_longitude = lead(longitude)) %>% # lon
  # Define a movement as a change in location
  filter(!is.na(next_location), location != next_location) %>% # filter movements that go to the same location (my methods but you can remove)
  mutate(movement_id = row_number()) %>% # movement_id to connect an arrival / departure movement as one in future 
  ungroup() 

toc() # 1.978 seconds for 2.5 million rows, we love vectorised function

# lead created data for detecion 2, and replicated all variables
# we now use this dataframe to create arrival and departure data
# departures are detection 1, arrivals are detection 2 in a 'non-residency' or 'movement'
# we then use bind_rows to connect them 
# the output gives us one row per arrival / departure, with all data we need

# Departure rows creation
departures <- move_base %>%
  transmute(tag_id, # transmute brings over data, arranges in order, and keeps str as well as renames all in one
            datetime = datetime, # first datetime val in the dataframe
            sex = sex,
            location = location,
            station_name = station_name,
            latitude = latitude,
            longitude = longitude,
            movement = "departure",
            movement_id)

# Arrival rows
arrivals <- move_base %>%
  transmute(tag_id, # transmute brings over data, arranges in order, and keeps str as well as renames all in one
            datetime = next_time,
            sex = sex,
            location = next_location,
            station_name = next_station_name,
            latitude = next_latitude,
            longitude = next_longitude,
            movement = "arrival",
            movement_id)

# Combine
dat2 <- bind_rows(departures, arrivals) %>% # departure and arrival dataframes should be the same size in obs (they're halfs of the same df movebase)
  arrange(tag_id, movement_id, movement, datetime)

str(dat2)
table(dat2$location)
# dat2 should be double the obs of movebase
# since all we did was split rows of a movement into two: arrivals and depatures

datxy <- dat2 %>%
  group_by(location, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(datxy, coords = c("longitude",
                        "latitude"), crs = 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = FALSE)


# save our beautiful work
write_rds(dat2, "Inputs/250827_step4.rds")
