#29.05.23
  # data preparation 
    # Pablo Fuenzalida

# let's calculate the time period of movements
# as well as direction of movement (north or south)

library(tidyverse)
rm(list=ls()) 
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250725_step4.csv")

# metrics -----------------------------------------------------------------

dat_wide <- dat %>%
  select(tag_id, movement_id, datetime, movement) %>%
  pivot_wider(names_from = movement, values_from = datetime) %>% # pivot wider reshapes data: for each tag_id and movement_id, get the "departure" datetime and the "arrival" datetime in the same row
  # This gives columns: tag_id, movement_id, arrival, departure
  mutate(num_days = as.numeric(difftime(arrival, departure, units = "days")))

# Now, join num_days back to the original dat2
dat1 <- dat %>%
  left_join(dat_wide %>% select(tag_id, movement_id, num_days),
            by = c("tag_id", "movement_id"))

rm(dat_wide) # clean up messy 
# map ---------------------------------------------------------------------
datxy <- dat %>%
  group_by(location, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(datxy, coords = c("longitude",
                        "latitude"), crs = 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = FALSE)
# levels ------------------------------------------------------------------

# Define your location levels (ordered N-S) 
Location_levels <- c("deg_-24", "deg_-25", "Wide Bay", "deg_-27", "Sunshine Coast", 
                     "Moreton Island","North Stradbroke Island", "Gold Coast","deg_-29","Ballina", 
                     "deg_-30", "Coffs Harbour", "deg_-31","deg_-32", "Port Macquarie", "Forster",
                     "Hawks Nest", "Newcastle", "deg_-34", "Sydney","deg_-35","deg_-36", "deg_-37")

mov_dir <- dat1 %>% # group and pivot 
  select(tag_id, movement_id, movement, location) %>%
  pivot_wider(names_from = movement,
    values_from = location,
    names_prefix = "location_") %>% # make some new cols
  mutate(departure_location = factor(location_departure,
                                levels = Location_levels, ordered = TRUE),
    arrival_location   = factor(location_arrival,
                                levels = Location_levels, ordered = TRUE),
    direction = case_when(is.na(departure_location) | # is departure infront or behind arrival in the levels
                            is.na(arrival_location) ~ NA_character_,
      as.integer(departure_location) > as.integer(arrival_location) ~ "North",
      as.integer(departure_location) < as.integer(arrival_location) ~ "South",
      TRUE ~ "None")) %>%
  select(tag_id, movement_id, direction)

# 2. Join direction back to tidy dat2
dat2 <- dat1 %>%
  left_join(mov_dir, by = c("tag_id", "movement_id")) %>%
  relocate(direction, .after = movement_id) # Move direction next to movement_id

unique(dat2$direction)

#save it -----------------------------------------------------------------------

write_csv(dat2, file = "Inputs/250725_step6.csv") 

