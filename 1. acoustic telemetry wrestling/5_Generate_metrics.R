#29.05.23
  # data preparation 
    # Pablo Fuenzalida

# let's calculate the time period of movements
# as well as direction of movement (north or south)

library(tidyverse)
rm(list=ls()) 
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250730_step4.rds")

# metrics -----------------------------------------------------------------

dat_wide <- dat %>%
  select(tag_id, movement_id, datetime, movement) %>%
  pivot_wider(names_from = movement, values_from = datetime) %>% # pivot wider reshapes data: for each tag_id and movement_id, get the "departure" datetime and the "arrival" datetime in the same row
  # This gives columns: tag_id, movement_id, arrival, departure
  mutate(num_days = as.numeric(difftime(arrival, departure, units = "days")))

dat1 <- dat %>% # Now, join num_days back to the original dat
  left_join(dat_wide %>% select(tag_id, movement_id, num_days),
            by = c("tag_id", "movement_id"))

rm(dat_wide) # clean up messy 


# location setting --------------------------------------------------------

# Compute your N→S ordering from the data itself
Location_levels <- dat1 %>%
  group_by(location) %>%
  summarise(mean_lat = mean(latitude, na.rm = TRUE)) %>%
  arrange(desc(mean_lat)) %>%
  pull(location)

# 2. Build your movement‐direction table using that ordering
mov_dir <- dat1 %>%
  select(tag_id, movement_id, movement, location) %>%
  pivot_wider(names_from  = movement,
              values_from = location,
              names_prefix = "location_") %>%
  mutate(departure_location = factor(location_departure,
                                levels = Location_levels,
                                ordered = TRUE),
         arrival_location   = factor(location_arrival,
                                levels = Location_levels,
                                ordered = TRUE),
         direction = case_when(
      is.na(departure_location) | is.na(arrival_location) ~ NA_character_,
      as.integer(departure_location) > as.integer(arrival_location) ~ "North",
      as.integer(departure_location) < as.integer(arrival_location) ~ "South",
      TRUE ~ "None")) %>%
  select(tag_id, movement_id, direction)

dat2 <- dat1 %>%# Join it back to your main dat2
  left_join(mov_dir, by = c("tag_id", "movement_id")) %>%
  relocate(direction, .after = movement_id)

colnames(dat2)
anyNA(dat2$direction) # this should be false

#save it -----------------------------------------------------------------------

write_rds(dat2, file = "Inputs/250730_step5.rds") 

