#29.05.23
  # data preparation 
    # Pablo Fuenzalida

# let's calculate the timeperiod of movements
# as well as direction of movement (north or south)

rm(list=ls()) 
library(tidyverse)
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


# map ---------------------------------------------------------------------

datxy <- dat %>%
  group_by(location, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(datxy, coords = c("longitude",
                        "latitude"), crs = 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = FALSE)


# levels ------------------------------------------------------------------

# use map to find out order of locations from north to south
Location_levels <- c("deg_-24", "deg_-25", "Wide Bay", "deg_-27", "Sunshine Coast", 
                     "Moreton Island","North Stradbroke Island", "Gold Coast","deg_-29","Ballina", 
                     "deg_-30", "Coffs Harbour", "deg_-31","deg_-32", "Port Macquarie", "deg_-33",
                     "Hawks Nest", "deg_-34",  "Sydney","deg_-35","deg_-36", "deg_-37")

dat3 <- dat2 %>%
  mutate( #create directionality as a row
    departure_location = factor(departure_location, levels = Location_levels),
    arrival_location = factor(arrival_location, levels = Location_levels),
    direction = if_else(as.integer(departure_location) > as.integer(arrival_location),
                        "North", "South"))

unique(dat3$direction)

str(dat3)

#save it -----------------------------------------------------------------------

write_csv(dat3,file = "Inputs/250709_step5.csv") 

