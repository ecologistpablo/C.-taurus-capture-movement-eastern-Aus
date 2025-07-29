# 18 July 23 - 25 July 25
  #cleaning up
    #

rm(list=ls()) 
pacman::p_load("tidyverse")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250725_step8.csv")

# gloves on? let's wrestle ------------------------------------------------


location_levels <- c("Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                     "Gold Coast", "Ballina", "Coffs Harbour", "Hawks Nest", "Sydney")

dat1 <- dat %>%
  filter(location %in% location_levels) %>%
  mutate(location = factor(location, levels = location_levels))
unique(dat1$location)

colnames(dat1)
colnames(dat1)

dat2 <- dat1 %>% 
  group_by(station_name, latitude, longitude) %>%
  mutate(station_name = if (n_distinct(latitude, longitude) > 1) 
    paste0(station_name, "_", row_number()) 
    else 
      station_name) %>%
  ungroup()

unique(dat2$station_name)

# unique station_names ----------------------------------------------------

# Identify station_names with more than one lat/lon combo
station_duplicates <- dat2 %>%
  distinct(station_name, latitude, longitude) %>%
  add_count(station_name) %>%
  filter(n > 1) %>%
  pull(station_name)

# Fix duplicates by suffixing only where needed
dat3 <- dat2 %>%
  group_by(station_name) %>%
  mutate(station_name = case_when(
    station_name %in% station_duplicates ~ paste0(
      station_name, "_", match(interaction(latitude, longitude),
                               unique(interaction(latitude, longitude)))),
    TRUE ~ station_name)) %>%
  ungroup()

unique(dat3$station_name) #did it work?
setequal(unique(dat2$station_name), unique(dat3$station_name)) # they should be different
# if it's true, it didn't work (or you don't have duplicate station_names)
# plot it -----------------------------------------------------------------

# Calculate the number of detections at each station
IMOSxy <- dat3 %>%
  group_by(location, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude", "latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = F)

# nice, gaaaarrrrryyy

#save it ----------------------------------------------------------------------------

write_csv(dat3, file = "Inputs/250728_step9.csv")

# xy coords ---------------------------------------------------------------

xy <- dat3 %>% 
  distinct(latitude, longitude, station_name)

write_csv(xy, file = "Inputs/250728_step9_coordinates.csv")

