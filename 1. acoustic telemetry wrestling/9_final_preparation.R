# 18 July 23 - 25 July 25
  #cleaning up
    #

rm(list=ls()) 
pacman::p_load("tidyverse")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250730_step8.rds")

# gloves on? let's wrestle ------------------------------------------------

dat1 <- dat %>%
  filter(!startsWith(location, "deg_"))

# unique station_names ----------------------------------------------------

# Identify station_names with more than one lat/lon combo
station_duplicates <- dat1 %>%
  distinct(station_name, latitude, longitude) %>%
  add_count(station_name) %>%
  filter(n > 1) %>%
  pull(station_name)

# Fix duplicates by suffixing only where needed
dat2 <- dat1 %>%
  group_by(station_name) %>%
  mutate(station_name = case_when(
    station_name %in% station_duplicates ~ paste0(
      station_name, "_", match(interaction(latitude, longitude),
                               unique(interaction(latitude, longitude)))),
    TRUE ~ station_name)) %>%
  ungroup()

unique(dat2$station_name) #did it work?
unique(dat1$station_name) # are they different sizes?

# plot it -----------------------------------------------------------------

# Calculate the number of detections at each station
IMOSxy <- dat2 %>%
  group_by(location, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude", "latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = F)

# nice, gaaaarrrrryyy

#save it ----------------------------------------------------------------------------

write_rds(dat2, file = "Inputs/250730_step9.rds")

# xy coords ---------------------------------------------------------------

xy <- dat2 %>% 
  distinct(latitude, longitude, station_name)

write_rds(xy, file = "Inputs/250730_step9_coordinates.rds")

