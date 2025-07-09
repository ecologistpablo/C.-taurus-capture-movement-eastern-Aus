#29.05.23
  # data preparation 
    # Pablo Fuenzalida

rm(list=ls()) 
library(tidyverse)
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250709_step4.csv")

# metrics -----------------------------------------------------------------

dat1 <- dat %>%
  mutate(num_days = round(DURATION / 86400, 2)) # calculate duration of movement in days and round to 1 decimal

dat2 <- dat1 %>% 
  rename(departure_date = STARTTIME,
    arrival_date = ENDTIME,
    tag_id = TRANSMITTERID,
    departure_location = STATIONNAME1,
    arrival_location = STATIONNAME2) %>% 
  dplyr::select(-NONRESIDENCEEVENT, -DURATION) %>% 
  mutate(arrival_date = as.Date(arrival_date),
         departure_date = as.Date(departure_date))

Location_levels <- c("deg_-24", "deg_-25", "Wolf Rock", "deg_-27", "Sunshine Coast", 
                     "deg_-28","Flat Rock", "Gold Coast", "Ballina", "deg_-29", 
                     "deg_-30", "Coffs Harbour", "deg_-31","deg_-32", "deg_-33",
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

