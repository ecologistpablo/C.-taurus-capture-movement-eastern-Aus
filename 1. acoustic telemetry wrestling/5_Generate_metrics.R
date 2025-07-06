#29.05.23
  # data preparation 
    # Pablo Fuenzalida

rm(list=ls()) 
library(tidyverse)
setwd("/Users/owuss/Documents/USC/Honours/R/data")
cdat <- read_csv("Inputs/250705_step4.csv")

# metrics -----------------------------------------------------------------

cdat1 <- cdat %>%
  mutate(num_days = round(DURATION / 86400, 2)) # calculate duration of movement in days and round to 1 decimal

cdat2 <- cdat1 %>% 
  rename( #rename rows
    Departure_date = STARTTIME,
    Arrival_date = ENDTIME,
    Tag_ID = TRANSMITTERID,
    Departure_location = STATIONNAME1,
    Arrival_location = STATIONNAME2) %>% 
  dplyr::select(-NONRESIDENCEEVENT, -DURATION)


cdat3 <- cdat2 %>% 
  mutate(Arrival_date = as.Date(Arrival_date),
         Departure_date = as.Date(Departure_date))


Location_levels <- c("deg_-24", "deg_-25", "Wolf Rock", "deg_-27", 
                     "Flat Rock",  "deg_-28", "Ballina", "deg_-29", "deg_-30",
                     "Coffs Harbour", "deg_-31",
                     "deg_-32", "deg_-33",
                     "Hawks Nest", "deg_-34",  "Sydney",
                     "deg_-35",
                     "deg_-36", "deg_-37")

cdat4 <- cdat3 %>%
  mutate( #create directionality as a row
    Departure_location = factor(Departure_location, levels = Location_levels),
    Arrival_location = factor(Arrival_location, levels = Location_levels),
    Direction = if_else(as.integer(Departure_location) > as.integer(Arrival_location),
                        "North", "South"))

unique(cdat4$Direction)

str(cdat4)

#save it -----------------------------------------------------------------------

write_csv(cdat3,file = "Inputs/250705_step5.csv") 

