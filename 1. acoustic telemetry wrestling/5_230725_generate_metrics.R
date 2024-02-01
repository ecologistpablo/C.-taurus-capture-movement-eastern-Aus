#29.05.23
#data combination
#We ran ddat & IMOS seperately through VTrack, let's combine the data now

rm(list=ls()) 
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("~/University/2023/Honours/R/data")
cdat <- read_csv("Inputs/240114_step4.csv")

#641


# metrics -----------------------------------------------------------------

cdat <- cdat %>%
  mutate(Num_days = as.numeric(ENDTIME - STARTTIME)) #calculate duration of movement

cdat <- cdat %>% 
  rename( #rename rows
    Departure_date = STARTTIME,
    Arrival_date = ENDTIME,
    Tag_ID = TRANSMITTERID,
    Departure_location = STATIONNAME1,
    Arrival_location = STATIONNAME2) %>% 
  dplyr::select(-NONRESIDENCEEVENT, -DURATION)


Location_levels <- c("deg_-24", "deg_-25", "Wolf Rock", "deg_-27", 
                     "Moreton Island", 
                     "Flat Rock",  "deg_-28", "deg_-29", "deg_-30",
                     "Coffs Harbour", "deg_-31",
                     "deg_-32", "Port Macquarie", "deg_-33",
                     "Seal Rocks", "Hawks Nest", "deg_-34",  "Sydney",
                     "deg_-35", "Jervis Bay",
                     "deg_-36", "Montague Island")

cdat1 <- cdat %>%
  mutate( #create directionality 
    Departure_location = factor(Departure_location, levels = Location_levels),
    Arrival_location = factor(Arrival_location, levels = Location_levels),
    Direction = if_else(as.integer(Departure_location) > as.integer(Arrival_location),
                        "North", "South") 
  )

unique(cdat1$Direction)
summary(cdat1$Direction)

#save it -----------------------------------------------------------------------

write_csv(cdat1,file = "Inputs/240114_step5.csv") 

