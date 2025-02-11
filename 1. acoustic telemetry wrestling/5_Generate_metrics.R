#29.05.23
  #data combination
    #We ran ddat & IMOS seperately through VTrack, let's combine the data now

rm(list=ls()) 
source("/Users/owuss/Documents/USC/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
cdat <- read_csv("Inputs/250211_step4.csv")

# metrics -----------------------------------------------------------------

cdat <- cdat %>%
  mutate(Num_days = as.numeric(DURATION / 3600)) #calculate duration of movement

cdat <- cdat %>% 
  rename( #rename rows
    Departure_date = STARTTIME,
    Arrival_date = ENDTIME,
    Tag_ID = TRANSMITTERID,
    Departure_location = STATIONNAME1,
    Arrival_location = STATIONNAME2) %>% 
  dplyr::select(-NONRESIDENCEEVENT, -DURATION)


cdat <- cdat %>% 
  mutate(Arrival_date = as.Date(Arrival_date),
         Departure_date = as.Date(Departure_date))


Location_levels <- c("deg_-24", "deg_-25", "Wolf Rock", "deg_-27", 
                     "Flat Rock",  "deg_-28", "deg_-29", "deg_-30",
                     "Coffs Harbour", "deg_-31",
                     "deg_-32", "deg_-33",
                     "Hawks Nest", "deg_-34",  "Sydney",
                     "deg_-35",
                     "deg_-36", "deg_-37")

cdat1 <- cdat %>%
  mutate( #create directionality as a row
    Departure_location = factor(Departure_location, levels = Location_levels),
    Arrival_location = factor(Arrival_location, levels = Location_levels),
    Direction = if_else(as.integer(Departure_location) > as.integer(Arrival_location),
                        "North", "South") 
  )

unique(cdat1$Direction)
summary(cdat1$Direction)

str(cdat1)

#save it -----------------------------------------------------------------------

write_csv(cdat1,file = "Inputs/250211_step5.csv") 

