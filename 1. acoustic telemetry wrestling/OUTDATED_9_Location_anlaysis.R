# site selection
  #02.06.23
    #Pfuenzalida
      #Wrangling 

rm(list=ls()) 
source("/Users/owuss/Documents/USC/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
cdat <- read_csv("Inputs/250211_step8.csv")
str(cdat)
head(cdat)

#lets find out what sites have the most data, to direct our attention to their analysis

# our lil function --------------------------------------------------------

count_arrival_departure_by_location <- function(data) {
  # Count arrivals by location
  arrival_counts <- data %>%
    filter(movement == "Arrival") %>%
    count(Location, name = "Arrival_Count") %>%
    arrange(desc(Arrival_Count))
  
  # Count departures by location
  departure_counts <- data %>%
    filter(movement == "Departure") %>%
    count(Location, name = "Departure_Count") %>%
    arrange(desc(Departure_Count))
  
  # Join the arrival and departure counts
  combined_counts <- full_join(arrival_counts, departure_counts, by = "Location")
  
  return(combined_counts)
}

# Call the function
movement_counts <- count_arrival_departure_by_location(cdat)

movement_counts

# to record for results
