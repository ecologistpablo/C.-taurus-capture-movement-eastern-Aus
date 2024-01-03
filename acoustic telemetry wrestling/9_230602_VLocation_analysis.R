# site selection
  #02.06.23
    #Pfuenzalida
      #Wrangling 

rm(list=ls()) 
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("~/University/2023/Honours/R/data")
cdat <- read_csv("Inputs/230907_step8.csv")
str(cdat)
head(cdat)

#lets find out what sites have the most data, to direct our attention to their analysis

# total num of movements by location -------------------------------------------

count_location_total <- function(data) {
  # Combine departures and arrivals in one step
  total_location_count <- data %>%
    count(Location, name = "Count") %>%
    arrange(desc(Count))
  return(total_location_count)
}

# Call the new function
tlc <- count_location_total(cdat)
print(tlc, n=28)

# by arrival / departure -------------------------------------------------------

count_locations <- function(data) {
  # Combine departures and arrivals in one step
  location_counts <- data %>%
    gather(Type, Location, movement) %>%
    count(Type, Location, name = "Count") %>%
    arrange(desc(Count))
  return(location_counts)
}

location_counts <- count_locations(cdat)
print(location_counts, n =54)

# output -----------------------------------------------------------------------

#old output w 14 day rule on
# # A tibble: 28 × 2
# Location                Count
# <chr>                   <int>
# 1 Coffs Harbour              94
# 2 Flat Rock                  81
# 3 Sydney                     59
# 4 Wolf Rock                  50
# 5 Hawks Nest                 39
# 6 Moreton Island             36
# 7 Port Macquarie             26
# 8 Jervis Bay                 20
# 9 Gold Coast                 14
# 10 Seal Rocks                 13
# 11 Montague Island            12
# 12 Ballina                    10
# 13 Yamba                       7
# 14 Fish Rock                   6
# 15 Sunshine Coast Inshore      6
# 16 Moreton Island North        5
# 17 Forster                     4
# 18 Moreton Island South        4
# 19 Taree                       4
# 20 Wollongong                  4
# 21 Byron Bay                   3
# 22 Heron Island                3
# 23 K'gari                      3
# 24 Batemans Bay                2
# 25 Noosa                       2
# 26 Pimpernel Rock              2
# 27 Sunshine Coast Offshore     2
# 28 Crescent Head               1

#   1 Flat Rock            114
# 2 Coffs Habour          66
# 3 Sydney                56
# 4 Moreton Island        54
# 5 Wolf Rock             49
# 6 Hawks Nest            36
# 7 Jervis Bay            32
# 8 deg_-34               25
# 9 Sunshine Coast        21
# 10 deg_-32               20
# 11 deg_-33               20
# 12 deg_-27               16
# 13 deg_-35               16
# 14 deg_-29               14
# 15 Coffs Habour North    13
# 16 deg_-28               12
# 17 Port Macquarie        11
# 18 Fish Rock             10
# 19 deg_-36                9
# 20 deg_-31                8
# 21 Montague Island        7
# 22 Kiama                  5
# 23 Pimpernel Rock         4
# 24 deg_-30                2
# 25 deg_-24                1
# 26 deg_-25                1

sites_to_keep <- c("Flat Rock", "Coffs Habour",
                   "Sydney", "Moreton Island",
                   "Wolf Rock", "Hawks Nest", "Jervis Bay")

ldat1 <- ldat %>%
  filter(Location %in% sites_to_keep)

#save it -----------------------------------------------------------------------

save(ldat1, file = "Inputs/ldat_230718.RData")
write_csv(ldat1,file = "Inputs/ldat_230718.csv")

#loading 
#load("Inputs/cdat_230715.RData")


