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



sites_to_keep <- c("Flat Rock", "Coffs Habour",
                   "Sydney", "Moreton Island",
                   "Wolf Rock", "Hawks Nest", "Jervis Bay")

ldat1 <- cdat %>%
  filter(Location %in% sites_to_keep)

#save it -----------------------------------------------------------------------

save(ldat1, file = "Inputs/ldat_230718.RData")
write_csv(ldat1,file = "Inputs/ldat_230718.csv")

#loading 
#load("Inputs/cdat_230715.RData")


# results munging ---------------------------------------------------------

dat <- ldat1 %>% 
  filter(Presence == "1")





