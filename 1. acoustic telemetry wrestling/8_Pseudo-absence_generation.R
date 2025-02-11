#15.07.23
  #Psuedo 2 IMOS generation

rm( list=ls()) 

source("/Users/owuss/Documents/USC/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250211_step7.csv")

summary(dat$date)

# preamble ----------------------------------------------------------------

# we have movements which will be presence in a model
# we need an absence, something acoustic telemetry only gives on a large scale
# to avoid zero-inflated models (the dark arts of biostatistics...), we shall dive into something else
# pseudo-absences
# the largest number of absences per presence without inflating a model with biase, is 2:1
# so for every presence, let's make two absences


# Psuedo Presence x 2 ----------------------------------------------------------

generate_pseudo_absences <- function(...) {
  
  # Combine all the arguments into a single-row data frame
  data_row <- tibble(...)
  
  # Extract the year and month from the detection_datetime
  year <- lubridate::year(data_row$date)
  month <- lubridate::month(data_row$date)
  
  # Specify the end date based on the year
  if(year == 2023) {
    end_date <- as.Date(paste(year, "-07-31", sep = ""))
  } else {
    end_date <- as.Date(paste(year, "-12-31", sep = ""))
  }
  
  # Generate two random dates within the year but outside the detection month
  random_dates <- sample(
    x = seq.Date(
      from = as.Date(paste(year, "-01-01", sep = "")),
      to = end_date,
      by = "day"
    ),
    size = 2
  )
  
  # Filter out dates within the detection month
  random_dates <- random_dates[lubridate::month(random_dates) != month]
  
  # If there are less than two dates after filtering, try again
  if (length(random_dates) < 2) {
    return(generate_pseudo_absences(tibble(...)))
  }
  
  # Create the pseudo-absence rows
  pseudo_absence_rows <- data_row %>%
    dplyr::slice(rep(1:n(), each = 2)) %>%
    mutate(
      date = random_dates,
      presence = 0
    )
  
  # Add Presence = 1 for the original detections
  data_row <- data_row %>%
    mutate(presence = 1)
  
  # Bind the original and pseudo-absence rows together
  result <- bind_rows(data_row, pseudo_absence_rows)
  
  return(result)
}


# Run forest run ---------------------------------------------------------------

dat1 <- dat %>%
  pmap_dfr(generate_pseudo_absences)

summary(dat1$date) #it worked

# prep data ---------------------------------------------------------------

dat2 <- dat1 %>% 
mutate(month = lubridate::month(date))
  
dat3 <- dat2 %>%
  mutate(location = if_else(movement_type == "Arrival", arrival_location, departure_location)) %>% 
  rename(movement = movement_type)


# plot it -----------------------------------------------------------------

# Calculate the number of detections at each station
IMOSxy <- dat3 %>%
  group_by(location, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude", "latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = F)

#save it -----------------------------------------------------------------------

write_csv(dat1, file = "Inputs/250211_step8.csv")

