#15.07.23
  # Pseudo absence generation

# preamble ----------------------------------------------------------------

# we have movements which will be presence in a model
# we need an absence, something acoustic telemetry does not give
# to avoid zero-inflated models (the dark arts of biostatistics...)
# we shall dive into something else
# pseudo-absences
# the largest number of absences per presence without inflating a model with biase
# is 2:1, according to Dr. Kylie Scales, an experienced modeller
# so for every presence, let's make two absences
# let's make them random in time, but not in space
# they shall sit outside the month of the presence, but within the same year
# to imitate real absences that we cannot detect
# other methods also randomise them in space, but we won't

# packages ----------------------------------------------------------------

rm( list=ls()) 
library(tidyverse) 
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250725_step7.csv")

# Psuedo Presence x 2 ----------------------------------------------------------

generate_pseudo_absences <- function(...) {   # Combine all the arguments into a single-row data frame
  data_row <- tibble(...)
  year <- lubridate::year(data_row$datetime) # extract year
  month <- lubridate::month(data_row$datetime) # extract month
    if(year == 2025) {   # Specify the end date based on the year
    end_date <- as.Date(paste(year, "-07-31", sep = ""))
  } else {
    end_date <- as.Date(paste(year, "-12-31", sep = ""))
  }
  random_dates <- sample(   # Generate two random dates
        x = seq.Date( # within the same year, but outside of the same month
      from = as.Date(paste(year, "-01-01", sep = "")),
      to = end_date,
      by = "day"),
      size = 2)
  # Filter out dates within the detection month
  random_dates <- random_dates[lubridate::month(random_dates) != month]
  
  # If there are less than two dates after filtering, try again
  if (length(random_dates) < 2) {
    return(generate_pseudo_absences(tibble(...)))
  }
  
  # Create the pseudo-absence rows
  pseudo_absence_rows <- data_row %>%
    dplyr::slice(rep(1:n(), each = 2)) %>%
    mutate(date = random_dates, presence = 0)
  
  data_row <- data_row %>%   # Add Presence = 1 for the original detections
    mutate(presence = 1)
  
  result <- bind_rows(data_row, pseudo_absence_rows) # bind presence and absence
  return(result)
}

# Run forest run ---------------------------------------------------------------

dat1 <- dat %>%
  pmap_dfr(generate_pseudo_absences) # wait a while, it takes some time
# is dat1 the same as dat multiplied by 3?
summary(dat1$datetime) # are they within the same timeframe?
table(dat1$presence) # is presence the same num as dat obs ?

# prep data ---------------------------------------------------------------

dat2 <- dat1 %>% 
mutate(month = lubridate::month(datetime)) %>% 
  select(-date)

# plot it -----------------------------------------------------------------

# Calculate the number of detections at each station
IMOSxy <- dat2 %>%
  group_by(location, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("longitude", "latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "location", fbg = F)

#save it -----------------------------------------------------------------------

write_csv(dat2, file = "Inputs/250725_step8.csv")

# xy coordinates ----------------------------------------------------------

xy <- dat2 %>% 
  dplyr::distinct(latitude, longitude, location, station_name)

write_csv(xy, file = "Inputs/250626_xy_coordinates.csv")

