


pacman::p_load(tidyverse, furrr, future, lubridate, geosphere)

pacman::p_load("tidyverse", 'tictoc')
rm(list=ls()) 
setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/260323_step3.rds")

dat1 <- dat %>% 
  select(-receiver_name) %>% 
  filter(if_all(everything(), ~ !is.na(.))) # remove NAs and their entire row

str(dat1)


# to use ------------------------------------------------------------------


# requirements: a data frame with:

# datetime: POSIXct
# location: character
# longitude: numeric
# latitude: numeric
# station_name : character
# sex: character


tic()
res <- RunResidenceExtraction(dat1,
                              location_col = "location",   # or "station_name" / "receiver_name"
                              time_threshold_secs = 86400, # 1 day
                              min_detections_per_day = 2, # daily threshold (optional)
                              min_residence_days = 2,# daily threshold (optional)
                              cores = 6)
toc()

residency <- res$residences      # residence events, with sex + duration_secs + n_detections
reslog <- res$residenceslog   # detection-level log
movement <- res$nonresidences   # long arrival/departure rows, with sex + distance (km)
