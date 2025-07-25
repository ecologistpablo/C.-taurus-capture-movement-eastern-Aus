# 24 July 2025
  # VTrack broken, adapting by calculating residency ourselves
    # Pablo Fuenzalida

# Acoustic telemetry residency function
# you can manipulate minimum residency period detections per day
# and minimum days to equate residency
# as well as maximum time gap between detections
# for more info on residency, read:
# https://link.springer.com/article/10.1186/s40462-022-00364-z

# library and data load ---------------------------------------------------

pacman::p_load("tidyverse", 'tictoc')
rm(list=ls()) 
setwd("~/Documents/USC/Honours/R/data")
dat <- read.csv("Inputs/250725_step3.csv")

dat1 <- dat %>% 
mutate(datetime = with_tz(ymd_hms(datetime, tz = "UTC"), tzone = "Etc/GMT-10"),
       tag_id = as.character(tag_id)) %>% 
  select(-receiver_name) %>% 
  filter(if_all(everything(), ~ !is.na(.))) # remove NAs
anyNA(dat1)
str(dat1)
# tag id = character
# datetime = POSIXct
# location = character
# station name = chr
# lat / lon = numeric
# sex = chr

# inputs ------------------------------------------------------------------

min_detections <- 1 # minimum detections per day to enter residency
min_res_period <- 1 # minimum duration threshold in days for 'residency' to occur
max_gap_secs <- 1296000  # 1 day gap allowed between detections (in seconds: 86400 seconds in a day)

# 60*60*24*15 15 days in seconds

# residency ---------------------------------------------------------------

tic() # tic toc times functions 

residency <- dat1 %>%
  arrange(tag_id, location, datetime) %>% # location or receiver grouping
  group_by(tag_id, location) %>% # arrange by date, group by ID
  mutate(time_gap = as.numeric(difftime(datetime, # compute time gap between det 1 & 2
                                   lag(datetime, default = first(datetime)), # lag finds the second ping and grabs the time stamp
                                   units = "secs")), # calculate lag in seconds for filtering late
    new_event = ifelse(is.na(time_gap) | time_gap > max_gap_secs, 1, 0),  
    event_id = cumsum(new_event)) %>% # number residency events 
  group_by(tag_id, location, event_id) %>% # re-group
  summarise(start_datetime = min(datetime), # start date
            end_datetime = max(datetime), # end date
            n_detections = n(), # total number of pings 
            duration_days = round(as.numeric(difftime(max(datetime), # num of days 
                      min(datetime), units = "days")), 2), .groups = "drop",
           sex = first(sex)) %>%                   
  dplyr::filter(n_detections >= min_detections, duration_days >= min_res_period) %>% # filter residency that is less than min days
  arrange(tag_id, start_datetime) # return a clean df 

toc() # 2.165 seconds to process 2.5 million rows
# we love vectorised functions 

residency

# save your beautiful work
write_csv(residency, "Inputs/250725_residency.csv")

