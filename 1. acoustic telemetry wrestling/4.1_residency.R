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
dat <- read_rds("Inputs/250902_step3.rds")
unique(dat$location)

CH <- dat %>% 
  filter(location == "Coffs Harbour") %>% 
  mutate(date = date(datetime),
         month = month(date)) %>% 
  filter(month %in% c(3, 4, 5))

dat1 <- dat %>% 
mutate(datetime = with_tz(ymd_hms(datetime, tz = "UTC"), tzone = "Etc/GMT-10"),
       tag_id = as.character(tag_id)) %>% 
  select(-receiver_name) %>% 
  filter(if_all(everything(), ~ !is.na(.))) # remove NAs

anyNA(dat1) # you can't have NAs in a dataset
str(dat1) # structure of columns should be the same as below:

# tag id = character
# datetime = POSIXct
# location = character
# station name = chr
# lat / lon = numeric
# sex = chr

# outdated ----------------------------------------------------------------


#oudated 
min_detections <- 2 # minimum detections per day to enter residency
min_res_period <- 2 # minimum duration threshold in days for 'residency' to occur
max_gap_secs <- 86400  
60*60*24*1 #15 days in seconds

tic() # tic toc times functions 

residency <- dat1 %>%
  arrange(tag_id, location, datetime) %>% # location or receiver grouping
  group_by(tag_id, location) %>% # arrange by date, group by ID
  mutate(time_gap = as.numeric(difftime(datetime, # compute time gap between det 1 & 2
                                   lag(datetime, default = first(datetime)), # lag finds the second ping and grabs the time stamp
                                   units = "secs")), # calculate gap in seconds for filtering late
    new_event = ifelse(is.na(time_gap) | time_gap > max_gap_secs, 1, 0), #filter rows that have a gap larger than the one specified  
    event_id = cumsum(new_event)) %>% # number residency events 
  group_by(tag_id, location, event_id) %>% # re-group
  summarise(start_datetime = min(datetime), # start date
            end_datetime = max(datetime), # end date
            n_detections = n(), # total number of pings within the resideny event
            duration_days = round(as.numeric(difftime(max(datetime), # num of days 
                      min(datetime), units = "days")), 2), .groups = "drop",
           sex = first(sex)) %>% # bring sex over                  
  dplyr::filter(n_detections >= min_detections, duration_days >= min_res_period) %>% # filter residency that is less than min days
  arrange(tag_id, start_datetime) # return a clean df 

toc() # 2.165 seconds to process 2.5 million rows
# we love vectorised functions 

residency
table(residency$location)

# updated -----------------------------------------------------------------

min_detections_per_day <- 1   # minimum detections required on each day
min_res_days           <- 2   # minimum length of residency in days (calendar, inclusive)
max_gap_secs           <- 60*60*24*2  # 10 days in seconds  (update comment if you want 15)

# residency ------------------------------------------------------------
tic()
residency <- dat1 %>%
  arrange(tag_id, location, datetime) %>% 
  group_by(tag_id, location) %>%
  mutate(time_gap = as.numeric(difftime(datetime, lag(datetime), units = "secs")),
    new_event = ifelse(is.na(time_gap) | time_gap > max_gap_secs, 1L, 0L),
    event_id = cumsum(replace_na(new_event, 1L))) %>%
  ungroup() %>%
  group_by(tag_id, location, event_id) %>%
  # compute daily stats within each event
  mutate(day = as_date(datetime)) %>%
  summarise(start_datetime = min(datetime),
    end_datetime = max(datetime),
    start_day = min(day),
    end_day = max(day),
    n_days_incl = as.integer(end_day - start_day) + 1L,   # inclusive day span
    n_detections = n(),
    # per-day detection counts
    days_meeting_threshold = sum( tapply(rep(1, n()), day, length) >= min_detections_per_day),
    sex = first(sex),
    .groups = "drop_last") %>%
  # keep events that last long enough AND meet the per-day threshold on every day
  filter(n_days_incl >= min_res_days,
    days_meeting_threshold == n_days_incl) %>%
  ungroup() %>%
  arrange(tag_id, start_datetime)
toc()

unique(residency$location)

# save your beautiful work
write_rds(residency, "Inputs/250902_residency.rds")

