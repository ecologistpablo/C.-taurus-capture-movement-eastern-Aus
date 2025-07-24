

rm(list=ls()) 
setwd("~/Documents/USC/Honours/R/data")
pacman::p_load("tidyverse", "VTrack", "lubridate", 'tictoc')
dat <- read.csv("Inputs/250724_step3.csv")

dat1 <- dat %>% 
mutate(datetime = with_tz(ymd_hms(datetime, tz = "UTC"), tzone = "Etc/GMT-10")) %>% 
  select(-receiver_name) %>% 
  filter(if_all(everything(), ~ !is.na(.)))

str(dat1)



min_detections <- 2
min_days <- 2          # minimum event duration in days
max_gap_secs <- 86400  # 1 day

residency_events <- dat1 %>%
  arrange(tag_id, location, datetime) %>%
  group_by(tag_id, location) %>%
  mutate(
    time_gap = as.numeric(difftime(datetime, lag(datetime, default = first(datetime)), units = "secs")),
    new_event = ifelse(is.na(time_gap) | time_gap > max_gap_secs, 1, 0),
    event_id = cumsum(new_event)) %>%
  group_by(tag_id, location, event_id) %>%
  summarise(
    start_time = min(datetime),
    end_time = max(datetime),
    n_detections = n(),
    duration_days = round(as.numeric(difftime(max(datetime), min(datetime), units = "days")), 2), .groups = "drop") %>%
  filter(n_detections >= min_detections, duration_days >= min_days) %>%
  arrange(tag_id, start_time)

residency_events

write_csv(residency_events, "Inputs/250724_residency.csv")

