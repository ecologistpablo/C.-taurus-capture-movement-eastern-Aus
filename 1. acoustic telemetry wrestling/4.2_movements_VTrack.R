

rm(list=ls()) 
setwd("~/Documents/USC/Honours/R/data")
pacman::p_load("tidyverse", "VTrack", "lubridate", 'tictoc')
dat <- read.csv("Inputs/250724_step3.csv")


dat1 <- dat %>% 
  mutate(datetime = with_tz(ymd_hms(datetime, tz = "UTC"), tzone = "Etc/GMT-10")) %>% 
  select(-receiver_name) %>% 
  filter(if_all(everything(), ~ !is.na(.)))

str(dat1)

movements <- dat1 %>%
  arrange(tag_id, datetime) %>%
  group_by(tag_id) %>%
  mutate(next_location = lead(location),
         next_time = lead(datetime)) %>%
  # Only keep rows where the fish moves to a new location (omit NA at end and same-location events)
  filter(!is.na(next_location), location != next_location) %>%
  transmute(
    tag_id = tag_id,
    start_time = datetime,
    end_time = next_time,
    start_location = location,
    end_location = next_location,
    duration = round(as.numeric(difftime(next_time, datetime, units = "days")),2)) %>%
  ungroup()

print(head(movements))
