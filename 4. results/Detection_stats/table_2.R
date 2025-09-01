library(tidyverse)
rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250827_step9.rds") 

loc_summary <- dat %>%
  filter(location!= "Moreton Island") %>% 
  mutate(location = fct_relevel(location,
                                        "Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                                        "Gold Coast", "Ballina", "Evans Head", "Yamba", "Coffs Harbour",
                                        "Port Macquarie", "Forster",  "Hawks Nest",
                                        "Central Coast",  "Sydney", "Illawarra", "Merimbula"))%>% 
  group_by(location) %>%
  summarise(movements  = n(),
    tags_detected = n_distinct(tag_id), 
    detected_receivers  = n_distinct(station_name), 
    years_detected  = n_distinct(year(datetime)), 
    unique_detection_days = n_distinct(as.Date(datetime))) %>%  
  arrange(location)


