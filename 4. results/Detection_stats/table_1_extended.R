library(tidyverse)
rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250827_step3.rds") 

dat1 <- dat %>%
  filter(location!= "Moreton Island") %>% 
  mutate(location = fct_relevel(location,
                                "Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                                "Gold Coast", "Ballina", "Evans Head", "Yamba", "Coffs Harbour",
                                "Port Macquarie", "Forster",  "Hawks Nest",
                                "Central Coast",  "Sydney", "Illawarra", "Merimbula"))

str(dat1)



# cumulative days ---------------------------------------------------------

ct1 <- dat1 %>%
  group_by(location, tag_id) %>%
  summarise(days = n_distinct(as.Date(datetime)), .groups = "drop") %>%
  group_by(location) %>%
  summarise(`Cumulative days detected (total)` = sum(days), .groups = "drop")


# mean days per year ------------------------------------------------------

# 5) mean ± SD per year across tag-years (location, tag_id, year)
ct2 <- dat1 %>%
  mutate(yr = year(datetime)) %>%
  group_by(location, tag_id, yr) %>%
  summarise(days = n_distinct(as.Date(datetime)), .groups = "drop") %>%
  group_by(location) %>%
  summarise(
    mean_days_per_year = mean(days),
    sd_days_per_year   = sd(days),
    .groups = "drop"
  ) %>%
  mutate(`Mean days/yr (± SD)` = sprintf("%.1f ± %.1f", mean_days_per_year, sd_days_per_year)) %>%
  select(location, `Mean days/yr (± SD)`)


# 6) final table
loc_summary <- dat1 %>%
  left_join(ct1, by = "location") %>%
  left_join(ct2, by = "location") %>%
  mutate(
    `Cumulative days detected (total)` = coalesce(`Cumulative days detected (total)`, 0L),
    `Mean days/yr (± SD)` = coalesce(`Mean days/yr (± SD)`, "0.0 ± 0.0")
  ) %>%
  arrange(location) %>% 
  distinct(location, .keep_all = TRUE)

loc_summary
