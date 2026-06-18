# 2nd July 2025
  # Pablo Fuenzalida
    # results for C. taurus capture paper

# libraries ---------------------------------------------------------------

pacman::p_load("tidyverse", "viridis", "ggpubr", "plotly", "lubridate", "stringr", "flextable",
               "officer", "forcats")

# Data --------------------------------------------------------------------

rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/260323_step3.rds")

# munging -----------------------------------------------------------------
location_levels <- c("Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                     "Gold Coast", "Ballina", "Evans Head", "Yamba",
                     "Coffs Harbour",
                     "Port Macquarie", "Forster",
                     "Hawks Nest", "Central Coast", "Sydney", "Illawarra", "Merimbula")

lvls <- c("Illawarra", "Sydney", "Hawks Nest", "Port Macquarie", "Coffs Harbour",
          "Evans Head", "Ballina", "Gold Coast", "North Stradbroke Island",
          "Sunshine Coast", "Wide Bay")


dat1 <- dat %>%
  filter(location %in% location_levels) %>%
  mutate(location = factor(location, levels = location_levels),
         sex = fct_recode(sex,
                          "Female" = "F",
                          "Male"   = "M"))

# ---- 2) Pair arrival/departure by tag_id + movement_id ----------------------
# One row per movement (a "stay" at a site). We take arrival/departure times,
# the site (prefer the arrival's site; fall back to departure if needed),
# and a single distance value for that movement.

tbl_summary <- dat1 %>%
  mutate(year = year(datetime),
    date = as.Date(datetime)) %>%
  group_by(location) %>%
  summarise(females = n_distinct(tag_id[sex == "Female"]),
    males = n_distinct(tag_id[sex == "Male"]),
    years_tag_detections = n_distinct(year),
    no_tag_detections = n_distinct(tag_id, datetime, station_name),
    no_receiver_stations = n_distinct(station_name),
    cumulative_days_detected = n_distinct(
      paste(tag_id, date)),
    .groups = "drop") %>%
  left_join(dat %>%
      mutate(year = year(datetime),
        date = as.Date(datetime)) %>%
      group_by(location, tag_id, year) %>%
      summarise(days_detected = n_distinct(date),
        .groups = "drop") %>%
      group_by(location) %>%
      summarise(mean_days = mean(days_detected),
        sd_days = sd(days_detected),
        .groups = "drop"),
    by = "location") %>%
  transmute(location = location,
    `Tagged C. taurus detected in region (female / male)` =
      paste0(females, " — ", males),
    `Years of tag detections` = years_tag_detections,
    `No. tag detections` = no_tag_detections,
    `No. receiver stations` = no_receiver_stations,
    `Cumulative days tagged C. taurus detected (total)` =
      cumulative_days_detected,
    `Mean ± SD number of days per year tagged C. taurus detected` =
      sprintf("%.1f ± %.1f", mean_days, sd_days))



write_csv(tbl_summary, "Outputs/Graphs/Final/detections/260603_table_one.csv")


