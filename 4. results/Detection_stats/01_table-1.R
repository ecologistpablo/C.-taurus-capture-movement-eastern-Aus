# 2nd July 2025
  # Pablo Fuenzalida
    # results for C. taurus capture paper

# libraries ---------------------------------------------------------------

pacman::p_load("tidyverse", "viridis", "ggpubr", "plotly", "lubridate", "stringr", "flextable",
               "officer")

# Data --------------------------------------------------------------------

rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250827_step9.rds") 

# ---- 1) Filter / clean ------------------------------------------------------

dat_clean <- dat %>%
  filter(presence != 0,
         !location %in% c("Moreton Island", "Yamba", "Forster", "Central Coast", "Merimbula")) %>%
  mutate(
    sex  = recode(sex, "M" = "Male", "F" = "Female"),
    date = as.Date(datetime),
    location = fct_relevel(location,
                                  "Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                                  "Gold Coast", "Ballina", "Evans Head", "Coffs Harbour",
                                  "Port Macquarie", "Hawks Nest", 
                                  "Sydney", "Illawarra")) 

# ---- 2) Pair arrival/departure by tag_id + movement_id ----------------------
# One row per movement (a "stay" at a site). We take arrival/departure times,
# the site (prefer the arrival's site; fall back to departure if needed),
# and a single distance value for that movement.
stays <- dat_clean %>%
  arrange(tag_id, movement_id, datetime) %>%
  group_by(tag_id, movement_id) %>%
  summarise(
    sex        = first(sex),
    # site from arrival if present, otherwise from the other row
    location_a = location[movement == "arrival"] |> first(default = NA_character_),
    location_d = location[movement == "departure"] |> first(default = NA_character_),
    location   = coalesce(location_a, location_d),
    arrival_dt = datetime[movement == "arrival"]   |> first(default = NA_POSIXct_),
    depart_dt  = datetime[movement == "departure"] |> first(default = NA_POSIXct_),
    distance   = distance |> discard(is.na) |> first(default = NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    duration_days = as.numeric(difftime(depart_dt, arrival_dt, units = "days")),
    # keep rows that have at least a site and an arrival; departure may be missing
    keep = !is.na(location) & !is.na(arrival_dt)
  ) %>%
  filter(keep) %>%
  select(-keep, -location_a, -location_d)

# ---- 3) Summary table by location × sex -------------------------------------
fmt_range <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_character_ 
  else sprintf("%s to %s", round(min(x), digits), round(max(x), digits))
}

fmt_mean_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_character_ 
  else sprintf("%s ± %s", round(mean(x), digits), round(sd(x), digits))
}

# ---- 3a) Detection timeframe for each location × sex ------------------------
select_for_timeframe <- dat_clean %>%
  group_by(location, sex) %>%
  summarise(
    .time_min = min(date, na.rm = TRUE),
    .time_max = max(date, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 3b) Summary table from stays -------------------------------------------
table_out <- stays %>%
  group_by(location, sex) %>%
  summarise(
    `Unique transmitter IDs detected`      = n_distinct(tag_id),
    `Num. of movements detected`           = n_distinct(movement_id),
    `Movement distance (km) - Range`       = fmt_range(distance),
    `Movement distance (km) - Mean ± SD`   = fmt_mean_sd(distance),
    `Movement duration (days) - Range`     = fmt_range(duration_days),
    `Movement duration (days) - Mean ± SD` = fmt_mean_sd(duration_days),
    .groups = "drop") %>%
  left_join(select_for_timeframe, by = c("location", "sex")) %>%
  mutate(
    `Detection Timeframe` = ifelse(is.na(.time_min), NA_character_,
                                   paste(.time_min, "to", .time_max))
  ) %>%
  select(location, sex,
         `Unique transmitter IDs detected`,
         `Detection Timeframe`,
         `Num. of movements detected`,
         `Movement distance (km) - Range`,
         `Movement distance (km) - Mean ± SD`,
         `Movement duration (days) - Range`,
         `Movement duration (days) - Mean ± SD`) %>%
  arrange(location, sex)
table_out

write_csv(table_out, "Outputs/Graphs/Final/detections/250828_table_one.csv")


#make a flextable
ft <- flextable(table_out) %>%
  fontsize(size = 5, part = "all") %>%   # shrink everything
  fontsize(size = 5, part = "header") %>% # slightly bigger headers
  autofit()

# create a Word doc
doc <- read_docx() %>%
  body_add_flextable(ft)

# save
print(doc, target = "Outputs/Graphs/Final/detections/250828_table_one.xlsx")


