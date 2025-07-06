# 2nd July 2025
  # Pablo Fuenzalida
    # results for C. taurus capture paper

# libraries ---------------------------------------------------------------

pacman::p_load("tidyverse", "viridis", "ggpubr", "plotly", "lubridate", "stringr", "flextable",
               "officer")

# Data --------------------------------------------------------------------

rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250701_det_enviro_complete.csv")

# get munging -------------------------------------------------------------

dat1 <- dat %>% 
  filter(location %in% c("Wolf Rock", "Flat Rock", "Coffs Harbour", "Hawks Nest", "Sydney")) %>% 
  mutate(location = factor(location, levels = c("Wolf Rock",
                  "Flat Rock", "Coffs Harbour", "Hawks Nest", "Sydney")))
  
dat2 <- dat1 %>%
  mutate(sex = recode(sex, "M" = "Male", "F" = "Female"),
    movement_duration = as.numeric(abs(departure_date - arrival_date))) %>%
  group_by(location, sex) %>%
  summarise(`Unique transmitter IDs detected` = n_distinct(tag_id),
    `Detection Timeframe` = paste(min(date, na.rm = TRUE), "to", max(date, na.rm = TRUE)),
    
    # Split movement count
    `Num. of movements detected - Arrival` = sum(!is.na(arrival_date)),
    `Num. of movements detected - Departure` = sum(!is.na(departure_date)),
    
    # Distance
    `Movement distance (km) - Range` = str_c(min(distance, na.rm = TRUE),
                                             " to ", max(distance, na.rm = TRUE)),
    `Movement distance (km) - Mean ± SD` = str_c(round(mean(distance, na.rm = TRUE)), 
                                                 " ± ", 
                                                 round(sd(distance, na.rm = TRUE))),

    # Duration
    `Movement duration (days) - Range` = {
      dur <- movement_duration[!is.na(movement_duration)]
      if (length(dur) == 0) NA else str_c(min(dur), " to ", max(dur))
    },
    `Movement duration (days) - Mean ± SD` = {
      dur <- movement_duration[!is.na(movement_duration)]
      if (length(dur) == 0) NA else str_c(round(mean(dur)), " ± ", round(sd(dur)))
    },
    .groups = "drop")


# Convert tibble to microsoft table ---------------------------------------

table <- flextable(dat2) #tibble to table

read_docx() %>%
  body_add_flextable(table) %>%
  print(target = "outputs/Graphs/Final/250702_c.taurus_table_1.docx")
