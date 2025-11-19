
pacman::p_load("tidyverse", "ggpubr")
rm(list=ls()) 
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250827_step9.rds") %>%  #residence events
  filter(presence == 1)

dat1 <- dat %>%
  filter(!location %in% c("Moreton Island", "Yamba", "Forster", "Central Coast", "Merimbula"),
         !str_starts(location, "deg_")) %>%   # drop these sites)
  mutate(location = fct_relevel(location,
                                "Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                                "Gold Coast", "Ballina", "Evans Head", "Coffs Harbour",
                                "Port Macquarie", "Hawks Nest", 
                                "Sydney", "Illawarra")) 


# Table 2 analysis --------------------------------------------------------

movement_summary <- dat1 %>%
  group_by(location, sex) %>% 
  summarise(`Movement distance (km) - Range` = sprintf("%.2f–%.2f", min(distance, na.rm = TRUE), max(distance, na.rm = TRUE)),
    `Movement distance (km) - Mean ± SD` = sprintf("%.2f ± %.2f",
                                                   mean(distance, na.rm = TRUE),
                                                   sd(distance, na.rm = TRUE)),
    `Movement duration (days) - Range` = sprintf("%.2f–%.2f", min(num_days, na.rm = TRUE), max(num_days, na.rm = TRUE)),
    `Movement duration (days) - Mean ± SD` = sprintf("%.2f ± %.2f",
                                                     mean(num_days, na.rm = TRUE),
                                                     sd(num_days, na.rm = TRUE)))

movement_summary
