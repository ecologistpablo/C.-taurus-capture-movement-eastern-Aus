# 2026 June 03
  # nearly there
    # cleaning up last figures and tables


rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/260329_det_enviro_complete.rds")


table <- dat %>% 
  filter(presence == 1,
         !location %in% c("Moreton Island", "Forster",
                          "Central Coast", "Naroomba", "Merimbula")) %>% 
  group_by(location, sex) %>% 
  summarise(num_movements = n(),
            mean_distance = round(mean(distance), 1),
            sd_distance = round(sd(distance), 1),
            mean_days = round(mean(num_days), 1),
            sd_days = round(sd(num_days), 1))


table            

table1 <- table %>%
  transmute(location, sex, num_movements,
    `Distance ± SD` = sprintf("%.1f ± %.1f km", mean_distance, sd_distance),
    `Days ± SD` = sprintf("%.1f ± %.1f", mean_days, sd_days))

table1
