# 2024 02 21
  # abacus plots of tag IDs to show total detections

library(tidyverse)
rm(list=ls()) 

setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/260323_step3.rds") 

lvls <- c("Illawarra", "Sydney", "Hawks Nest", "Forster", "Port Macquarie", "Coffs Harbour", "Yamba",
          "Evans Head", "Ballina", "Gold Coast", "North Stradbroke Island",
          "Sunshine Coast", "Wide Bay")

dat1 <- dat %>%
  filter(location %in% lvls) %>%
  mutate(location = factor(location, levels = lvls),
         date = as_date(datetime),
         year = year(date),
         tag_id = as_factor(tag_id),
         location = recode(location, "Hawks Nest" = "Hunter")) %>% 
  filter(!year == 2026) %>% 
  distinct(station_name, date, tag_id, .keep_all = T) %>% 
  summarise(num_det = n(), .by = c(location, date, tag_id))


  
# Plotting
a <- 
  ggplot(dat1, aes(x = date, y = location, colour = tag_id)) +
  geom_point(aes(size = num_det), alpha = 0.5) + 
  theme_bw() +
  labs(x = NULL, y = NULL, colour = "Transmitter ID", size = "Number of Detections") +
  scico::scale_color_scico_d(palette = "lajolla")+
  #scale_colour_viridis_d(direction = -1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-05-01", "2025-12-31")))  

a

#save
ggsave(path = "outputs/Graphs/Final/detections", "260604_abacus.png",
       plot = a, width = 10, height = 8) #in inches because gg weird

