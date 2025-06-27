#06 September 2023
  #we have our sst vals
    #lets generate monthly averages

rm(list=ls())

# Packages ----------------------------------------------------------------

library(tidyverse)

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/250627_SST_vals_12-22.csv")

# wrestle monthly averages ------------------------------------------------

monthly_avg <- dat %>%
  mutate(year = year(date), month = sprintf("%02d", month(date))) %>%
  group_by(station_name, year, month) %>%
  summarise(mean_SST = mean(SST, na.rm = TRUE), .groups = "drop")

monthly_climatology <- monthly_avg %>%
  group_by(station_name, month) %>%
  summarise(climatology_SST = mean(mean_SST, na.rm = TRUE), .groups = "drop")


# join --------------------------------------------------------------------

dat1 <- dat %>%
  mutate(month = month(date)) %>%
  left_join(dat %>%
      mutate(month = month(date)) %>%
      group_by(station_name, month) %>%
      summarise(sst_month = mean(SST, na.rm = TRUE), .groups = "drop"),
    by = c("station_name", "month")) %>%
  mutate(sst_anomaly = SST - sst_month) %>% 
  select(-month)

# save --------------------------------------------------------------------

write_csv(dat1, file = "Inputs/250627_SST_m_avrg_12-22.csv")
