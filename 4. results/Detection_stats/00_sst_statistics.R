# libraries ---------------------------------------------------------------

pacman::p_load("tidyverse", "viridis", "ggpubr", "plotly", "sf", "rnaturalearth", "ggspatial")
rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250827_det_enviro_complete.rds")

# Filter only relevant rows first
dat1 <- dat %>%
  filter(!location %in% c("Moreton Island", "Yamba", "Forster", "Merimbula"),
         presence == 1) # only real data no pseudo-absences

# Summarise min and max SST by location and sex
sst_summary <- dat1 %>%
  group_by(location, sex) %>%
  summarise(min_sst = min(sst, na.rm = TRUE),
            max_sst = max(sst, na.rm = TRUE),
            .groups = "drop")

sst_summary
