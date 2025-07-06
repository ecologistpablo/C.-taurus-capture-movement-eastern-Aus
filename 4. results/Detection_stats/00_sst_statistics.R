# libraries ---------------------------------------------------------------

pacman::p_load("tidyverse", "viridis", "ggpubr", "plotly", "sf", "rnaturalearth", "ggspatial")

# Data --------------------------------------------------------------------

rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250701_det_enviro_complete.csv")

# Filter only relevant rows first
dat1 <- dat %>%
  filter(location %in% c("Wolf Rock", "Flat Rock", "Coffs Harbour", "Hawks Nest", "Sydney"),
         presence == 1) # only real data no pseudo-absences

# Summarise min and max SST by location and sex
sst_summary <- dat1 %>%
  group_by(location, sex) %>%
  summarise(min_sst = min(sst, na.rm = TRUE),
            max_sst = max(sst, na.rm = TRUE),
            .groups = "drop")

sst_summary
