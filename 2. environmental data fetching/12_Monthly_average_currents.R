#86 September 2023
  #we have our cur vals
    #lets generate monthly averages

rm(list=ls())
#setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

library(tidyverse)

# pts ---------------------------------------------------------------------

setwd("~/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250728_Currents_vals_12-24.csv")

# wrestle monthly averages ------------------------------------------------

# 1. Monthly averages per station/year/month
monthly_avg <- dat %>%
  mutate(year = year(date), month = sprintf("%02d", month(date))) %>%
  group_by(station_name, year, month) %>%
  summarise(mean_GSLA = mean(GSLA, na.rm = TRUE),
    mean_UCUR = mean(UCUR, na.rm = TRUE),
    mean_VCUR = mean(VCUR, na.rm = TRUE),
    .groups = "drop")

# 2. Climatological monthly averages (across all years)
monthly_climatology <- monthly_avg %>%
  group_by(station_name, month) %>%
  summarise(clim_GSLA = mean(mean_GSLA, na.rm = TRUE),
    clim_UCUR = mean(mean_UCUR, na.rm = TRUE),
    clim_VCUR = mean(mean_VCUR, na.rm = TRUE),
    .groups = "drop")

# 3. Join to compute temporal anomalies
dat1 <- dat %>%
  mutate(month = sprintf("%02d", month(date))) %>%
  left_join(monthly_climatology, by = c("station_name", "month")) %>%
  mutate(GSLA_anomaly = GSLA - clim_GSLA,
    UCUR_anomaly = UCUR - clim_UCUR,
    VCUR_anomaly = VCUR - clim_VCUR) %>%
  select(-month)

head(dat1)

# save --------------------------------------------------------------------

write_csv(dat1, file = "Inputs/250728_CUR_m_avrg_12-24.csv")
