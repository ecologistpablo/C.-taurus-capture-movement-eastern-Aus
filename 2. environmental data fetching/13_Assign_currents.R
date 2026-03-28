#09 September 2023
  #we have our cur vals: Both daily and monthly values
    #let's get cur values, and anomalies into our detection dataset

# Packages ----------------------------------------------------------------

library(tidyverse)

# pts ---------------------------------------------------------------------

setwd("/Users/owuss/Documents/USC/Honours/R/data")
rm(list=ls())
m_avg <- read_rds("Inputs/260329_CUR_m_avrg_12-25.rds") #climatological averages
det <- read_rds("Inputs/260324_SST_det.rds") #xy coords w SST data on it

m_avg <- janitor::clean_names(m_avg)
colnames(m_avg)
colnames(det)

# calculate anomalies and add enviro data to det --------------------------

det1 <- det %>%
  left_join(m_avg %>%  # left join for da win
              select(station_name, date, vcur, ucur, gsla, # raw values
                     vcur_anomaly, ucur_anomaly, gsla_anomaly), # anomalies
            by = c("station_name", "date")) %>%  # based on station and date
  select(-sst_month) # I realised I don't want this column, not needed


colnames(det1)

# save --------------------------------------------------------------------

write_rds(det1, file = "Inputs/260329_cur_det.rds")

