#09 September 2023
  #we have our cur vals: Both daily and monthly values
    #let's get cur values, and anomalies into our detection dataset

rm(list=ls())
#setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

m_avg <- read_csv("Inputs/250701_CUR_m_avrg_12-22.csv") #climatological averages
det <- read_csv("Inputs/250627_SST_det.csv") #xy coords w SST data on it

m_avg <- janitor::clean_names(m_avg)

colnames(m_avg)
colnames(det)


# calculate anomalies and add enviro data to det --------------------------

det1 <- det %>%
  left_join(m_avg %>% 
              select(station_name, date, vcur, ucur, gsla, vcur_anomaly, ucur_anomaly, gsla_anomaly),
            by = c("station_name", "date")) %>% 
  select(-sst_month) # I realised I don't want this column, not needed


colnames(det1)

# save --------------------------------------------------------------------

write_csv(det1, file = "Inputs/250701_cur_det.csv")

