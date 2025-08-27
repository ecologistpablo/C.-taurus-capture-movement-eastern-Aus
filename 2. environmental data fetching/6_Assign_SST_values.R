#06 September 2023
  #we have our sst vals: Both daily and monthly values
    #let's get sst values, and anomalies into our detection dataset

rm(list=ls())

library(tidyverse)
setwd("/Users/owuss/Documents/USC/Honours/R/data")

m_avg <- read_rds("Inputs/250827_SST_m_avrg_12-24.rds")  #Climatological averages for month
det <- read_rds("Inputs/250827_step9.rds") #dataframe you use to model

summary(det)
summary(m_avg)
 
unique(det$station_name) # are they the same
unique(m_avg$station_name) # in the number of unique stations ? 

# if not, you have a problem
# if they are, you don't

det <- det %>%
  mutate(date = as.Date(datetime))


# left_join to the rescue -------------------------------------------------

# combine SST data with our detections
det1 <- det %>%
  left_join(m_avg %>% 
      select(station_name, date, SST, sst_month, sst_anomaly),
    by = c("station_name", "date")) %>% 
  rename(sst = SST)

str(det1$sst)

# where do the NAs go ? ---------------------------------------------------

# Create a new data frame with rows where SST is NA
NAsst <- det1 %>% 
  filter(is.na(sst)) %>% 
  group_by(location) %>%
  summarise(na_count = sum(is.na(sst)))

 det1 %>% 
  group_by(location) %>%
  summarise(sum = n()) %>% 
  left_join(NAsst, by = "location") %>% # Join on Location
  mutate(difference = sum - ifelse(is.na(na_count), 0, na_count)) # Calculate the difference

# all focal locations still have plenty of data 
# NAs are not an enormous problem

# save --------------------------------------------------------------------

write_rds(det1, file = "Inputs/250827_SST_det.rds")
