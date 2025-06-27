#06 September 2023
  #we have our sst vals: Both daily and monthly values
    #let's get sst values, and anomalies into our detection dataset

rm(list=ls())

library(tidyverse)
setwd("/Users/owuss/Documents/USC/Honours/R/data")

m_avg <- read_csv("Inputs/250627_SST_m_avrg_12-22.csv")  #Climatological averages for month
det <- read_csv("Inputs/250626_step10.csv") #dataframe you use to model

summary(det)
summary(m_avg)

# rm duplicates -----------------------------------------------------------

# now remember, we had duplicate names in our station_name column
# so we need to clear that up, by using the same function
unique(det$station_name) #200 cols, should be 237

# Identify station_names with more than one lat/lon combo
station_duplicates <- det %>%
  distinct(station_name, latitude, longitude) %>%
  add_count(station_name) %>%
  filter(n > 1) %>%
  pull(station_name)

# Fix duplicates by suffixing only where needed
det1 <- det %>%
  group_by(station_name) %>%
  mutate(station_name = case_when(
      station_name %in% station_duplicates ~ paste0(
        station_name, "_", match(interaction(latitude, longitude),
                                 unique(interaction(latitude, longitude)))),
      TRUE ~ station_name)) %>%
  ungroup()

unique(det1$station_name) #did it work?
setequal(unique(det1$station_name), unique(m_avg$station_name)) # are two dfs the same?
# if this is false, you have a problem

# left_join to the rescue -------------------------------------------------

# combine SST data with our detections
det2 <- det1 %>%
  left_join(m_avg %>% 
      select(station_name, date, SST, sst_month, sst_anomaly),
    by = c("station_name", "date")) %>% 
  rename(sst = SST)
head(det2)

# where do the NAs go ? ---------------------------------------------------

# Create a new data frame with rows where SST is NA
NAsst <- det2 %>% 
  filter(is.na(sst)) %>% 
  group_by(location) %>%
  summarise(na_count = sum(is.na(sst)))

 det2 %>% 
  group_by(location) %>%
  summarise(sum = n()) %>%
  left_join(NAsst, by = "location") %>% # Join on Location
  mutate(difference = sum - ifelse(is.na(na_count), 0, na_count)) # Calculate the difference

# all focal locations still have plenty of data 
# NAs are not an enormous problem

# save --------------------------------------------------------------------

write_csv(det2, file = "Inputs/250211_SST_det.csv")
