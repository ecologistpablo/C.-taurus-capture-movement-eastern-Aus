#06 September 2023
  #we have our sst vals: Both daily and monthly values
    #let's get sst values, and anomalies into our detection dataset

rm(list=ls())

source("/Users/owuss/Documents/USC/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("/Users/owuss/Documents/USC/Honours/R/data")

sst <- read_csv("Inputs/250211_SST_vals_12-22.csv") #sst values raw for each day
m_avg <- read_csv("Inputs/250211_SST_m_avrg_12-22.csv")  #Climatological averages for month
det <- read_csv("Inputs/250211_step8.csv") #dataframe you use to model

head(sst)
head(m_avg)
head(det)

# bind enviro dat ----------------------------------------------------------

# Using map_dfr to loop through each row of det, and bind the results into a new dataframe
det1 <- map_dfr(seq_len(nrow(det)), ~{
  row <- det[.x, ]  # Extract the current row from det
  
  # Create column names based on the detection date in det
  sst_colname <- paste0("SST_", format(row$date, "%Y%m%d"))  # For sst
  
  # Convert the detection_datetime to a monthly format and create a corresponding m_avg column name
  m_avg_colname <- paste0("MonthlyMean_", format(row$date, "%m"))  # For m_avg
  
  # Check if the column exists in sst, else return NA
  sst_value <- if (sst_colname %in% names(sst)) {
    sst[sst$location == row$location, sst_colname]  # From sst
  } else {
    NA
  }
  
  # same for monthly average
  m_avg_value <- if (m_avg_colname %in% names(m_avg)) {
    m_avg[m_avg$location == row$location, m_avg_colname]  # From m_avg
  } else {
    NA
  }
  
  # Add new columns to the current row
  row$sst <- as.numeric(sst_value)  # Adding SST
  row$sst_m_avrg <- as.numeric(m_avg_value)  # Adding SST_m_avrg
  
  return(row)  # Return the modified row
})


sum(is.na(det1$sst))
sum(is.na(det1$sst_m_avrg))

# anomaly -----------------------------------------------------------------

det2 <- det1 %>%
  mutate(sst_anomaly = sst - sst_m_avrg)

# where do the NAs go ? ---------------------------------------------------

# Create a new data frame with rows where SST is NA
NAsst <- det2 %>% 
  filter(is.na(sst)) %>% 
  group_by(location) %>%
  summarise(na_count = sum(is.na(sst)))

det3 <- det2 %>% 
  group_by(location) %>%
  summarise(sum = n())

# Join det3 and NAsst by Location and calculate the difference
diff <- det3 %>%
  left_join(NAsst, by = "location") %>% # Join on Location
  mutate(difference = sum - ifelse(is.na(na_count), 0, na_count)) # Calculate the difference

diff 
#Montague Isl & Port Macq, gone :(

# save --------------------------------------------------------------------

write_csv(det2, file = "Inputs/250211_SST_det.csv")
