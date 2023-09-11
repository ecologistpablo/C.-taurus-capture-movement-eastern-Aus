#06 September 2023
  #we have our sst vals: Both daily and monthly values
    #let's get sst values, and anomalies into our detection dataset

rm(list=ls())
#setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

sst <- read_csv("Inputs/230909_SST_vals_12-22.csv")
m_avg <- read_csv("Inputs/230909_SST_m_avrg_12-22.csv")
det <- read_csv("Inputs/230907_step8.csv")

head(sst)
head(m_avg)
head(det)


# bind enviro dat ----------------------------------------------------------

# Using map_dfr to loop through each row of det, and bind the results into a new dataframe
det1 <- map_dfr(seq_len(nrow(det)), ~{
  row <- det[.x, ]  # Extract the current row from det
  
  # Create column names based on the detection date in det
  sst_colname <- paste0("SST_", format(row$detection_datetime, "%Y%m%d"))  # For sst
  
  # Convert the detection_datetime to a monthly format and create a corresponding m_avg column name
  m_avg_colname <- paste0("MonthlyMean_", format(row$detection_datetime, "%m"))  # For m_avg
  
  # Check if the column exists in sst, else return NA
  sst_value <- if (sst_colname %in% names(sst)) {
    sst[sst$station_name == row$station_name, sst_colname]  # From sst
  } else {
    NA
  }
  
  # same for monthly average
  m_avg_value <- if (m_avg_colname %in% names(m_avg)) {
    m_avg[m_avg$station_name == row$station_name, m_avg_colname]  # From m_avg
  } else {
    NA
  }
  
  # Add new columns to the current row
  row$SST <- as.numeric(sst_value)  # Adding SST
  row$SST_m_avrg <- as.numeric(m_avg_value)  # Adding SST_m_avrg
  
  return(row)  # Return the modified row
})


sum(is.na(det1$SST))
sum(is.na(det1$SST_m_avrg))

# AND ANOMALYYYY ----------------------------------------------------------

det2 <- det1 %>%
  mutate(SST_anomaly = SST_m_avrg - SST)


# where do the NAs go ? ---------------------------------------------------

# Create a new data frame with rows where SST is NA
NAsst <- det2 %>% filter(is.na(SST)) %>% 
  group_by(Location) %>%
  summarise(na_count = sum(is.na(SST)))

unique(NAsst$Location)

det3 <- det2 %>% 
  group_by(Location) %>%
  summarise(sum = n())

# Join det3 and NAsst by Location and calculate the difference
diff <- det3 %>%
  left_join(NAsst, by = "Location") %>% # Join on Location
  mutate(difference = sum - ifelse(is.na(na_count), 0, na_count)) # Calculate the difference

#Montague Isl & Port Macq, gone :(

# save --------------------------------------------------------------------

write_csv(det2, file = "Inputs/230911_SST_det.csv")

ggplot(det2, aes(x = month, y = SST_anomaly, colour = movement)) +
  geom_jitter(width = 0.2) +
  facet_wrap(~Location)

