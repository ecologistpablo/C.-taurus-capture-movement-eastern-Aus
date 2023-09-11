#09 September 2023
  #we have our cur vals: Both daily and monthly values
    #let's get cur values, and anomalies into our detection dataset

rm(list=ls())
#setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

cur <- read_csv("Inputs/230911_Currents_vals_12-22.csv")
m_avg <- read_csv("Inputs/230911_CUR_m_avrg_12-22.csv")
det <- read_csv("Inputs/230911_SST_det.csv")

head(cur)
head(m_avg)
head(det)


# calculate anomalies and add enviro data to det --------------------------

# Using map_dfr to loop through each row of det, and bind the results into a new dataframe
det1 <- map_dfr(seq_len(nrow(det)), ~{
  row <- det[.x, ]  # Extract the current row from det
  
  for(prefix in c("GSLA", "UCUR", "VCUR")) { #what prefixes are we using
    
    # Create column names based on the detection date in det
    cur_colname <- paste0(prefix, "_", format(row$detection_datetime, "%Y%m%d"))  # For cur
    m_avg_colname <- paste0("MonthlyMean_", format(row$detection_datetime, "%m"))  # For m_avg
    
    # Check if the column exists in cur, else return NA
    cur_value <- if (cur_colname %in% names(cur)) {
      temp <- cur[cur$station_name == row$station_name, cur_colname]
      if (nrow(temp) == 0) {
        warning("No rows found in cur for ", cur_colname)
        NA_real_
      } else {
        as.numeric(unlist(temp))
      }
    } else {
      NA_real_
    }
    
    # Check if the column exists in m_avg, else return NA
    m_avg_value <- if (m_avg_colname %in% names(m_avg)) {
      temp <- m_avg[m_avg$station_name == row$station_name, m_avg_colname]
      if (nrow(temp) == 0) {
        warning("No rows found in m_avg for ", m_avg_colname)
        NA_real_
      } else {
        as.numeric(unlist(temp))
      }
    } else {
      NA_real_
    }
    
    # Calculate the anomaly
    anomaly <- m_avg_value - cur_value
    
    anomaly <- m_avg_value - cur_value
    
    # Add new columns to the current row
    row[paste0("cur_", prefix)] <- as.numeric(cur_value)  # Adding cur with prefix
    row[paste0("cur_m_avrg_", prefix)] <- as.numeric(m_avg_value)  # Adding cur_m_avrg with prefix
    row[paste0("anomaly_", prefix)] <- as.numeric(anomaly)  # Adding anomaly with prefix
  }
  
  return(row)  # Return the modified row
})

# Checking for NAs
sum(is.na(det1 %>% dplyr::select(starts_with("cur_V"))))
sum(is.na(det1 %>% dplyr::select(starts_with("cur_m_avrg_"))))
sum(is.na(det1 %>% dplyr::select(starts_with("anomaly_"))))


# where do the NAs go ? ---------------------------------------------------

# Define a list of prefixes to loop over
prefixes <- c("GSLA", "UCUR", "VCUR")

# Create an empty data frame to store the aggregated results
agg_df <- data.frame(Location = character(0), na_count_GSLA = numeric(0))

# Loop over each prefix to filter, group, and summarize data
for (prefix in prefixes) {
  
  cur_col <- paste0("cur_", prefix)
  na_col <- paste0("na_count_", prefix)
  
  print(paste0("Checking for NA in ", cur_col))
  
  # Create a new data frame with rows where cur is NA
  NAcur <- det1 %>% 
    filter(is.na(!!sym(cur_col))) %>% 
    group_by(Location) %>% 
    summarise(!!na_col := sum(is.na(!!sym(cur_col))))
  
  print(NAcur)
  
  # Append to the aggregated data frame
  if (nrow(agg_df) == 0) {
    agg_df <- NAcur
  } else {
    agg_df <- full_join(agg_df, NAcur, by = "Location")
  }
}

# Summarize det2 by Location
det2 <- det1 %>% 
  group_by(Location) %>% 
  summarise(sum = n())

# Join det3 and agg_df by Location and calculate the difference
diff <- det2 %>%
  left_join(agg_df, by = "Location") %>% # Join on Location
  mutate(across(starts_with("na_count_"), ~sum - .x, .names = "difference_{.col}"))

print(diff)

# save --------------------------------------------------------------------

write_csv(det1, file = "Inputs/230911_cur_det.csv")

MI <- det1 %>% 
  filter(Location == "Moreton Island")
