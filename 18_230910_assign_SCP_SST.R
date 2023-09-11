#10 September 2023
  #we have our sst vals: Both daily and monthly values
    #let's get sst values, and anomalies into our detection dataset
  #for SCP data this time

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

sst <- read_csv("Inputs/230911_capture_SST.csv")
m_avg <- read_csv("Inputs/230911_capture_SST_m_avrg.csv")
SCP <- read_csv("shark control/230910_SCP_complete_12-22.csv")

head(sst)
head(m_avg)
head(SCP)

SCP <- SCP %>% 
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Area = as.character(Area),
    Location = as.character(Location),
    IsAlive = as.character(IsAlive),
    Length = as.numeric(Length),
    Sex = as.character(Sex),
    GearType = as.character(GearType),
    `Dist to land` = as.character(`Dist to land`),
    Bathy = as.numeric(Bathy),
    Bait = as.character(Bait),
    `Time to respond` = as.character(`Time to respond`),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )


# row numbers -------------------------------------------------------------

# Initialize row numbers
SCP$rownum <- seq_len(nrow(SCP))
m_avg$rownum <- seq_len(nrow(m_avg))
sst$rownum <- seq_len(nrow(sst))

# processing --------------------------------------------------------------

# Processing
SCP1 <- map_dfr(seq_len(nrow(SCP)), function(.x) {
  
  row <- SCP[.x, ]  # Extract the current row from SCP
  
  # Initialize a list to hold new columns
  new_columns <- list()
  
  # Create column names based on the detection date in SCP
  sst_colname <- paste0("SST_", format(row$Date, "%Y%m%d"))
  m_avg_colname <- paste0("MonthlyMean_", format(row$Date, "%m"))
  
  # Check if the column exists in sst, else return NA
  sst_value <- if (sst_colname %in% names(sst)) {
    temp <- sst[sst$rownum == .x, sst_colname]  # Using rownum for direct row mapping
    if(length(temp) > 1) warning("Multiple sst values found for a single row")
    as.numeric(unlist(temp))
  } else {
    NA_real_
  }
  
  # Check if the column exists in m_avg, else return NA
  m_avg_value <- if (m_avg_colname %in% names(m_avg)) {
    temp <- m_avg[m_avg$rownum == .x, m_avg_colname]  # Using rownum for direct row mapping
    if(length(temp) > 1) warning("Multiple m_avg values found for a single row")
    as.numeric(unlist(temp))
  } else {
    NA_real_
  }
  
  # Calculate the anomaly
  anomaly <- m_avg_value - sst_value
  
  # Add new columns to the current row
  new_columns[["SST"]] <- sst_value
  new_columns[["SST_m_avrg"]] <- m_avg_value
  new_columns[["SST_anomaly"]] <- anomaly
  
  # Combine the existing row with new columns
  out_row <- bind_cols(row, as_tibble(new_columns))
  
  return(out_row)
})



sum(is.na(SCP1$SST))
sum(is.na(SCP1$SST_m_avrg))

# # where do the NAs go ? ---------------------------------------------------
# 
# # Create a new data frame with rows where SST is NA
# NAsst <- SCP2 %>% filter(is.na(SST)) %>% 
#   group_by(Location) %>%
#   summarise(na_count = sum(is.na(SST)))
# 
# unique(NAsst$Location)
# 
# SCP3 <- SCP2 %>% 
#   group_by(Location) %>%
#   summarise(sum = n())
# 
# # Join SCP3 and NAsst by Location and calculate the difference
# diff <- SCP3 %>%
#   left_join(NAsst, by = "Location") %>% # Join on Location
#   mutate(difference = sum - ifelse(is.na(na_count), 0, na_count)) # Calculate the difference

# save --------------------------------------------------------------------

write_csv(SCP1, file = "Inputs/230911_SST_SCP.csv")

