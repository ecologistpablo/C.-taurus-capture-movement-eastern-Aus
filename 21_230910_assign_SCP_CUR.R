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

cur <- read_csv("Inputs/230910_capture_CUR.csv")
m_avg <- read_csv("Inputs/230910_capture_CUR_m_avrg.csv")
SCP <- read_csv("Inputs/230910_SST_SCP.csv")

head(cur)
head(m_avg)
head(SCP)
str(SCP)

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

str(SCP)


#initialise row numbers
SCP$rownum <- seq_len(nrow(SCP))
m_avg$rownum <- seq_len(nrow(m_avg))
cur$rownum <- seq_len(nrow(cur))

det <- SCP

# pair currents w captures ------------------------------------------------


# Processing
det1 <- map_dfr(seq_len(nrow(det)), function(.x) {
  
  row <- det[.x, ]  # Extract the current row from det
  
  # Initialize a list to hold new columns
  new_columns <- list()
  
  for(prefix in c("GSLA", "UCUR", "VCUR")) {  # What prefixes are we using
    
    # Create column names based on the detection date in det
    cur_colname <- paste0(prefix, "_", format(row$Date, "%Y%m%d"))
    m_avg_colname <- paste0("MonthlyMean_", format(row$Date, "%m"))
    
    # Check if the column exists in cur, else return NA
    cur_value <- if (cur_colname %in% names(cur)) {
      temp <- cur[cur$rownum == .x, cur_colname]  # Using rownum for direct row mapping
      if(length(temp) > 1) warning("Multiple cur values found for a single row")
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
    anomaly <- m_avg_value - cur_value
    
    # Add new columns to the current row
    new_columns[[paste0("cur_", prefix)]] <- cur_value
    new_columns[[paste0("cur_m_avrg_", prefix)]] <- m_avg_value
    new_columns[[paste0("anomaly_", prefix)]] <- anomaly
  }
  
  # Combine the existing row with new columns
  out_row <- bind_cols(row, as_tibble(new_columns))
  
  return(out_row)
})

# Checking for NAs
sum(is.na(det1 %>% dplyr::select(starts_with("cur_"))))
sum(is.na(det1 %>% dplyr::select(starts_with("cur_m_avrg_"))))
sum(is.na(det1 %>% dplyr::select(starts_with("anomaly_"))))

#That worked splendidly, but we have quite a few NAs (again)


# same location == same value ---------------------------------------------

#some rows have NAs with the same Location as other rows
#which means there are values nearby but we didn't pick them up
#lets interpolate

# Group by location and fill in NA values
det2 <- det1 %>%
  group_by(Location) %>%
  summarise(across(everything(), ~ {
    first_value <- first(.[!is.na(.)], default = NA)
    ifelse(is.na(.), first_value, .)
  })) %>%
  ungroup()

sum(is.na(det1)) - sum(is.na(det2))

#fill 202 values



# where do the NAs go ? ---------------------------------------------------

# Create a new data frame with rows where SST is NA
NAcur <- det2 %>% filter(is.na(cur_GSLA)) %>% 
  group_by(Location) %>%
  summarise(na_count = sum(is.na(cur_GSLA)))

unique(NAcur$Location)

det3 <- det2 %>% 
  group_by(Location) %>%
  summarise(sum = n())

# Join SCP3 and NAsst by Location and calculate the difference
diff <- det3 %>%
  left_join(NAcur, by = "Location") %>% # Join on Location
  mutate(difference = sum - ifelse(is.na(na_count), 0, na_count)) # Calculate the difference

#Montague Isl & Port Macq, gone :(

# save --------------------------------------------------------------------

write_csv(det2, file = "Inputs/230910_SCP_enviro.csv")

