#14.08.23
  #data anomaly synthesization

rm(list=ls())

#listen to your supervisors:
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/230808_step7.csv")
receiver_stations <- read_csv("Inputs/receiver_station_XY_230822.csv")
#load("Inputs/230813_SCP54.RData")
#dat <- read_csv("Inputs/JB17_230817.csv")

unique(dat$station_name)
#we have 132 stations to create averages for
#Then subtract the values of the date and point in time and then bring it back
dat$detection_datetime <- as.Date(dat$detection_datetime, "%d/%m/%Y")
dat <- subset(dat, Location != "Montague Island")

# receiver munging --------------------------------------------------------

unique(dat$station_name)

#same df
dat1 <- dat

#eg
JB17 <- dat %>% 
  filter(station_name == "JB17")

#take two of munging a df
generate_pseudo_absences <- function(data, unique_lat, unique_lon) {
  
  # Generate a sequence of dates from 2012 to 2022
  random_dates <- seq.Date(
    from = as.Date("2012-01-01"),
    to = as.Date("2022-12-31"),
    by = "day"
  )
  
  # Create the pseudo-absence rows for each date
  pseudo_absence_rows <- expand.grid(
    detection_datetime = random_dates,
    Presence = 0
  )
  
  # Merge with the original data to get lat/lon values
  data_with_latlon <- merge(pseudo_absence_rows, data, by = "detection_datetime", "Presence", all.x = TRUE)
  
  # Replace NAs in receiver_deployment_longitude with unique_lon values
  data_with_latlon$receiver_deployment_longitude <- ifelse(
    is.na(data_with_latlon$receiver_deployment_longitude),
    unique_lon,
    data_with_latlon$receiver_deployment_longitude
  )
  
  # Replace NAs in receiver_deployment_latitude with unique_lat values
  data_with_latlon$receiver_deployment_latitude <- ifelse(
    is.na(data_with_latlon$receiver_deployment_latitude),
    unique_lat,
    data_with_latlon$receiver_deployment_latitude
  )
  
  # Select only the relevant columns
  result <- data_with_latlon %>%
    dplyr::select(detection_datetime, Presence, everything())
  
  return(result)
}

# Calculate unique latitude and longitude values
unique_lat <- unique(JB17$receiver_deployment_latitude)
unique_lon <- unique(JB17$receiver_deployment_longitude)

# Usage
JB17a <- generate_pseudo_absences(JB17, unique_lat, unique_lon)


JB17a <- as_tibble(JB17a)

# round 2 -----------------------------------------------------------------

#127 elements

# Function to expand dates
expand_dates <- function(df) {
  months <- unique(month(df$detection_datetime))
  all_dates <- list()
  
  for (m in months) {
    for (y in 2012:2023) {
      
      # For July 2006 onward
      if (y == 2012 && m < 1) next
      
      # Up to July 2023
      if (y == 2023 && m > 7) next
      
      start_date <- as.Date(paste0(y, "-", m, "-01"))
      end_date <- as.Date(paste0(y, "-", m, "-", days_in_month(start_date)))  # Get last day of that month
      month_dates <- seq.Date(from = start_date, to = end_date, by = "day")
      all_dates <- c(all_dates, list(month_dates))
    }
  }
  
  # Unlist to get a single vector of dates
  expanded_dates <- sort(unique(do.call(c, all_dates)))
  
  # Create a character representation of the unique months for the station
  unique_month_id <- paste(sort(months), collapse = ",")
  
  # Merge the original data attributes with expanded dates, and add the unique_month_id column
  df_expanded <- data.frame(
    detection_datetime = expanded_dates,
    station_name = rep(unique(df$station_name), length.out = length(expanded_dates)),
    receiver_deployment_latitude = rep(unique(df$receiver_deployment_latitude), length.out = length(expanded_dates)),
    receiver_deployment_longitude = rep(unique(df$receiver_deployment_longitude), length.out = length(expanded_dates)),
    unique_month_id = rep(unique_month_id, length.out = length(expanded_dates))
  )
  
  # Assuming there might be more columns in 'dat', we should merge those as well
  other_columns <- df[1, !(names(df) %in% c("detection_datetime", "station_name", "receiver_deployment_latitude", "receiver_deployment_longitude", "unique_month_id"))]
  for(colname in names(other_columns)) {
    df_expanded[[colname]] <- rep(other_columns[[colname]], length.out = length(expanded_dates))
  }
  
  return(df_expanded)
}

# Split the full 'dat' data by station_name
list_of_dfs <- split(dat, dat$station_name)

# Apply the function to each station's dataframe
expanded_dfs <- lapply(list_of_dfs, expand_dates)

# Assign each dataframe to its respective station name
for(station in names(expanded_dfs)) {
  assign(station, expanded_dfs[[station]])
}


# number of rows in all dfs -----------------------------------------------


observation_counts <- sapply(expanded_dfs, nrow)

# To get the total number of observations across all dataframes
total_observations <- sum(observation_counts)
print(total_observations)

#452,418

#It takes approx. 2 hrs to process 1000 points for rs_sst, and rs_sst_interpolated

439968/1000 * 2

879 / 24
#131.95 days of processing data if we go down this route
#so its gonna be a lot quicker to access the L3S monthly product outside of remora, extract values and avrge
#however, rs_currents will need to follow this approach

# tibble creating ---------------------------------------------------------

merged_data <- lapply(merged_data, as_tibble)


list2env(list_of_tibbles, envir = .GlobalEnv)

# extractEnv --------------------------------------------------------------



JB17 <- dat %>%
  filter(station_name == "JB17") %>%
  select(detection_datetime, receiver_deployment_longitude, receiver_deployment_latitude, station_name)


#unlooped test
MD <- extractEnv(df = dat[1:50,],
                        X = "receiver_deployment_longitude", 
                        Y = "receiver_deployment_latitude",
                        datetime = "detection_datetime", 
                        env_var = "rs_sst",
                        cache_layers = F,
                        crop_layers = TRUE,
                        fill_gaps = TRUE,
                        full_timeperiod = F,
                        folder_name = "VP2 AUG detections",
                        .parallel = F) #parallel::detectCores() - 2


#do the loop de loop
station_names <- names(list_of_dfs)

for (station in station_names) {
  # Access data frame by station name
  current_df <- get(station)
  
  result_df <- extractEnv(df = current_df,
                          X = "receiver_deployment_longitude", 
                          Y = "receiver_deployment_latitude",
                          datetime = "full_dates", 
                          env_var = "rs_sst",
                          cache_layers = F,
                          crop_layers = TRUE,
                          fill_gaps = TRUE,
                          full_timeperiod = F,
                          folder_name = "VP2 AUG detections",
                          .parallel = parallel::detectCores() - 2)
  
  # Assign the result back to the same station name or a modified one
  assign(paste0(station, "_env"), result_df)
}


# Calculate Monthly Averages -----------------------------------------
    

monthly_averages <- df %>%
  group_by(Month, Year) %>%
  summarise(
    avg_rs_sst = mean(rs_sst, na.rm = TRUE),
    avg_ucur = mean(ucur, na.rm = TRUE),
    avg_vcur = mean(vcur, na.rm = TRUE),
    avg_rs_gsla = mean(rs_gsla, na.rm = TRUE),
    avg_rs_current_bearing = mean(rs_current_bearing, na.rm = TRUE),
    avg_rs_geostrophic_velocity = mean(rs_geostrophic_velocity, na.rm = TRUE)
  )

# Anomaly Calculation Function ---------------------------------------

calculate_anomaly <- function(data, monthly_data) {
  data %>% 
    left_join(monthly_data, by = c("Month", "Year")) %>%
    mutate(
      rs_sst_anomaly = rs_sst - avg_rs_sst,
      ucur_anomaly = rs_ucur - avg_ucur,
      vcur_anomaly = rs_vcur - avg_vcur,
      rs_gsla_anomaly = rs_gsla - avg_rs_gsla,
      rs_current_bearing_anomaly = rs_current_bearing - avg_rs_current_bearing,
      rs_geostrophic_velocity_anomaly = rs_geostrophic_velocity - avg_rs_geostrophic_velocity
    ) %>%
    select(-starts_with("avg_"))
}

# Note: This function assumes that your data has columns "rs_sst", "ucur", etc. and that your `monthly_averages` has columns "avg_rs_sst", "avg_ucur", etc.

df_with_anomalies <- calculate_anomaly(df, monthly_averages)

# Deploy the function using purrr -----------------------------------------

# Assuming you have a list of dataframes named `data_list`
list_of_df_with_anomalies <- map(data_list, ~ calculate_anomaly(.x, monthly_averages))
