#28.12.23
  #doing more detection stats
    #some summary tables

library(tidyverse)
#bring and clean data environment
rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250212_det_enviro_complete.csv")

# cleaning  ---------------------------------------------------------------

dat1 <- dat[dat$presence != 0, ]

dat2 <- dat1 %>% 
  filter(location %in% c("Wolf Rock", "Flat Rock", "Coffs Harbour",
                         "Hawks Nest", "Sydney"))

# stats -------------------------------------------------------------------

# Function to calculate stats for a given variable and sex
summarize_stats <- function(data, variable) {
  stats <- data %>%
    summarize(
      Min = min(!!sym(variable), na.rm = TRUE),
      Max = max(!!sym(variable), na.rm = TRUE),
      Mean = mean(!!sym(variable), na.rm = TRUE),
      SD = sd(!!sym(variable), na.rm = TRUE)
    )
  
  # Format to two decimal places
  stats <- round(stats, 2)
  
  # Combine Min and Max for range, Mean and SD for mean ± SD
  range <- paste(stats$Min, "to", stats$Max)
  mean_sd <- paste(stats$Mean, "±", stats$SD)
  
  return(list(range = range, mean_sd = mean_sd))
}

# Function to calculate and organize stats for a given location
calculate_stats <- function(location, data) {
  loc_data <- data %>% filter(location == location)
  males <- loc_data %>% filter(sex == "M")
  females <- loc_data %>% filter(sex == "F")
  
  # List of variables
  variables <- c("sst", "sst_anomaly", "cur_GSLA", "anomaly_GSLA", "cur_VCUR", "anomaly_VCUR")
  
  # Calculate stats for each variable and sex using map
  stats <- map_dfr(variables, ~ {
    male_stats <- summarize_stats(males, .x)
    female_stats <- summarize_stats(females, .x)
    
    tibble(
      Location = location,
      sex = c("Males", "Females"),
      Variable = .x,
      Range = c(male_stats$range, female_stats$range),
      Mean_SD = c(male_stats$mean_sd, female_stats$mean_sd)
    )
  })
  
  return(stats)
}

# Apply this function for each location
locations <- unique(dat2$location)
all_stats <- map_dfr(locations, calculate_stats, data = dat2)
all_stats

# Reshape the data for the desired output format
final_stats <- all_stats %>%
  pivot_wider(names_from = Variable, values_from = c(Range, Mean_SD)) %>%
  unite("Location_Sex", Location, sex, sep = " ")

# View the results
print(final_stats)

#can save that to a csv now and put in results

write_csv(final_stats, file = "outputs/250226_enviro_SDs.csv")

# Male SD + Mean SST ------------------------------------------------------

dat2 %>%
  filter(sex == "M") %>%
  summarise(
    Mean_SST = mean(sst, na.rm = TRUE),
    SD_SST = sd(sst, na.rm = TRUE))

dat2 %>%
  filter(sex == "F") %>%
  summarise(
    Mean_SST = mean(sst, na.rm = TRUE),
    SD_SST = sd(sst, na.rm = TRUE))

msst <- dat2 %>% 
  filter(sex == "M")

summary(msst$sst)
summary(msst$sst_anomaly)
summary(msst$anomaly_GSLA)

fsst <- dat2 %>% 
  filter(sex == "F")
summary(fsst$sst)
summary(fsst$sst_anomaly)
summary(fsst$anomaly_GSLA)

msst1 <- msst %>% 
  summarise(SD = sd(SST, na.rm = T))

msst1

# View the result
print(male_sst_stats)


# temporal range of each site ---------------------------------------------

# getting all variables at once is fucking too hard 
# once step at a time

# Function to get the range of dates
get_date_range <- function(df) {
  range <- range(df$detection_datetime, na.rm = TRUE)
  return(paste(range[1], "to", range[2]))
}

# Calculate the range of detection_datetime for each Sex and Location
date_ranges <- dat2 %>%
  group_by(Location, Sex) %>%
  summarise(Date_Range = get_date_range(cur_data()), .groups = 'drop') %>%
  pivot_wider(names_from = Location, values_from = Date_Range) #warning

# WARNING : females is before males on this, 
# you have to manually paste these in the correct place in excel

# Print the result
print(date_ranges)

write_csv(date_ranges, file = "outputs/240108_date_ranges.csv")


# Number of movements -----------------------------------------------------


movement_counts <- dat2 %>%
  group_by(Location, Sex, movement) %>%
  summarise(Count = n(), .groups = 'drop')
movement_counts
# copy and paste into excel, and transpose


# months ------------------------------------------------------------------

# Get all unique months for both sexes at all locations
unique_months <- dat2 %>%
  dplyr::select(Location, Sex, month) %>%
  distinct() %>%
  arrange(Location, Sex, month)

# If you want to see them in a more structured way, like a list by Location and Sex, you can do:
month_list <- unique_months %>%
  group_by(Location, Sex) %>%
  summarise(Months = toString(sort(unique(month))), .groups = 'drop')
month_list
# copy and paste into excel, and transpose


# num of unique tags ------------------------------------------------------

unique_tag_counts <- dat2 %>%
  group_by(Location, Sex) %>%
  summarise(Tag_count = n_distinct(Tag_ID), .groups = 'drop')

unique_tag_counts
# copy and paste into excel, and transpose


# distance metrics --------------------------------------------------------

dist_stats <- dat2 %>%
  group_by(Location, Sex) %>%
  summarise(
    Min_Distance = round(min(distance, na.rm = TRUE), 0),
    Max_Distance = round(max(distance, na.rm = TRUE), 0),
    Mean_Distance = round(mean(distance, na.rm = TRUE), 0),
    SD_Distance = round(sd(distance, na.rm = TRUE), 0),
    .groups = 'drop'
  ) %>%
  mutate(
    Range_Distance = paste(Min_Distance, "to", Max_Distance),
    mean_SD = paste(Mean_Distance, "±", SD_Distance)
  ) %>%
  dplyr::select(Location, Sex, Range_Distance, mean_SD)

dist_stats
# copy and paste into excel, and transpose


# days at liberty ---------------------------------------------------------

Duration_stats <- dat2 %>%
  group_by(Location, Sex) %>%
  summarise(
    Min_duration = round(min(Num_days, na.rm = TRUE), 0),
    Max_duration = round(max(Num_days, na.rm = TRUE), 0),
    Mean_duration = round(mean(Num_days, na.rm = TRUE), 0),
    SD_duration = round(sd(Num_days, na.rm = TRUE), 0),
    .groups = 'drop'
  ) %>%
  mutate(
    Duration_range = paste(Min_duration, "to", Max_duration),
    mean_SD = paste(Mean_duration, "±", SD_duration)
  ) %>%
  dplyr::select(Location, Sex, Duration_range, mean_SD)

Duration_stats
# copy and paste into excel, and transpose

