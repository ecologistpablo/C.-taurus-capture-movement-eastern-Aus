#14.11.23

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#bring and clean data environment
rm(list=ls())
setwd("~/University/2023/Honours/R/data")
#dat <- read_csv("Inputs/230726_step8.csv")
dat <- read_csv("Inputs/230912a_complete_det_enviro.csv")

unique(dat$Location)

#dat munging -------------------------------------------------------------------

# Remove rows where Presence is equal to 0
dat1 <- dat[dat$Presence != 0, ]

dat2 <- dat1 %>% 
  filter(Location %in% c("Wolf Rock", "Moreton Island", "Flat Rock", "Coffs Harbour",
                         "Hawks Nest", "Sydney", "Jervis Bay"))

unique(dat2$Tag_ID)

# high performing tags ----------------------------------------------------

tag29124 <- dat2 %>% 
  filter(Tag_ID == "29124")

summary(tag29124)

tag$Sex

Males <- dat2 %>% 
  filter(Sex == "M")

Females <- dat2 %>% 
  filter(Sex == "F")

table(Males$Tag_ID)

tag29145 <- Males %>% 
  filter(Tag_ID == "29145")



# tag performance ---------------------------------------------------------

# Calculate the duration for each tag
tag_durations <- dat2 %>%
  group_by(Tag_ID) %>%
  summarize(
    Start_Date = min(detection_datetime),
    End_Date = max(detection_datetime),
    Duration = as.numeric(End_Date - Start_Date)
  )

#lets save that in-case we want to come back to it
write_csv(tag_durations, file = "Inputs/231228_tag_performance_results.csv")

# Calculate summary statistics
summary_stats <- tag_durations %>%
  summarize(
    Mean = mean(Duration),
    Median = median(Duration),
    SD = sd(Duration),
    Min = min(Duration),
    Max = max(Duration),
    Q1 = quantile(Duration, 0.25),
    Q3 = quantile(Duration, 0.75)
  )

# Print summary statistics
print(summary_stats)

-3496/ 365


# duration of movements ---------------------------------------------------

daysatliberty <- dat2 %>%
  summarize(Mean = mean(Num_days, na.rm = TRUE), 
    SD = sd(Num_days, na.rm = TRUE))
    
table(daysatliberty)    


# distance means ----------------------------------------------------------

distancestats <- dat2 %>% 
  summarise(Mean = mean(distance, na.rm = T),
            SD = sd(distance, na.rm = T))

table(distancestats)
summary(dat2$distance)


# summaries ---------------------------------------------------------------

summary(tag29145)
summary(dat2)
summary(Females$SST)
summary(Males$SST)
summary(Females$Location)
summary(Males$Location)
summary(Males$distance)
summary(Females$distance)
summary(Males$Num_days)
summary(Females$Num_days)
table(Males$Departure_location)
table(Females$Departure_location)
summary(Males$cur_VCUR)
summary(Females$cur_VCUR)
summary(Males$anomaly_VCUR)
summary(Females$anomaly_VCUR)


# purrr duration of tags + distance stats ---------------------------------

# Function to calculate summary statistics with rounding
calculate_stats <- function(data, variable) {
  data %>%
    summarise(
      Min = round(min(!!sym(variable), na.rm = TRUE), 2),
      Max = round(max(!!sym(variable), na.rm = TRUE), 2),
      Mean = round(mean(!!sym(variable), na.rm = TRUE), 2),
      SD = round(sd(!!sym(variable), na.rm = TRUE), 2)
    ) %>%
    mutate(Mean_SD = paste(Mean, "±", SD),
           Range = paste(Min, "to", Max)) %>%
    dplyr::select(Mean_SD, Range) %>%
    unlist()
}


# Analysis split by sex
analysis_by_sex <- function(data) {
  # Split the data by sex
  males <- filter(data, Sex == "M")
  females <- filter(data, Sex == "F")
  
  # Variables for analysis
  variables <- c("Num_days", "distance")
  
  # Calculate stats for each variable and sex
  stats <- map_dfr(variables, ~ {
    tibble(
      Variable = .x,
      Male = list(calculate_stats(males, .x)),
      Female = list(calculate_stats(females, .x))
    )
  }, .id = "Metric")
  
  return(stats)
}

# Apply the function to the dataset
tag_movement_stats <- analysis_by_sex(dat2)

# View the results
print(tag_movement_stats)


write_csv(tag_movement_stats, "Outputs/231229_distance_duration_sex.csv")



