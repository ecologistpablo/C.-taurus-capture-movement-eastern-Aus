#14.11.23

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#bring and clean data environment
rm(list=ls())
setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/250212_det_enviro_complete.csv")

unique(dat$location)

#dat munging -------------------------------------------------------------------

# Remove rows where Presence is equal to 0
dat1 <- dat[dat$presence != 0, ]

dat2 <- dat1 %>% 
  filter(location %in% c("Wolf Rock", "Flat Rock", "Coffs Harbour",
                         "Hawks Nest", "Sydney"))

unique(dat2$tag_id)

# high performing tags ----------------------------------------------------

tag29124 <- dat2 %>% 
  filter(tag_id == "29124")

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
  group_by(tag_id) %>%
  summarize(
    start_date = min(date),
    end_date = max(date),
    duration = as.numeric(end_date - start_date))


#lets save that in-case we want to come back to it
write_csv(tag_durations, file = "Inputs/231228_tag_performance_results.csv")

# Calculate summary statistics
summary_stats <- tag_durations %>%
  summarize(
    mean = mean(duration),
    median = median(duration),
    SD = sd(duration),
    Min = min(duration),
    Max = max(duration),
    Q1 = quantile(duration, 0.25),
    Q3 = quantile(duration, 0.75)
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
  males <- filter(data, sex == "M")
  females <- filter(data, sex == "F")
  
  # Variables for analysis
  variables <- c("num_days", "distance")
  
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

sex_unique <- dat2 %>% 
  distinct(tag_id, .keep_all = T)
table(sex_unique$sex)

