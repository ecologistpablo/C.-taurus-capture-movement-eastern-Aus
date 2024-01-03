#28.12.23
  #doing more detection stats
    #some summary tables

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
  loc_data <- data %>% filter(Location == location)
  males <- loc_data %>% filter(Sex == "M")
  females <- loc_data %>% filter(Sex == "F")
  
  # List of variables
  variables <- c("SST", "SST_anomaly", "cur_GSLA", "anomaly_GSLA", "cur_VCUR", "anomaly_VCUR")
  
  # Calculate stats for each variable and sex using map
  stats <- map_dfr(variables, ~ {
    male_stats <- summarize_stats(males, .x)
    female_stats <- summarize_stats(females, .x)
    
    tibble(
      Location = location,
      Sex = c("Males", "Females"),
      Variable = .x,
      Range = c(male_stats$range, female_stats$range),
      Mean_SD = c(male_stats$mean_sd, female_stats$mean_sd)
    )
  })
  
  return(stats)
}

# Apply this function for each location
locations <- unique(dat2$Location)
all_stats <- map_dfr(locations, calculate_stats, data = dat2)
all_stats

# Reshape the data for the desired output format
final_stats <- all_stats %>%
  pivot_wider(names_from = Variable, values_from = c(Range, Mean_SD)) %>%
  unite("Location_Sex", Location, Sex, sep = " ")

# View the results
print(final_stats)

#can save that to a csv now and put in results

write_csv(final_stats, file = "outputs/231228_enviro_SDs.csv")

# Male SD + Mean SST ------------------------------------------------------

male_sst_stats <- dat2 %>%
  filter(Sex == "M") %>%
  summarise(
    Mean_SST = mean(SST, na.rm = TRUE),
    SD_SST = sd(SST, na.rm = TRUE)
  )

msst <- dat2 %>% 
  filter(Sex == "M")

summary(msst$SST)

msst1 <- msst %>% 
  summarise(SD = sd(SST, na.rm = T))

msst1

# View the result
print(male_sst_stats)

