#24.01.24
# creating residence data

rm(list=ls()) 
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("~/University/2023/Honours/R/data")
res_dat <- read_csv("Inputs/240124_residency.csv") #residence events

# dplyr wrestling ---------------------------------------------------------

res_dat <- res_dat %>% 
  rename( #rename rows
    date = DATETIME,
    Tag_ID = TRANSMITTERID,
    Location = STATIONNAME) %>% 
  dplyr::select(-RESIDENCEEVENT)

str(res_dat)

res_dat <- res_dat %>%
  mutate(Month = format(date, "%Y-%m")) %>%  # Create Year-Month column from date
  group_by(Tag_ID, Location, date) %>%  # Group data by Tag ID, Location, and Year-Month
  summarise(
    monthly_res = n_distinct(date)) %>%  # Get the first date in each group
  ungroup()  # Remove grouping

head(res_dat)


# connect to sex ----------------------------------------------------------

IMOS <- read_csv("Inputs/240114_step3.csv") #after receiver renaming but before VTrack


# our beautiful function that connects the two
combine <- function(IMOS, res_dat) {
  
  # Ensure IMOS data has the necessary columns
  if(!("detection_datetime" %in% names(IMOS)) || 
     !("tag_id" %in% names(IMOS)) || 
     !("Location" %in% names(IMOS)) || 
     !("animal_sex" %in% names(IMOS))) {
    stop("IMOS data does not have the required columns.")
  }
  
  # Ensure res_dat data has the necessary columns
  if(!("Tag_ID" %in% names(res_dat)) || 
     !("Location" %in% names(res_dat)) || 
     !("Month" %in% names(res_dat))) {
    stop("res_dat does not have the required columns.")
  }
  
  # Prepare IMOS data
  IMOS <- IMOS %>%
    mutate(YearMonth = format(as.Date(detection_datetime), "%Y-%m")) %>%
    group_by(Tag_ID, Location, YearMonth) %>%
    summarise(animal_sex = first(animal_sex)) %>%
    ungroup()
  
  # Prepare res_dat data
  res_dat <- res_dat %>%
    mutate(YearMonth = format(Month, "%Y-%m"))
  
  # Perform the join
  res_dat <- res_dat %>%
    left_join(IMOS, by = c("Tag_ID", "Location", "YearMonth")) %>%
    select(-YearMonth) # Remove the YearMonth column after join
  
  return(res_dat)
}

combo_dat <- combine(IMOS, res_dat)
