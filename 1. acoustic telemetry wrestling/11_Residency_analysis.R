#24.01.24
# creating residence data

rm(list=ls()) 
source("/Users/owuss/Documents/USC/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
res_dat <- read_csv("Inputs/241122_residency.csv") #residence events
IMOS <- read_csv("Inputs/241116_step3.csv") #after receiver renaming but before VTrack
# residency does not include any pseudo absenses


# preamble ----------------------------------------------------------------

# pretty late into my honours year, we realised I should be adding in residency into my analysis too
# VTrack calculates this in script 4, so it's pretty easy to add
# we will focus our analysis on residency of locations that hold detections in the same location
# non-residency will indicate residency is further away and individuals will travel through these areas
# you can copy this for residency munging though but it's pretty specific towards my methods

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
  distinct(Tag_ID, Location, date)
head(res_dat)


# connect to sex ----------------------------------------------------------

# Ensure Tag_ID is the same format in both data frames
IMOS$Tag_ID <- as.character(IMOS$Tag_ID)

# Add the animal_sex column to res_dat, initially filled with NA
res_dat$animal_sex <- NA

# Match and fill in animal_sex
matched_indices <- match(res_dat$Tag_ID, IMOS$Tag_ID)
res_dat$animal_sex <- IMOS$animal_sex[matched_indices]

str(res_dat)

write_csv(res_dat, file = "Inputs/241122_res_dat.csv")
