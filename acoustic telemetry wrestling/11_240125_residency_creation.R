#24.01.24
# creating residence data

rm(list=ls()) 
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("~/University/2023/Honours/R/data")
res_dat <- read_csv("Inputs/240124_residency.csv") #residence events
IMOS <- read_csv("Inputs/240114_step3.csv") #after receiver renaming but before VTrack



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

write_csv(res_dat, file = "Inputs/240125_res_dat.csv")
