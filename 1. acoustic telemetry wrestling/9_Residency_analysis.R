#24.01.24
# creating residence data

rm(list=ls()) 
source("/Users/owuss/Documents/USC/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
res_dat <- read_csv("Inputs/250211_residency.csv") #residence events
IMOS <- read_csv("Inputs/250211_step3.csv") #after receiver renaming but before VTrack
# residency does not include any pseudo absenses


# preamble ----------------------------------------------------------------

# VTrack calculates this in script 4, so it's pretty easy to add
# we will focus our analysis on residency of locations that hold detections in the same location
# non-residency will indicate residency is further away and individuals will travel through these areas
# you can copy this for residency munging though but it's pretty specific towards my methods

# dplyr wrestling ---------------------------------------------------------

res_dat1 <- res_dat %>% 
  rename( #rename rows
    date = STARTTIME,
    tag_id = TRANSMITTERID,
    location = STATIONNAME) %>% 
  dplyr::select(-ENDREASON, -NUMRECS, -ENDTIME)
#each date is a single day of residency
str(res_dat1)

res_dat1$date <- as.Date(res_dat1$date)
str(res_dat1)

res_dat2 <- res_dat1 %>%
  mutate(YM = format(date, "%Y-%m"),
         month = month(date),
         tag_id = as.character(tag_id)) %>% 
  distinct(tag_id, location, date, .keep_all = T)
head(res_dat2)

res_dat3 <- res_dat2

# connect to sex ----------------------------------------------------------

# Add the animal_sex column to res_dat, initially filled with NA
res_dat3$sex <- NA
# Match and fill in animal_sex
matched_indices <- match(res_dat3$tag_id, IMOS$Tag_ID)
res_dat3$sex <- IMOS$animal_sex[matched_indices]

anyNA(res_dat3$sex) #should be false


write_csv(res_dat3, file = "Inputs/250211_res_dat.csv")
