#VTrack wrestling
  #12.06.23
    #Pablo Fuenzalida

# explanation -------------------------------------------------------------

# VTrack was written to quantify movement between sites in acoustic telemetry
# It uses two detections, and determines departure and arrival sites, times etc.
# You need to format a dataframe correctly, using distinct names & data structures
# Once you pass these data through runresidenceextraction, you can quantify both:
# residency and non-residency (movement) simultaneously
# It's incredible, thankto R. Dwyer!
#for more info on VTrack, read: https://github.com/RossDwyer/VTrack

# libraries ---------------------------------------------------------------

rm(list=ls()) 
setwd("~/Documents/USC/Honours/R/data")
pacman::p_load("tidyverse", "VTrack", "lubridate", 'tictoc')
dat <- read.csv("Inputs/250724_step3.csv")

#ReadInputData -----------------------------------------------------------------------

dat1 <- dat %>% #clean and format VTrack 
  mutate(transmitter_sensor_raw_value = 0, # required col
    transmitter_sensor_unit = 0) %>% # Extract tag ID from transmitter_id
    transmute(DATETIME = lubridate::with_tz(lubridate::ymd_hms(datetime, tz = "UTC"), tzone = "Etc/GMT-10"),
    TRANSMITTERID = as.factor(tag_id),
    SENSOR1 = as.numeric(transmitter_sensor_raw_value),
    UNITS1 = as.numeric(transmitter_sensor_unit),
    RECEIVERID = unlist(data.table::tstrsplit(receiver_name, "-", keep=2)),
    STATIONNAME = as.factor(location)) %>% # location or station name
  dplyr::select(DATETIME,TRANSMITTERID,SENSOR1,UNITS1,RECEIVERID,STATIONNAME) %>% 
  data.frame()

str(dat1) # Verify structure

dat1 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  arrange(desc(na_count)) # a few rows have NAs


# VTrack can't handle NAs in these rows, so remove beforehand
dat2 <- dat1 %>%
    filter(if_all(everything(), ~ !is.na(.)))
anyNA(dat2)

head <- dat2[1:1000000,]

write_csv(dat2, "Inputs/250724_step4.csv")



