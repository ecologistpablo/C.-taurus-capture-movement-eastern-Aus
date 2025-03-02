#VTrack wrestling
  #12.06.23
    #Pablo Fuenzalida

#for info on how we utilised VTrack, read: https://github.com/RossDwyer/VTrack

rm(list=ls()) 

# libraries ---------------------------------------------------------------

setwd("~/Documents/USC/Honours/R/data")
library(tidyverse)
library(VTrack)
IMOS <- read_csv("Inputs/241116_step2.csv")

#ReadInputData -----------------------------------------------------------------------

# VTrack only works with specific column names and structures
# we need to bend our data into this so VTrack likes it
# 05.03.23 Ross Dwyer code for ReadInputData (old function) to read in new IMOS ATF data

# Clean and format data for VTrack in one pipe
detections_formatted_vtrack <- IMOS %>%
  mutate(  # Add required sensor columns
    transmitter_sensor_raw_value = 0,
    transmitter_sensor_unit = 0
  ) %>%  # Extract tag ID from transmitter_id
  separate(transmitter_id, c("Frequency", "Code", "ID"), sep = "-") %>%
  transmute(  # Select and rename columns in one step
    DATETIME = as.POSIXct(detection_datetime),
    TRANSMITTERID = as.numeric(ID),
    SENSOR1 = as.numeric(transmitter_sensor_raw_value),
    UNITS1 = as.numeric(transmitter_sensor_unit),
    RECEIVERID = unlist(data.table::tstrsplit(receiver_name, "-", keep=2)),
    STATIONNAME = as.factor(station_name)
  ) %>%
  arrange(DATETIME) %>%  # Sort as required by VTrack
  as.data.frame()

# Verify structure
str(detections_formatted_vtrack)

head <- head(detections_formatted_vtrack)

#RunResidenceExtraction --------------------------------------------------------------------

TID.Res_all <-  #to understand RunResidenceExtraction, read vignette
  VTrack::RunResidenceExtraction(sInputFile = detections_formatted_vtrack,
                         sLocation = "STATIONNAME",
                         iResidenceThreshold = 1,
                         iTimeThreshold = 0, 
                         sDistanceMatrix = NULL,
                         iCores = parallel::detectCores() - 2) 

# Data exploration ------------------------------------------------------------------

save(TID.Res_all, file = "Inputs/TID.Res_all_250301.RData")
#load("Inputs/TID.Res_all_250211.RData")

# Explore Residences log
TID.Res_all.Logs <-
TID.Res_all$residenceslog

TID.Res_all.Logs <- 
TID.Res_all$residences

# Explore Non-Residences/Movements
TID.Res.Movements <- 
  TID.Res_all$nonresidences

TID.Res.Movements <- TID.Res.Movements[ , -c(8, 9)] #remove unnessecary columns

colnames(TID.Res.Movements)

TID.Res.Movements <- TID.Res.Movements %>%
  filter(STATIONNAME1 != STATIONNAME2) #remove movements that return to the same location


# residence munging -------------------------------------------------------

TID.Res_all.Logs <- TID.Res_all.Logs[, -c(3, 6)]

TID.Res_all.Logs <- TID.Res_all.Logs %>% 
  distinct(STARTTIME, ENDTIME, TRANSMITTERID, STATIONNAME, .keep_all = TRUE)

# save --------------------------------------------------------------------

write_csv(TID.Res.Movements,file = "Inputs/250301_step4.csv")
write_csv(TID.Res_all.Logs,file = "Inputs/250301_residency.csv")
