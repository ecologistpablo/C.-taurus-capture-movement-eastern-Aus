#VTrack wrestling
  #12.06.23
    #Pablo Fuenzalida

rm(list=ls()) 

# explanation -------------------------------------------------------------

# VTrack was written to quantify movement between sites in acoustic telemetry
# It uses two detections, and determines departure and arrival sites, times etc.
# You need to format a dataframe correctly, using distinct names & data structures
# You cannot have any NAs in a few of these columns, so data prep is important
# Once you pass these data through runresidenceextraction, you can quantiy both
# residency and non-residency (movement) simultaneously
#for more info on VTrack, read: https://github.com/RossDwyer/VTrack

# libraries ---------------------------------------------------------------

setwd("~/Documents/USC/Honours/R/data")
library(tidyverse)
library(VTrack)
IMOS <- read.csv("Inputs/250302_step3.csv")

#ReadInputData -----------------------------------------------------------------------

detections_formatted_vtrack <- IMOS %>% #clean and format VTrack 
  mutate(  # Add required sensor columns
    transmitter_sensor_raw_value = 0, # required col
    transmitter_sensor_unit = 0 # required col
  ) %>%  # Extract tag ID from transmitter_id
  separate(transmitter_id, c("Frequency", "Code", "ID"), sep = "-") %>%
  transmute(  # Select and rename columns in one step
    DATETIME = as.POSIXct(detection_datetime, format = "%Y-%m-%d"),
    TRANSMITTERID = as.numeric(ID),
    SENSOR1 = as.numeric(transmitter_sensor_raw_value),
    UNITS1 = as.numeric(transmitter_sensor_unit),
    RECEIVERID = unlist(data.table::tstrsplit(receiver_name, "-", keep=2)),
    STATIONNAME = as.factor(location) # loction or station name
  ) %>%
  arrange(DATETIME) %>%  # Sort as required by VTrack
  as.data.frame() #names, order and structure needs to be exactly as above

str(detections_formatted_vtrack) # Verify structure
# head <- head(detections_formatted_vtrack)

# VTrack can't handle NAs in these rows, so remove beforehand
detections_formatted_vtrack1 <- detections_formatted_vtrack %>%
  filter(!is.na(DATETIME), !is.na(TRANSMITTERID), !is.na(STATIONNAME))

#RunResidenceExtraction --------------------------------------------------------------------

TID.Res_all <-  #to understand RunResidenceExtraction, read vignette
  VTrack::RunResidenceExtraction(sInputFile = detections_formatted_vtrack1,
                         sLocation = "STATIONNAME",
                         iResidenceThreshold = 1,
                         iTimeThreshold = 0, 
                         sDistanceMatrix = NULL,
                         iCores = parallel::detectCores() - 2) # parallel processing

# Data exploration ------------------------------------------------------------------

save(TID.Res_all, file = "Inputs/TID.Res_all_250625_receivers.RData")
load("Inputs/TID.Res_all_250623.RData")

TID.Res_all.Logs <- # Explore Residences log
TID.Res_all$residenceslog

TID.Res_all.Logs <- 
TID.Res_all$residences

TID.Res.Movements <- # Explore Non-Residences/Movements
  TID.Res_all$nonresidences

TID.Res.Movements <- TID.Res.Movements[ , -c(8, 9)] #remove unnessecary columns

colnames(TID.Res.Movements) # how's it looking

TID.Res.Movements <- TID.Res.Movements %>%
  filter(STATIONNAME1 != STATIONNAME2) #remove movements that return to the same location


# residence munging -------------------------------------------------------

TID.Res_all.Logs <- TID.Res_all.Logs[, -c(3, 6)]

TID.Res_all.Logs <- TID.Res_all.Logs %>% 
  distinct(STARTTIME, ENDTIME, TRANSMITTERID, STATIONNAME, .keep_all = TRUE)

# save --------------------------------------------------------------------

write_csv(TID.Res.Movements,file = "Inputs/250625_step4_receivers.csv")
write_csv(TID.Res_all.Logs,file = "Inputs/250626_residency.csv")
