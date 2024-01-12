#VTrack wrestling
  #12.06.23
    #Pablo Fuenzalida

#for info on how we utilised VTrack, read: https://github.com/RossDwyer/VTrack

rm(list=ls()) 

# libraries ---------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

setwd("~/University/2023/Honours/R/data") 
IMOS <- read_csv("Inputs/231020_step2.csv")

#ReadInputData -----------------------------------------------------------------------

#VTrack only works with specific column names and structures
#we need to bend our data into this so VTrack likes it
IMOS <- IMOS %>%
  mutate(transmitter_sensor_raw_value = 0,
         transmitter_sensor_unit = 0)

#05.03.23 RDwyer code for ReadInputData (old function) to read in new IMOS ATF data
detections_formatted <- 
  IMOS %>%
  dplyr::select(c(detection_datetime, transmitter_id, transmitter_sensor_raw_value, transmitter_sensor_unit, #select what to pull out of the dataframe
                  receiver_name, Location, receiver_deployment_longitude, receiver_deployment_latitude))  %>% #and again
  rename(Date.Time = detection_datetime, #rename them so VTrack knows what variables are
         Transmitter.ID = transmitter_id, 
         Sensor.1 = transmitter_sensor_raw_value,
         Units.1 = transmitter_sensor_unit, 
         Station.Name = Location, #we want a location-based analysis which is a little more coarse then looking at each individual hydrophone or receiver
         Receiver.Name = receiver_name,
         Station.Longitude = receiver_deployment_longitude, 
         Station.Latitude = receiver_deployment_latitude)  %>%
  separate(Transmitter.ID, #Just want the tag code from transmitter_ID
           c("Frequency", "Code", "ID"), sep = "-") %>% # Extract just the tag code
  mutate(Code.Space = paste0(Frequency,"-",Code)) %>% #mutate it
  dplyr::select(c(Date.Time,Code.Space,ID,Sensor.1,Units.1,Receiver.Name, Station.Name,
                  Station.Longitude, Station.Latitude))

#Get dataframe in correct format for VTrack
detections_formatted_vtrack <- detections_formatted %>%
  mutate(DATETIME = Date.Time,
         Code.Space = as.factor(Code.Space),
         TRANSMITTERID = as.character(ID),
         SENSOR1 = as.numeric(Sensor.1),
         UNITS1 = as.character(Units.1),
         RECEIVERID = unlist(tstrsplit(Receiver.Name,"-",keep=2)),
         STATIONNAME = as.character(Station.Name)) %>% 
  dplyr::select(DATETIME,TRANSMITTERID,SENSOR1,UNITS1,RECEIVERID,STATIONNAME) %>% 
  arrange(TRANSMITTERID,DATETIME) %>% data.frame()

head(detections_formatted_vtrack)


#RunResidenceExtraction --------------------------------------------------------------------

TID.Res_all <-  #to understand RunResidenceExtraction, read vignette
  RunResidenceExtraction(sInputFile = detections_formatted_vtrack,
                         sLocation = "STATIONNAME",
                         iResidenceThreshold = 1,
                         iTimeThreshold = 0, 
                         sDistanceMatrix = NULL,
                         iCores = parallel::detectCores() - 2) #parallel::detectCores() - 2

# Data exploration ------------------------------------------------------------------

save(TID.Res_all, file = "Inputs/TID.Res_all_231020.RData")
#load("TID.Res_all_230805.RData")

#Explore Residences log
TID.Res_all.Logs <-
 TID.Res_all$residenceslog

#TID.Res_all.Logs <- 
#  TID.Res_all$residences

# Explore Non-Residences/Movements
TID.Res.Movements <- 
  TID.Res_all$nonresidences

#remove unnessecary columns
TID.Res.Movements <- TID.Res.Movements[ , -9]
TID.Res.Movements <- TID.Res.Movements[ , -8]

head(TID.Res.Movements)

TID.Res.Movements <- TID.Res.Movements %>%
  filter(STATIONNAME1 != STATIONNAME2) #remove movements that return to the same location
#559, 543 movements, 641 (still 641 :o)


# save --------------------------------------------------------------------

write_csv(TID.Res.Movements,file = "Inputs/230906_step3.csv")

