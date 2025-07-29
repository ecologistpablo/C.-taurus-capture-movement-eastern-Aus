# VTrack RunRes





#RunResidenceExtraction --------------------------------------------------------------------

tic("RunResidenceExtraction")

TID.Res_all <- VTrack::RunResidenceExtraction(
  sInputFile = head, # your dataframe name
  sLocation = "STATIONNAME", #has to be this format / name for receivers / stations
  iResidenceThreshold = 1, # how many detections = residence ? 
  iTimeThreshold = 60 * 60 * 24 * 15, # 15 d threshold on days
  sDistanceMatrix = NULL, #dist. threshold ? 
  iCores = 5) # parallel processing

toc() 

myoutput <- RunResidenceExtraction(head, 
                                   sLocation = "STATIONNAME", 
                                   iResidenceThreshold = 1, 
                                   iTimeThreshold = 60*60*24*2, 
                                   sDistanceMatrix = NULL, 
                                   n_workers = 4)
toc() 


# Data exploration ------------------------------------------------------------------

save(TID.Res_all, file = "Inputs/TID.Res_all_250709_receivers.RData")
#load("Inputs/TID.Res_all_250623.RData")

#TID.Res_all.Logs <- TID.Res_all$residenceslog  # Explore Residences log
TID.Res_all.Logs <- TID.Res_all$residences

TID.Res.Movements <- TID.Res_all$nonresidences # Explore Non-Residences/Movements
TID.Res.Movements <- TID.Res.Movements[ , -c(8, 9)] #remove unnessecary columns

TID.Res.Movements <- TID.Res.Movements %>%
  filter(STATIONNAME1 != STATIONNAME2) #remove movements that return to the same location

# residence munging -------------------------------------------------------

TID.Res_all.Logs <- TID.Res_all.Logs[, -c(3, 6)]

TID.Res_all.Logs <- TID.Res_all.Logs %>% 
  distinct(STARTTIME, ENDTIME, TRANSMITTERID, STATIONNAME, .keep_all = TRUE)

# save --------------------------------------------------------------------

write_csv(TID.Res.Movements,file = "Inputs/250709_step4.csv")
write_csv(TID.Res_all.Logs,file = "Inputs/250709_residency.csv")
