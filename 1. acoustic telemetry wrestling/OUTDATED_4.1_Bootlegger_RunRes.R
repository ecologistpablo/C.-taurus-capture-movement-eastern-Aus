# Helper: Extract unique values (same as VTrack)
ExtractUniqueValues <- function(sInputFile, iFieldToExtract) {
  ivals <- unique(sInputFile[, iFieldToExtract])
  ivals[order(ivals)]
}

# Helper: ExtractData (Same as VTrack)
ExtractData <- function(sInputFile, sQueryTransmitterList = NULL) {
  fEvaluateQuery <- sInputFile
  if (!is.null(sQueryTransmitterList))
    fEvaluateQuery <- fEvaluateQuery[fEvaluateQuery[,2] %in% sQueryTransmitterList,]
  return(fEvaluateQuery)
}

# Helper: ReturnVR2Distance (same as VTrack)
ReturnVR2Distance <- function(NonResidenceFile,sDistanceMatrix)
  {
    iReceiverIndex1 <- 0
    iReceiverIndex2 <- 0
    VR2Distance <- 0
    
    for(j in 1:dim(NonResidenceFile)[1])
    {
      for (i in 1:dim(sDistanceMatrix)[1])
      {    
        if (as.character(sDistanceMatrix$DM[i]) == as.character(NonResidenceFile$RECEIVERID1[j]))
          iReceiverIndex1 <- i
        if (as.character(sDistanceMatrix$DM[i]) == as.character(NonResidenceFile$RECEIVERID2[j]))
          iReceiverIndex2 <- i
      }
      VR2Distance[j] <- sDistanceMatrix[iReceiverIndex1,(iReceiverIndex2)+1]
    }
    return(VR2Distance)
  }

# the big function --------------------------------------------------------

RunResidenceExtraction <- function(
    sInputFile, sLocation, iResidenceThreshold, iTimeThreshold,
    sDistanceMatrix = NULL, n_workers = 2
) {
  if (n_workers == "auto") {
    n_workers <- parallelly::availableCores(omit = 1)
  }
  
  i <- NULL
  cl <- parallel::makeCluster(n_workers)
  doParallel::registerDoParallel(cl)
  on.exit({
    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)
  }, add = TRUE)
  
  # Sort and deduplicate
  sInputFile <- unique(sInputFile[order(as.character(sInputFile$DATETIME)), ])
  TransmitterNames <- ExtractUniqueValues(sInputFile, 2)
  iTransmitterCount <- length(TransmitterNames)
  if (sLocation == "STATIONNAME") iLocationCol <- 6
  if (sLocation == "RECEIVERID") iLocationCol <- 5
  
  NonResidenceExtractId <- function(sResidenceEventFile, sDistanceMatrix = NULL) {
    if (length(unique(sResidenceEventFile$TRANSMITTERID)) > 1) 
      stop("length(TRANSMITTERID) > 1. Unable to extract non-residences.")
    if (length(unique(sResidenceEventFile[, 5])) < 2) {
      newnonresidencetable <- data.frame(
        STARTTIME = sResidenceEventFile[1, 1], ENDTIME = sResidenceEventFile[1, 2], 
        NONRESIDENCEEVENT = 0, TRANSMITTERID = "", RECEIVERID1 = "", RECEIVERID2 = "",
        DURATION = 0, DISTANCE = 0, ROM = 0
      )[NULL, ]
    } else {
      RECEIVERID1 <- sResidenceEventFile[c(1:(nrow(sResidenceEventFile) - 1)), 5]
      RECEIVERID2 <- sResidenceEventFile[c(2:nrow(sResidenceEventFile)), 5]
      STARTTIME <- sResidenceEventFile[c(1:(nrow(sResidenceEventFile) - 1)), 2]
      ENDTIME <- sResidenceEventFile[c(2:nrow(sResidenceEventFile)), 1]
      TRANSMITTERID <- sResidenceEventFile[c(2:nrow(sResidenceEventFile)), 4]
      DURATION <- as.numeric(difftime(as.POSIXct(ENDTIME), as.POSIXct(STARTTIME), units = "secs"))
      NONRESIDENCEEVENT <- 1
      nonresidencetable <- na.omit(data.frame(
        STARTTIME, ENDTIME, NONRESIDENCEEVENT, TRANSMITTERID, RECEIVERID1, RECEIVERID2, DURATION
      ))
      if (!is.null(sDistanceMatrix)) {
        DISTANCE <- ReturnVR2Distance(nonresidencetable, sDistanceMatrix)
        ROM <- DISTANCE/nonresidencetable$DURATION
      } else {
        DISTANCE <- 0
        ROM <- 0
      }
      newnonresidencetable <- data.frame(nonresidencetable, DISTANCE, ROM)
    }
    if (nrow(newnonresidencetable) >= 1) 
      newnonresidencetable$NONRESIDENCEEVENT <- seq_len(nrow(newnonresidencetable))
    return(newnonresidencetable)
  }
  
  ResidenceExtractId <- function(sTransmitterId) {
    cat("DATETIME class in worker:", class(sInputFile$DATETIME), "\n")
    cat("Example values:\n")
    print(head(sInputFile$DATETIME, 5))
    iCount <- 1
    iResidenceEvents <- 0
    ilogevent <- 1
    event <- data.frame(
      STARTTIME = sInputFile[1, 1], ENDTIME = sInputFile[1, 1], 
      RESIDENCEEVENT = 0, TRANSMITTERID = TransmitterNames[1], 
      RECEIVERID = as.character(sInputFile[1, iLocationCol]), 
      DURATION = 0, ENDREASON = NA, NUMRECS = 0, stringsAsFactors = FALSE
    )[NULL, ]
    logtable <- data.frame(
      DATETIME = sInputFile[1, 1], RESIDENCEEVENT = 0, 
      RECORD = 0, TRANSMITTERID = TransmitterNames[1], 
      RECEIVERID = as.character(sInputFile[1, iLocationCol]), 
      ELAPSED = 0, stringsAsFactors = FALSE
    )[NULL, ]
    infile <- ExtractData(sInputFile, sQueryTransmitterList = TransmitterNames[sTransmitterId])
    fResidenceEventStarted <- FALSE
    sEndReason <- ""
    iRecordsProcessed <- 0
    iNumberOfRecords <- 0
    sRECEIVERID <- ""
    sPreviousReceiver <- ""
    sSTARTTIME <- ""
    sPreviousTime <- ""
    while (iCount <= nrow(infile)) {
      sDATETIME <- infile[iCount, 1]
      sRECEIVERID <- as.character(infile[iCount, iLocationCol])
      iRecordsProcessed <- iRecordsProcessed + 1
      if (fResidenceEventStarted) {
        iNumberOfRecords <- iNumberOfRecords + 1
        iElapsedTime <- as.numeric(difftime(as.POSIXct(as.character(sDATETIME)), as.POSIXct(as.character(sPreviousTime)), units = "secs"))
        logtable[ilogevent, ] <- data.frame(
          DATETIME = as.character(sDATETIME), 
          RESIDENCEEVENT = iResidenceEvents, RECORD = iNumberOfRecords, 
          TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
          RECEIVERID = as.character(sRECEIVERID), ELAPSED = iElapsedTime, 
          stringsAsFactors = FALSE
        )
        ilogevent <- ilogevent + 1
        if (iElapsedTime > iTimeThreshold) {
          fResidenceEventStarted <- FALSE
          sEndReason <- "timeout"
        }
        if (iElapsedTime <= iTimeThreshold & sPreviousReceiver != sRECEIVERID) {
          fResidenceEventStarted <- FALSE
          sEndReason <- "receiver"
        }
        if (iCount == nrow(infile)) {
          fResidenceEventStarted <- FALSE
          sEndReason <- "signal lost"
        }
        if (!fResidenceEventStarted) {
          if (iNumberOfRecords >= iResidenceThreshold) {
            iElapsedTime <- as.numeric(difftime(as.POSIXct(as.character(sPreviousTime)), as.POSIXct(as.character(sSTARTTIME)), units = "secs"))
            event[iResidenceEvents, ] <- data.frame(
              STARTTIME = as.character(sSTARTTIME), 
              ENDTIME = as.character(sPreviousTime), 
              RESIDENCEEVENT = iResidenceEvents, 
              TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
              RECEIVERID = as.character(sPreviousReceiver), 
              DURATION = iElapsedTime, ENDREASON = sEndReason, 
              NUMRECS = iNumberOfRecords, stringsAsFactors = FALSE
            )
          }
        }
      } else {
        if (iRecordsProcessed > 1) {
          iElapsedTime <- as.numeric(difftime(as.POSIXct(as.character(sDATETIME)), as.POSIXct(as.character(sPreviousTime)), units = "secs"))
          if (iElapsedTime < iTimeThreshold) {
            iResidenceEvents <- iResidenceEvents + 1
            iNumberOfRecords <- 1
            sSTARTTIME <- sPreviousTime
            sEndReason <- ""
            logtable[ilogevent, ] <- data.frame(
              DATETIME = sPreviousTime, 
              RESIDENCEEVENT = iResidenceEvents, RECORD = 0, 
              TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
              RECEIVERID = as.character(sPreviousReceiver), 
              ELAPSED = 0, stringsAsFactors = FALSE
            )
            logtable[ilogevent + 1, ] <- data.frame(
              DATETIME = sDATETIME, 
              RESIDENCEEVENT = iResidenceEvents, RECORD = iNumberOfRecords, 
              TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
              RECEIVERID = as.character(sRECEIVERID), 
              ELAPSED = iElapsedTime, stringsAsFactors = FALSE
            )
            ilogevent <- ilogevent + 2
            if (as.character(sRECEIVERID) == as.character(sPreviousReceiver)) {
              fResidenceEventStarted <- TRUE
            } else {
              fResidenceEventStarted <- FALSE
            }
          }
        }
      }
      sPreviousTime <- sDATETIME
      sPreviousReceiver <- as.character(sRECEIVERID)
      iCount <- iCount + 1
    }
    ilist <- list(
      na.omit(logtable),
      na.omit(event),
      NonResidenceExtractId(na.omit(event), sDistanceMatrix)
    )
    return(ilist)
  }
  
  results <- foreach::foreach(
    i = 1:iTransmitterCount, 
    .packages = character(), 
    .export = c("ExtractData", "ExtractUniqueValues", "ReturnVR2Distance")
  ) %dopar% ResidenceExtractId(i)
  
  ilist2 <- list()
  ilist2$residences    <- do.call(rbind, lapply(results, function(x) x[[2]]))
  ilist2$residenceslog <- do.call(rbind, lapply(results, function(x) x[[1]]))
  ilist2$nonresidences <- do.call(rbind, lapply(results, function(x) x[[3]]))
  
  # Name output columns as original
  if (nrow(ilist2$residences) > 0 && "RECEIVERID" %in% colnames(ilist2$residences)) {
    names(ilist2$residences)[which(names(ilist2$residences) == "RECEIVERID")] <- sLocation
  }
  if (nrow(ilist2$residenceslog) > 0 && "RECEIVERID" %in% colnames(ilist2$residenceslog)) {
    names(ilist2$residenceslog)[which(names(ilist2$residenceslog) == "RECEIVERID")] <- sLocation
  }
  if (nrow(ilist2$nonresidences) > 0) {
    names(ilist2$nonresidences)[which(names(ilist2$nonresidences) == "RECEIVERID1")] <- paste0(sLocation, "1")
    names(ilist2$nonresidences)[which(names(ilist2$nonresidences) == "RECEIVERID2")] <- paste0(sLocation, "2")
  }
  ilist2$residences$DURATION <- as.numeric(difftime(ilist2$residences$ENDTIME, ilist2$residences$STARTTIME, units = "secs"))
  as.list(ilist2)
}


# run forest run ----------------------------------------------------------

tic("RunResidenceExtraction")
myoutput <- RunResidenceExtraction(
    dat2, # DATAFRAME NAME 
    sLocation = "STATIONNAME",       # or "RECEIVERID"
    iResidenceThreshold = 1,         # Any integer ≥ 1
    iTimeThreshold = 60*60*24*2,     # Any positive numeric (seconds)
    sDistanceMatrix = NULL,          # your distance matrix 
    n_workers = 5)                    # Number of cores
toc()

#TID.Res_all.Logs <- TID.Res_all$residenceslog  # Explore Residences log
myoutput.logs <- myoutput$residences

TID.Res.Movements <- myoutput$nonresidences # Explore Non-Residences/Movements
TID.Res.Movements <- TID.Res.Movements[ , -c(8, 9)] #remove unnessecary columns

TID.Res.Movements <- TID.Res.Movements %>%
  filter(STATIONNAME1 != STATIONNAME2) #remove movements that return to the same location
head(tid)
