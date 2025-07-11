# 11 July 2025
  # VTrack parallel processing is broken
    # So we will loop each tag individually

# By Pablo Fuenzalida

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
pacman::p_load("tidyverse", "VTrack", "lubridate", 'furrr', 'purrr', 'future',
               'parallelly')
dat <- read.csv("Inputs/250708_step3.csv")

# prep data ---------------------------------------------------------------

# Prepare and clean data
dat1 <- dat %>%
  mutate(transmitter_sensor_raw_value = 0,
         transmitter_sensor_unit = 0) %>%
  transmute(
    DATETIME = with_tz(ymd_hms(datetime, tz = "UTC"), tzone = "Etc/GMT-10"),
    TRANSMITTERID = as.factor(tag_id),
    SENSOR1 = as.numeric(transmitter_sensor_raw_value),
    UNITS1 = as.numeric(transmitter_sensor_unit),
    RECEIVERID = unlist(data.table::tstrsplit(receiver_name, "-", keep = 2)),
    STATIONNAME = as.factor(location)) %>%
  dplyr::select(DATETIME,TRANSMITTERID,SENSOR1,UNITS1,RECEIVERID,STATIONNAME) %>% 
  drop_na()

# has to have no NAs
# has to be in the order specified
# has to use the names specified
# has to use the structures specified

# purrr away bby ----------------------------------------------------------

# Set up parallel plan globally
plan(multisession, workers = 4) # parallelly

# List of unique tags
tag_list <- unique(dat1$TRANSMITTERID)

# Directory to save individual outputs
dir.create("Inputs", showWarnings = T) # direct output folder

# Function to run VTrack on a single tag
run_vtrack_safe <- function(tag_id) {
  file_path <- glue::glue("Inputs/250711_{tag_id}_VTrack.RData")
  
  # Check if file exists and is valid
  if (file.exists(file_path)) {
    # Load the object temporarily
    tmp_env <- new.env()
    load(file_path, envir = tmp_env)
    res <- tmp_env$result
    
    # Validate: is it a list/data.frame and not suspiciously small?
    if (is.null(res) || (is.data.frame(res) && nrow(res) == 0)) {
      message("Re-running tag ", tag_id, " — existing file is empty or incomplete.")
    } else {
      message("Skipping tag ", tag_id, " — file already exists and looks OK.")
      return(invisible(NULL))
    }
  }
  
  
  message("Processing tag: ", tag_id)
  
  tag_df <- dat1 %>%
    filter(TRANSMITTERID == tag_id)
  
  # Run the VTrack function safely
  result <- tryCatch({
    VTrack::RunResidenceExtraction(
      sInputFile = tag_df,
      sLocation = "STATIONNAME",
      iResidenceThreshold = 1,
      iTimeThreshold = 60 * 60 * 24 * 15, # 15 days
      sDistanceMatrix = NULL,
      iCores = 1 # must be 1 due to known bug
    )
  }, error = function(e) {
    message("Error for tag ", tag_id, ": ", e$message)
    return(NULL)
  })
  
  if (!is.null(result)) {
    save(result, file = file_path)
    message("Saved result for tag ", tag_id)
  }
  
  invisible(NULL)
}

# Run in parallel using furrr::future_walk (but call is still serial inside each)
future_walk(tag_list, run_vtrack_safe, .progress = TRUE)
