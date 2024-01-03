#14.06.23
  #A clean r workflow means a clean life
    #P. Fuenzalida
      #cleaning data

# load library & data ----------------------------------------------------------

rm(list=ls())
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#Quality control ---------------------------------------------------------------

setwd("~/University/2023/Honours/R/data/IMOS/IMOS_detections_2023-07-21_11-56-11") 

files <- list(det = "IMOS_detections.csv", #,
              #rmeta = "path_to/IMOS_receiver_deployment_metadata.csv",
              #tm %>% eta = "path_to/IMOS_transmitter_deployment_metadata.csv",
              meas = "IMOS_animal_measurements.csv")

#Quality control of REMORA
tag_qc <- runQC(files, .parallel = F, .progress = TRUE) 

#unnest
qc_data <- tag_qc %>%
  unnest(cols = QC) %>%
  ungroup()

#clean up our df, qc gave us useless columns
qc_data <- qc_data %>%
  select(-c(34:42))

# remove WA detections
WA <- c("SL9", "SL18", "SL19", "SL2", "SL22",
                   "PRT27", "PRT35", "PRT37", "PRT74", "PRT73", "PRT75")

# Filter out the rows with station names in the list
qc_data <- qc_data %>%
  filter(!(station_name %in% WA))

#convert to date
qc_data$detection_datetime <- as.Date(qc_data$detection_datetime)

#let's knock it down to one detection per day using distinct
qc_data <- qc_data %>%
  distinct(detection_datetime, transmitter_id, station_name, #keep only one per day (per tag id)
           .keep_all = TRUE) #only one row per day & keep everything else

qc_data$detection_corrected_datetime <- as.character(qc_data$detection_corrected_datetime)


# save it ----------------------------------------------------------------------

rm(files)
rm(tag_qc)
rm(WA)

setwd("~/University/2023/Honours/R/data")
write_csv(qc_data, file = "Inputs/230806_qc_data.csv")
qc_data <- read_csv("Inputs/230806_qc_data.csv")

# Dwyer dat ---------------------------------------------------------------

ddat <- read_csv("Inputs/230806_step0.csv")

#let's knock it down to one detection per day using distinct
ddat <- ddat %>%
  distinct(Date, Transmitter.Name, Station.Name, .keep_all = TRUE)

# merge data -------------------------------------------------------------------

# Step 1: Rename existing columns in 'ddat' to match corresponding columns in 'qc_data'
ddat <- ddat %>%
  rename(
    transmitter_id = Transmitter,
    station_name = Station.Name,
    receiver_name = Receiver,
    receiver_deployment_latitude = Latitude,
    receiver_deployment_longitude = Longitude,
    detection_datetime = Date,
    transmitter_serial_number = Transmitter.Name,
    detection_corrected_datetime = Date.and.Time..UTC.,
    transmitter_deployment_datetime = DATETIME
  )

# Step 2: Create new columns in 'ddat' to match those in 'qc_data' that don't have a corresponding column in 'ddat'.
# We'll fill these with NA for now
ddat <- ddat %>%
  mutate(
    filename = NA_character_,
    tag_id = NA_real_,
    transmitter_deployment_id = NA_real_,
    transmitter_deployment_datetime = NA_Date_,
        tagging_project_name = NA_character_,
    species_common_name = NA_character_,
    species_scientific_name = NA_character_,
    CAAB_species_id = NA_real_,
    WORMS_species_aphia_id = NA_real_,
    animal_sex = NA_character_,
    receiver_id = NA_real_,
    receiver_deployment_id = NA_real_,
    receiver_project_name = NA_character_,
    installation_name = NA_character_,
    transmitter_sensor_type = NA_character_,
    transmitter_sensor_raw_value = NA_real_,
    transmitter_sensor_unit = NA_character_,
    transmitter_sensor_slope = NA_real_,
    transmitter_sensor_intercept = NA_real_,
    transmitter_type = NA_character_,
    transmitter_estimated_battery_life = NA_real_,
    transmitter_status = NA_character_,
    transmitter_serial_number = NA_character_,
    transmitter_deployment_longitude = NA_real_,
    transmitter_deployment_latitude = NA_real_,
    transmitter_dual_sensor = NA_real_,
    Detection_QC = NA_real_
  )

# Step 3: Reorder columns in 'ddat' to match the order in 'qc_data'
ddat <- ddat[, colnames(qc_data)]

# Step 4: Convert data types of 'ddat' columns to match those in 'qc_data'
# need to use the appropriate conversion functions for each column depending on the expected data types in 'qc_data'
#convert date column
ddat$detection_datetime <- as.Date(ddat$detection_datetime)

#bind rows from both datastreams
adat <- bind_rows(qc_data, ddat)

#fix the damn tag_id column again
adat$tag_id <- str_extract(adat$transmitter_id, "\\d{4,5}$")

#Because we joined two dfs, let's make sure there arent duplicate rows
adat <- adat %>%
  distinct(detection_datetime, transmitter_id, station_name, #keep only one per day (per tag id)
           .keep_all = TRUE) #keep everything else

#NA check
which(is.na(adat$receiver_deployment_longitude))

# Replace lat/lon for missing values (physically inspected XY coordinates and corelated to IMOS database)
adat <- adat %>%
  mutate(receiver_deployment_latitude = replace(receiver_deployment_latitude, 
                                                station_name == 'Cabbage Tree Island - West', 
                                                -31.97845),
         receiver_deployment_longitude = replace(receiver_deployment_longitude, 
                                                 station_name == 'Cabbage Tree Island - West', 
                                                 152.59832),
         receiver_deployment_latitude = replace(receiver_deployment_latitude, 
                                                station_name == 'SIMP 9 - Groper Island', 
                                                -30.16000),
         receiver_deployment_longitude = replace(receiver_deployment_longitude, 
                                                 station_name == 'SIMP 9 - Groper Island', 
                                                 153.2300))

which(is.na(adat$receiver_deployment_longitude))

# save it ----------------------------------------------------------------------

write_csv(adat, file = "Inputs/230807_step1.csv")
save(adat, file = "Inputs/230807_step1.RData")

load("Inputs/230807_step1.RData")

# P. Butcher dat ----------------------------------------------------------
#06.09.23

bdat <- read_csv("Dwyer dat/GNS_det_data_Paul_Butcher_230904.csv")

str(bdat)

# Step 1: Rename existing columns in 'ddat' to match corresponding columns in 'qc_data'
bdat1 <- bdat %>%
  rename(
    tag_id = ID,
    animal_sex = Sex,
    receiver_deployment_latitude = `VR4G Lat.`,
    receiver_deployment_longitude = `VR4G Long.`,
    station_name = Location) %>%
  select(-TL)

# Step 2: Create new columns in 'ddat' to match those in 'qc_data' that don't have a corresponding column in 'ddat'.
# We'll fill these with NA for now.
bdat2 <- bdat1 %>%
  mutate(
    filename = NA_character_,
    transmitter_deployment_id = NA_character_,
    transmitter_deployment_datetime = NA_Date_,
    tagging_project_name = NA_character_,
    species_common_name = NA_character_,
    species_scientific_name = NA_character_,
    CAAB_species_id = NA_real_,
    WORMS_species_aphia_id = NA_real_,
    receiver_id = NA_real_,
    receiver_deployment_id = NA_real_,
    receiver_project_name = NA_character_,
    installation_name = NA_character_,
    transmitter_sensor_type = NA_character_,
    transmitter_sensor_raw_value = NA_real_,
    transmitter_sensor_unit = NA_character_,
    transmitter_sensor_slope = NA_real_,
    transmitter_sensor_intercept = NA_real_,
    transmitter_type = NA_character_,
    transmitter_estimated_battery_life = NA_real_,
    transmitter_status = NA_character_,
    transmitter_serial_number = NA_character_,
    transmitter_deployment_longitude = NA_real_,
    transmitter_deployment_latitude = NA_real_,
    transmitter_dual_sensor = NA_real_,
    Detection_QC = NA_real_,
    transmitter_id = NA_character_,
    receiver_name = NA_character_,
    detection_corrected_datetime = NA_character_,
  )

# Step 3: Reorder columns in 'ddat' to match the order in 'qc_data'
bdat3 <- bdat2[, colnames(adat)]

bdat3$detection_datetime <- as.Date(bdat3$detection_datetime, format="%d/%m/%Y")

bdat3$tag_id <- as.character(bdat3$tag_id)

bdat4 <- bdat3 %>% 
  mutate(transmitter_deployment_id = as.numeric(transmitter_deployment_id),
         animal_sex = as.character(animal_sex),
         receiver_id = as.numeric(receiver_id),
         receiver_deployment_id = as.numeric(receiver_deployment_id),
         transmitter_deployment_datetime = as.Date(transmitter_deployment_datetime)
  )

#bind rows from bottom to top
zdat <- bind_rows(adat, bdat4)

zdat1 <- zdat %>%
  distinct(detection_datetime, tag_id, station_name, #keep only one per day (per tag id)
           .keep_all = TRUE)

# save it ----------------------------------------------------------------------

write_csv(zdat1, file = "Inputs/230906_step1.csv")
save(zdat1, file = "Inputs/230807_step1.RData")

