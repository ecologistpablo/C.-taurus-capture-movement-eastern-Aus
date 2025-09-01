#14.06.23
  #A clean r workflow means a clean life
    #P. Fuenzalida
      #cleaning data

# load library & data ----------------------------------------------------------

rm(list=ls())
pacman::p_load("parallel", "remora", "lubridate", "tidyverse", "rsp")

#Quality control ---------------------------------------------------------------

setwd("/Users/owuss/Documents/USC/Honours/R/data/IMOS/IMOS_detections_2025-09-01_13-19-40")
list.files()
files <- list(det = "IMOS_detections.csv",meas = "IMOS_animal_measurements.csv")

#Quality control of REMORA
tag_qc <- remora::runQC(files, 
                .parallel = detectCores() - 1,  #leave me 2 cores
                .progress = TRUE)  #show me progress

#unnest

qc_data <- grabQC(tag_qc, what = "dQC",
                  flag = c("valid", "likely valid")) # only grab likely detections

# remove Western Australia detections, as we focus on eastern Aus
WA <- c("SL9", "SL18", "SL19", "SL2", "SL22", "PRT24", "PRT27", "PRT28",
        "PRT31", "PRT35", "PRT36", "PRT37",
        "PRT47", "PRT48", "PRT49", "PRT63", "PRT74", "PRT73", "PRT75")

# Filter out the rows with station names in the list
qc_data1 <- qc_data %>%
  filter(!(station_name %in% WA))

#convert to date
qc_data1$detection_datetime <- as.POSIXct(qc_data1$detection_datetime)

# qc_data2 <- qc_data1 %>% #filter out 2023 and 2024
#   filter(!format(detection_datetime, "%Y") %in% c("2023", "2024"))

qc_data2 <- qc_data1 %>% 
  distinct(transmitter_id, detection_datetime, 
           receiver_deployment_longitude, receiver_deployment_latitude,
           station_name, receiver_name) %>% 
  rename(datetime = detection_datetime,
         latitude = receiver_deployment_latitude,
         longitude = receiver_deployment_longitude)

colnames(qc_data2)

# save it ----------------------------------------------------------------------

rm(files)
rm(tag_qc)
rm(WA)

setwd("/Users/owuss/Documents/USC/Honours/R/data")
write_rds(qc_data2, file = "Inputs/250901_qc_data.rds")

# Dwyer dat ---------------------------------------------------------------

ddat <- read_csv("Inputs/230806_step0.csv")
ddat <- janitor::clean_names(ddat)
colnames(ddat)

ddat1 <- ddat %>% 
  rename(transmitter_id = transmitter,
         receiver_name = receiver) %>% 
   mutate(datetime = dmy_hm(datetime)) %>% #convert to datetime str 
  mutate(datetime = datetime + hours(10)) %>%  # convert UTC to EST 
  distinct(station_name, transmitter_id, latitude, longitude, 
           datetime, sex, receiver_name)
  
ddat1 %>%
  count(transmitter_id, datetime, station_name, latitude, longitude, sex) %>%
  filter(n > 1) # 

# Reorder columns in 'ddat' to match 'qc_data'
ddat2 <- ddat1[colnames(qc_data3)]
colnames(ddat2)
colnames(qc_data3)
str(ddat2)
str(qc_data3)
# are they the same str and format ? 

# bind dataframes ---------------------------------------------------------

adat <- bind_rows(qc_data3, ddat2)

# #Because we joined two dfs, let's make sure there arent duplicate rows
adat1 <- adat %>%
  distinct(transmitter_id, station_name, datetime, latitude, longitude, sex,
           receiver_name, .keep_all = TRUE) #keep everything else
# 
# #NA check, do we have any coords that are NA ? 
which(is.na(adat1$longitude))

# what are their station names lets look them up manually

# Replace lat/lon for missing values (physically inspected XY coordinates and corelated to IMOS database)
adat2 <- adat1 %>%
  mutate(latitude = replace(latitude, 
         station_name == 'Cabbage Tree Island - West', -31.97845),
         longitude = replace(longitude, 
         station_name == 'Cabbage Tree Island - West', 152.59832),
         latitude = replace(latitude, 
         station_name == 'SIMP 9 - Groper Island', -30.16000),
         longitude = replace(longitude, 
         station_name == 'SIMP 9 - Groper Island', 153.2300))

anyNA(adat2$longitude)

# save it ----------------------------------------------------------------------

write_csv(adat2, file = "Inputs/230708_step1.csv")
#load("Inputs/250708_step1.RData")

# NSW DPI dat -------------------------------------------------------------

bdat <- read_csv("Dwyer dat/GNS_det_data_Paul_Butcher_230904.csv")

str(bdat)

# Step 1: Rename existing columns in 'ddat' to match corresponding columns in 'qc_data'
bdat1 <- bdat %>%
  rename(transmitter_id = ID,
    sex = Sex,
    latitude = `VR4G Lat.`,
    longitude = `VR4G Long.`,
    station_name = Location,
    datetime= detection_datetime) %>%
  dplyr::select(-TL) %>% 
  mutate(datetime = dmy_hm(datetime),
         transmitter_id = as.character(transmitter_id),
         receiver_name = as.character('VR4G'))

str(bdat1)

# re-order columns to be the same as our final df
bdat2 <- bdat1[, colnames(adat2)]

#bind rows from bottom to top
zdat <- bind_rows(adat2, bdat2)

zdat1 <- zdat

zdat1$tag_id <- str_extract(zdat$transmitter_id, "\\d{4,5}$") #shorten tag ID strings
unique(zdat1$tag_id)

zdat2 <- zdat1 %>%
  distinct(datetime, tag_id, transmitter_id, station_name, latitude, longitude,
           sex, receiver_name, .keep_all = TRUE) %>% 
  select(-transmitter_id) # we have tag ids as a shortened version now

str(zdat2)
# save it ----------------------------------------------------------------------

write_csv(zdat2, file = "Inputs/250708_step1.csv")
# 01 September, adding in SEACAMs data from last few years:
IMOS <- read_csv("Inputs/250708_step1.csv")

list.files("IMOS")


# Y Niella dat ------------------------------------------------------------

dat <- read_csv("IMOS/250901_YNiella_SEACAMS_dat.csv")

dat$tag_id <- str_extract(dat$transmitter_id, "\\d{4,5}$") #shorten tag ID strings


dat1 <- dat %>% 
  rename(datetime = detection_datetime,
         latitude = receiver_deployment_latitude,
         longitude = receiver_deployment_longitude,
         sex = animal_sex) %>% 
  select(sex, datetime, longitude, latitude, station_name, receiver_name, tag_id) %>% 
  mutate(tag_id = as.double(tag_id))


IMOS1 <- bind_rows(IMOS, dat1)

IMOS2 <- IMOS1 %>% 
  distinct(datetime, tag_id, longitude, latitude, station_name, receiver_name, sex) # no double-ups


write_rds(IMOS2, file = "Inputs/250901_step1.rds")

# removing duplicates -----------------------------------------------------

qc_data <- read_rds("Inputs/250901_qc_data.rds") # combo dat of old qc, ddat, pb dat, and YN dat
IMOS <- read_rds("Inputs/250901_step1.1.rds") # qc dat from 51 09 01 

qc_data$tag_id <- str_extract(qc_data$transmitter_id, "\\d{4,5}$") #shorten tag ID strings

qc_data$tag_id <- as.double(qc_data$tag_id)

qc_data1 <- bind_rows(IMOS,qc_data)

# two decimal places
qc_data2 <- qc_data1 %>% 
mutate(latitude  = round(latitude, 2),
       longitude = round(longitude, 2))

qc_data3 <- qc_data2 %>% 
  distinct(tag_id, datetime, latitude, longitude, receiver_name, station_name)


write_rds(qc_data3, file = "Inputs/250901_step1.2.rds")

