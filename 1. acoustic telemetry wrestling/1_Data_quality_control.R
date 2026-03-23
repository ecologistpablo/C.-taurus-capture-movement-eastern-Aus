#14.06.23
  #A clean r workflow means a clean life
    #P. Fuenzalida
      #cleaning data

# load library & data ----------------------------------------------------------

rm(list=ls())
pacman::p_load("remora", "lubridate", "tidyverse", "rsp")

#Quality control ---------------------------------------------------------------

setwd("/Users/owuss/Documents/USC/Honours/R/data/IMOS/IMOS_detections_2026-03-23_12-50-00")
list.files()
files <- list(det = "IMOS_detections.csv",meas = "IMOS_animal_measurements.csv")

#Quality control of REMORA
tag_qc <- remora::runQC(files, 
                .parallel = parallel::detectCores() -2 ,  #leave me 2 cores
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

qc_data2 <- qc_data1 %>% #filter out 2026
  filter(!format(detection_datetime, "%Y") %in% c("2026"))

qc_data1$tag_id <- str_extract(qc_data1$transmitter_id, "\\d{4,5}$") #shorten tag ID strings


qc_data2 <- qc_data1 %>% 
  distinct(tag_id, detection_datetime, 
           receiver_deployment_longitude, receiver_deployment_latitude,
           station_name, receiver_name) %>% 
  rename(datetime = detection_datetime,
         latitude = receiver_deployment_latitude,
         longitude = receiver_deployment_longitude)


sum(is.na(qc_data2$tag_id))
head(qc_data2$tag_id)

colnames(qc_data2)

# save it ----------------------------------------------------------------------

rm(files)
rm(tag_qc)
rm(WA)

setwd("/Users/owuss/Documents/USC/Honours/R/data")
write_rds(qc_data2, file = "Inputs/260323_qc_data.rds")

# Dwyer dat ---------------------------------------------------------------

ddat <- read_csv("Dwyer dat/dwyerdat_230615.csv")
ddat <- janitor::clean_names(ddat)
colnames(ddat)

ddat$tag_id <- str_extract(ddat$transmitter, "\\d{4,5}$") #shorten tag ID strings

ddat1 <- ddat %>% 
  rename(receiver_name = receiver) %>% 
   mutate(datetime = dmy_hm(datetime)) %>% #convert to datetime str 
  mutate(datetime = datetime + hours(10)) %>%  # convert UTC to EST 
  dplyr::distinct(station_name, tag_id, latitude, longitude, 
           datetime, sex, receiver_name)

ddat1 %>%
  count(tag_id, datetime, station_name, latitude, longitude, sex) %>%
  filter(n > 1) # 


# Reorder columns in 'ddat' to match 'qc_data'
ddat2 <- ddat1[colnames(qc_data2)]
colnames(ddat2)
colnames(qc_data2)
str(ddat2)
str(qc_data2)
# are they the same str and format ? 

# bind dataframes ---------------------------------------------------------

adat <- bind_rows(qc_data2, ddat2)

# #Because we joined two dfs, let's make sure there arent duplicate rows
adat1 <- adat %>%
  distinct(tag_id, station_name, datetime, latitude, longitude,
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
anyNA(adat2$latitude)

# save it ----------------------------------------------------------------------

write_csv(adat2, file = "Inputs/260323_step1.csv")
#load("Inputs/250708_step1.RData")

# NSW DPI dat -------------------------------------------------------------

bdat <- read_csv("Dwyer dat/GNS_det_data_Paul_Butcher_230904.csv")

str(bdat)

# Step 1: Rename existing columns in 'ddat' to match corresponding columns in 'qc_data'
bdat1 <- bdat %>%
  transmute(tag_id = ID,
    sex = Sex,
    latitude = `VR4G Lat.`,
    longitude = `VR4G Long.`,
    station_name = Location,
    datetime= detection_datetime) %>%
  mutate(datetime = dmy_hm(datetime),
         tag_id = as.character(tag_id),
         receiver_name = as.character('VR4G')) %>% 
  distinct(tag_id, station_name, datetime, latitude, longitude,
           receiver_name)

str(bdat1)


# re-order columns to be the same as our final df
bdat2 <- bdat1[, colnames(adat2)]

#bind rows from bottom to top
zdat <- bind_rows(adat2, bdat2)

zdat1 <- zdat


zdat2 <- zdat1 %>%
  distinct(datetime, tag_id, station_name, latitude, 
           longitude, receiver_name, .keep_all = TRUE) %>% 
  mutate(sex = NA)

str(zdat2)
# save it ----------------------------------------------------------------------

write_csv(zdat2, file = "Inputs/260323_step1.csv")
# 01 September, adding in SEACAMs data from last few years:

# Y Niella dat ------------------------------------------------------------

dat <- read_csv("IMOS/250901_YNiella_SEACAMS_dat.csv")

dat$tag_id <- str_extract(dat$transmitter_id, "\\d{4,5}$") #shorten tag ID strings


dat1 <- dat %>% 
  rename(datetime = detection_datetime,
         latitude = receiver_deployment_latitude,
         longitude = receiver_deployment_longitude,
         sex = animal_sex) %>% 
  select(sex, datetime, longitude, latitude, station_name, receiver_name, tag_id) %>% 
  mutate(tag_id = as.character(tag_id))


zdat3 <- bind_rows(zdat2, dat1)

zdat4 <- zdat3 %>% 
  distinct(datetime, tag_id, longitude, latitude, 
           station_name, receiver_name, sex) # no double-ups


write_rds(zdat4, file = "Inputs/260323_step1.rds")

