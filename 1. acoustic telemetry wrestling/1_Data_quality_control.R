#14.06.23
  #A clean r workflow means a clean life
    #P. Fuenzalida
      #cleaning data

# load library & data ----------------------------------------------------------

rm(list=ls())
pacman::p_load("tidyverse", "parallel", "remora", "lubridate", "rnaturalearth")

#Quality control ---------------------------------------------------------------

setwd("/Users/owuss/Documents/USC/Honours/R/data/IMOS/IMOS_detections_2024-11-16_09-24-44")
files <- list(det = "IMOS_detections.csv",meas = "IMOS_animal_measurements.csv")

#Quality control of REMORA
tag_qc <- remora::runQC(files, 
                .parallel = detectCores() - 2,  #leave me 2 cores
                .progress = TRUE)  #show me progress

#unnest
qc_data <- tag_qc %>%
  unnest(cols = QC) %>%
  ungroup()

#clean up our df, qc gave us useless columns
qc_data <- qc_data %>%
  dplyr::select(-c(34:42))

# remove Western Australia detections, as we focus on eastern Aus
WA <- c("SL9", "SL18", "SL19", "SL2", "SL22", "PRT24", "PRT27", "PRT28",
        "PRT31", "PRT35", "PRT36", "PRT37",
        "PRT47", "PRT48", "PRT49", "PRT63", "PRT74", "PRT73", "PRT75")


# Filter out the rows with station names in the list
qc_data1 <- qc_data %>%
  filter(!(station_name %in% WA))

#convert to date
qc_data1$detection_datetime <- as.POSIXct(qc_data1$detection_datetime)

qc_data2 <- qc_data1 %>% #filter out 2023 and 2024
  filter(!format(detection_datetime, "%Y") %in% c("2023", "2024"))


# Get Australia map data
aus <- ne_countries(scale = "medium", country = "australia", returnclass = "sf")

unique_det <- qc_data2 %>%
  mutate(date = as.Date(detection_datetime)) %>% 
  distinct(date, receiver_deployment_longitude, receiver_deployment_latitude,
           station_name) 

# Create the map
ggplot() +
  geom_sf(data = aus, fill = "lightgrey") +
  geom_point(data = unique_det, aes(x = receiver_deployment_longitude, 
                 y = receiver_deployment_latitude), size = 1) +
  coord_sf(xlim = c(145, 155), ylim = c(-38, -20)) + # Focus on east coast
  theme_minimal()
rm(unique_det)

qc_data3 <- qc_data2 %>% 
  distinct(transmitter_id, animal_sex, detection_datetime, 
           receiver_deployment_longitude, receiver_deployment_latitude,
           station_name) %>% 
  rename(sex = animal_sex,
         datetime = detection_datetime,
         latitude = receiver_deployment_latitude,
         longitude = receiver_deployment_longitude)

colnames(qc_data3)

# save it ----------------------------------------------------------------------

rm(files)
rm(tag_qc)
rm(WA)

setwd("/Users/owuss/Documents/USC/Honours/R/data")
write_csv(qc_data3, file = "Inputs/250708_qc_data.csv")
qc_data <- read_csv("Inputs/241116_qc_data.csv")

# Dwyer dat ---------------------------------------------------------------

ddat <- read_csv("Inputs/230806_step0.csv")
ddat <- janitor::clean_names(ddat)
colnames(ddat)

ddat1 <- ddat %>% 
  rename(transmitter_id = transmitter) %>% 
   mutate(datetime = dmy_hm(datetime)) %>% #convert to datetime str 
  mutate(datetime = datetime + hours(10)) %>%  # convert UTC to EST 
  distinct(station_name, transmitter_id, latitude, longitude, 
           datetime, sex)
  
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
           .keep_all = TRUE) #keep everything else
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
#save(adat, file = "Inputs/230807_step1.RData")

load("Inputs/250708_step1.RData")

# NSW DPI dat -------------------------------------------------------------
#06.09.23

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
         transmitter_id = as.character(transmitter_id))

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
           sex, .keep_all = TRUE) %>% 
  select(-transmitter_id) # we have tag ids as a shortened version now

# save it ----------------------------------------------------------------------

write_csv(zdat2, file = "Inputs/250708_step1.csv")

# 1,702,983 unique rows per second, tag id and receiver

# plot it -----------------------------------------------------------------

# just to check there are no outliers
datxy <- zdat2 %>%
  group_by(station_name, latitude, longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(datxy, coords = c("longitude",
                        "latitude"), crs = 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "station_name", fbg = FALSE)
