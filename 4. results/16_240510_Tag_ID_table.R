#10.05.24
  #Creating tag ID table for manuscript 
    #addressing reviewer comments

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#bring and clean data environment
rm(list=ls())
setwd("~/University/2023/Honours/R/data")
#dat <- read_csv("Inputs/230726_step8.csv")
dat <- read_csv("Inputs/230912a_complete_det_enviro.csv") #final data (movements)
IMOS <- read_csv("Inputs/240114_step3.csv") #info re size


# lets check our data -----------------------------------------------------

colnames(IMOS)
IMOS1 <- IMOS %>% 
  distinct(Tag_ID, .keep_all = T)

unique(dat$Tag_ID)
unique(IMOS1$Tag_ID)

# Remove rows where Presence is equal to 0
dat1 <- dat[dat$Presence != 0, ]

dat2 <- dat1 %>% 
  filter(Location %in% c("Wolf Rock", "Moreton Island", "Flat Rock", "Coffs Harbour",
                         "Hawks Nest", "Sydney", "Jervis Bay"))

unique(dat2$Tag_ID)

# Extract relevant columns from the initial IMOS dataframe
IMOS2 <- IMOS1 %>% 
  dplyr::select(Tag_ID, measurement, transmitter_deployment_datetime)

# Perform a left join to merge this data into dat based on Tag_ID
dat3 <- dat2 %>%
  left_join(IMOS2, by = "Tag_ID")

str(dat3)
summary(dat3)
colnames(dat3)

# dat3 now has no pseudo absences, only our focused locations, and metadata
# we can work with that

# Create the summary table
table <- dat3 %>%
  group_by(Tag_ID) %>%
  summarise(
    Sex = first(Sex),  # Assuming sex does not change for a tag
    Measurement = measurement,  # First non-NA measurement
    Transmitter_Deployment_Datetime = first(transmitter_deployment_datetime),
    Most_Recent_Detection = max(detection_datetime, na.rm = TRUE),
    Locations_Detected = toString(unique(Location)),
    Number_of_Arrivals = sum(movement == "Arrival", na.rm = TRUE),
    Number_of_Departures = sum(movement == "Departure", na.rm = TRUE),
    Total_Movements = Number_of_Arrivals + Number_of_Departures) %>% 
  ungroup() %>% 
  distinct(Tag_ID, .keep_all = T)


# clean our table up ------------------------------------------------------

table$Transmitter_Deployment_Datetime <- as.Date(table$Transmitter_Deployment_Datetime, format= "Y-%m%d")

table1 <- table %>%
  mutate(
    Measurement = str_extract(Measurement, "\\d+\\s*[cm]*[m]*"),  # Extracts the number and unit
    Measurement = if_else(str_detect(Measurement, "m"),  # Check if 'm' is in the string
                          as.numeric(str_extract(Measurement, "\\d+")),  # Convert meters to cm
                          as.numeric(str_extract(Measurement, "\\d+")))  # Just extract the number if cm
  )
table1$Measurement 

# that worked, but some are in meters and centimeters, lets change that with a lil mutate function
table2 <- table1 %>%
  mutate(Measurement = if_else(Measurement < 10, Measurement * 100, Measurement))
table2$Measurement

# View the result
view(table2)

write_csv(table2, "outputs/240510_Tag_ID_table.csv")
