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
  dplyr::select(Tag_ID, measurement, transmitter_deployment_datetime,
                transmitter_deployment_longitude, transmitter_deployment_latitude)

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
    Total_Movements = Number_of_Arrivals + Number_of_Departures,
    Tagging_longitude = transmitter_deployment_longitude,
    Tagging_latitude = transmitter_deployment_latitude) %>% 
  ungroup() %>% 
  distinct(Tag_ID, .keep_all = T)
colnames(table)

# clean our table up ------------------------------------------------------

table$Transmitter_Deployment_Datetime <- as.Date(table$Transmitter_Deployment_Datetime, format= "Y-%m%d")

table1 <- table %>%
  mutate(Measurement = as.numeric(str_extract(Measurement, "\\d+")))  # Extracts only the number part

table1$Measurement 

# that worked, but some are in meters and centimeters, lets change that with a lil ifelse function
table2 <- table1 %>%
  mutate(Measurement = if_else(Measurement < 10, Measurement * 100, Measurement))
table2$Measurement

# View the result
view(table2)
head(table2$Locations_Detected)


# re-ordering -------------------------------------------------------------

#almost perfect, let's re-order the locations from north - south

# our preferred order of locations
preferred_order <- c("Wolf Rock", "Moreton Island", "Flat Rock", "Coffs Harbour", 
                     "Hawks Nest", "Sydney", "Jervis Bay")

# Define a function that orders locations based on the predefined order
reorder_locations <- function(location_string) {
  locations <- strsplit(location_string, ",\\s*")[[1]]  # Split into individual locations
  factor_locations <- factor(locations, levels = preferred_order)  # Convert to factor with levels
  ordered_locations <- sort(factor_locations)  # Sort according to factor levels
  paste(ordered_locations, collapse = ", ")  # Combine back into a single string
}

# Apply the function to the Locations_Detected column using mutate
table3 <- table2 %>%
  mutate(Locations_Detected = sapply(Locations_Detected, reorder_locations))

# View the modified dataframe
print(table3)

#dplyr for the win, nice and simple

# save --------------------------------------------------------------------

write_csv(table3, "outputs/240513_Tag_ID_table.csv")
