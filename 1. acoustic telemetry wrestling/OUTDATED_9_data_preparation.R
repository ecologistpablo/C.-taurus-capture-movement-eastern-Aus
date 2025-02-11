#230718
  #cleaning up
    #step 7

rm(list=ls()) 
source("/Users/owuss/Documents/USC/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/241122_step8.csv")

# just some more data curation and wrestling


# gloves on? let's wrestle ------------------------------------------------

unique(dat$Location) #what do we have ?

# Define the location order and which Locations we want to keep
location_order <- c("Wolf Rock", "Brisbane",  "Coffs Harbour",
                    "Port Macquarie", "Seal Rocks", "Hawks Nest", "Sydney",
                    "Jervis Bay", "Montague Island")

# Convert Location to factor with specified order
dat$Location <- factor(dat$Location, levels = location_order)


# Function to keep only certain locations
keep_locations <- function(df, locations) {
  df_filtered <- df %>%
    filter(Location %in% locations)
  return(df_filtered)
}

# Use the function to filter IMOS6
dat1 <- keep_locations(dat, location_order)

colnames(dat1)
dat2 <- dat1 %>% dplyr::select(-c(1:9, 14:19, 21:34))

colnames(dat2)
dat3 <- dat2 %>% 
  mutate(month = lubridate::month(detection_datetime)) %>% 
  rename(Sex = animal_sex)

 
# plot it -----------------------------------------------------------------

# Calculate the number of detections at each station
IMOSxy <- dat3 %>%
  group_by(Location, receiver_deployment_latitude, receiver_deployment_longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

IMOSxy_sf <- sf::st_as_sf(IMOSxy, coords = c("receiver_deployment_longitude", "receiver_deployment_latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(IMOSxy_sf, cex = "num_det", zcol = "Location", fbg = F)

# nice, gaaaarrrrryyy

#save it ----------------------------------------------------------------------------

write_csv(dat3, file = "Inputs/241122_step9.csv")

