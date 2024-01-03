#19.10.2023

rm(list=ls())
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#bring and clean data environment
setwd("~/University/2023/Honours/R/data")
dato <- read_csv("Inputs/230906_step2.csv")

dat <- read_csv("Inputs/230912a_complete_det_enviro.csv")

# Aus shpfle --------------------------------------------------------------

Aus <- st_read("Australia_shp/Australia.shp") #Read in our data

Au <- Aus %>% 
  filter(ADMIN_NAME %in% c("Queensland", "New South Wales", "Victoria"))
plot(Au)


# munging -----------------------------------------------------------------

dat1 <- dat %>% 
  group_by(Location, receiver_deployment_longitude, receiver_deployment_latitude) %>% 
  summarise(num_det = n(), .groups = 'drop') %>% 
  ungroup()
  
#dat1 <- dat1 %>% 
#mutate(Location = ifelse(str_starts(Location, "deg_"), "other", Location))

dat1 <- dat1 %>% 
  mutate(Location = fct_relevel(Location,"Wolf Rock", "Moreton Island","Flat Rock", "Coffs Harbour", 
                            "Port Macquarie", "Seal Rocks","Hawks Nest", "Sydney",
                            "Jervis Bay", "Montague Island"))


dat2 <- dat1 %>% 
  filter(Location  %in% c("Wolf Rock", "Moreton Island","Flat Rock", "Coffs Harbour", 
                         "Hawks Nest", "Sydney", "Jervis Bay"))

# shp map -----------------------------------------------------------------


m <-
ggplot() +
  geom_sf(data = Au) +  # Shapefile layer
  geom_point(data = dato, aes(x = receiver_deployment_longitude, y = receiver_deployment_latitude), colour = "black", size = 1) +  # Points layer from dato in grey
  geom_point(data = dat2, aes(x = receiver_deployment_longitude, y = receiver_deployment_latitude, colour = Location, size = num_det)) +  # Points layer
  labs(title = "Acoustic telemetry detections of C. taurus in Eastern Australia [2012 - 2022]",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  scale_colour_viridis_d(direction = -1) +
  coord_sf(xlim = c(149, 160), ylim = c(-37, -25)) +  # Zoom into specific lat-lon box
  annotation_scale(location = "bl") +
  annotation_north_arrow(style = north_arrow_nautical, location = "tl")
plot(m)

ggsave(path = "Outputs/Graphs/Polishing/Spatial", "231227_det_spatial_map.pdf",
       plot = m, width = 16, height = 10) #in inches because gg weird
