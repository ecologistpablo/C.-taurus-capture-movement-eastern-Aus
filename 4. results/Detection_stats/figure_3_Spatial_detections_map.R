#19.10.2023

rm(list=ls())
#bring and clean data environment
setwd("~/Documents/USC/Honours/R/data")
pacman::p_load(tidyverse, ggspatial, tidyterra, terra, sf, sp)
dat <- read_rds("Inputs/250827_det_enviro_complete.rds")
Aus <- st_read("Australia_shp/AUS_2021_AUST_GDA94.shp") # try both
IMOS <- read_csv("Inputs/250723_step2.csv") 

IMOS <- IMOS %>% 
  distinct(station_name, .keep_all = T)

dat <- dat %>% 
  filter(presence != 0, 
         !location %in% c("Moreton Island", "Yamba", "Forster",'Central Coast', 'Merimbula'))
unique(dat$location)


# latitudional bands ------------------------------------------------------

# 1) One row per station with its coords and location label
bands <- dat %>%
  distinct(location, latitude, longitude)

# 2) Pick the "central" receiver in each location
#    (closest to the location's median latitude — adjust if you prefer mean/other rule)
central_recv <- bands %>%
  group_by(location) %>%
  slice_min(abs(latitude - median(latitude)), n = 1, with_ties = FALSE) %>%
  ungroup()

# 3) Build one north–south band per location using only those central receivers
bands <- central_recv %>%
  mutate(ymin = latitude - 0.2478,
    ymax = latitude + 0.2478,
    xmin = 149, xmax = 155)

# munging -----------------------------------------------------------------

dat1 <- dat %>% 
  group_by(location, longitude, latitude) %>% 
  summarise(num_det = n()) %>% 
  ungroup()
  
# shp map -----------------------------------------------------------------

m <- 
  ggplot() +
  geom_rect(data = bands, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "grey60", alpha = 0.3) + 
  geom_sf(data = Aus) +  # Shapefile layer
  geom_point(data = IMOS, aes(x = longitude, y = latitude), colour = "red", alpha = 0.5, size = 0.5) +  # Points layer from dato in grey
  geom_point(data = dat1, aes(x = longitude, y = latitude, colour = location, size = num_det)) +  # Points layer
  scale_size_continuous(range = c(1, 4)) +
  labs(x = NULL, y = NULL)+
  theme_minimal() +
  coord_sf(xlim = c(149, 154), ylim = c(-37, -23.5)) +  # Zoom into specific lat-lon box
  annotation_scale(location = "bl") +
  annotation_north_arrow(style = north_arrow_nautical, location = "tl") #+ 
  #scale_colour_viridis_d(option = "D", direction = -1) 

plot(m)

ggsave(path = "Outputs/Graphs/Final/detections", "250828_det_spatial_map.tiff",
       plot = m, width = 6, height = 8) #in inches because gg weird
