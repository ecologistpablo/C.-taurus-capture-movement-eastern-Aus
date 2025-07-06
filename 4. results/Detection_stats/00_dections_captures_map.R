# 4nd July 2025
# Pablo Fuenzalida
# Spatial maps of sites specifically

# libraries ---------------------------------------------------------------

pacman::p_load("tidyverse", "viridis", "ggpubr", "plotly", "sf", "rnaturalearth", "ggspatial",
               "terra")

# Data --------------------------------------------------------------------

rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250701_det_enviro_complete.csv")
aus_shp <- ne_countries(scale = "large", country = "Australia", returnclass = "sf")
catches  <- read_csv("Inputs/240903_SCP_enviro.csv")

dat <- read_csv("inputs/250705_step3.csv")
dat1 <- dat %>% 
  filter(Location == "Ballina") %>%
  distinct(Location, detection_datetime, tag_id, station_name,
           receiver_deployment_longitude, receiver_deployment_latitude) %>% 
  rename(latitude = receiver_deployment_latitude,
         longitude = receiver_deployment_longitude)

SCP <- catches %>% 
  filter(Area %in% c("Ballina", "Evans Head"))


buffer_deg <- 0.3 # how far a box do you want around your data ? 
dat <- dat %>% 
  filter(presence == 1) #only real data shown


wolf <- dat %>% filter(location == "Wolf Rock")
wolf_catches <- catches %>% filter(Location == "Rainbow Beach")
flat     <- dat %>% filter(location == "Flat Rock")
coffs    <- dat %>% filter(location == "Coffs Harbour")
hawks    <- dat %>% filter(location == "Hawks Nest")
sydney   <- dat %>% filter(location == "Sydney")

# bounding box for maps ---------------------------------------------------

# bounding box 
make_window <- function(df, buffer = 0.3) { #buffer is in degrees = approx. 30km
  lon_centre <- mean(df$longitude, na.rm = TRUE)
  lat_centre <- mean(df$latitude, na.rm = TRUE)
  list(xlim = c(lon_centre - buffer, lon_centre + buffer),
       ylim = c(lat_centre - buffer, lat_centre + buffer))
}

# make detections an SF object --------------------------------------------
datxy <- dat1 %>%
    group_by(Location, latitude, longitude) %>%
    summarise(Detections = n(), .groups = 'drop')

datxy_sf <- sf::st_as_sf(datxy, coords = c("longitude", "latitude"), #sf object
                           crs = 4326, agr = "constant")
  
# make catches an sf object -----------------------------------------------
  
datxy1 <- SCP %>% 
    group_by(Location, Latitude, Longitude) %>% 
    summarise(Captures = n(), .groups = 'drop') %>% 
    rename(latitude = Latitude, 
           longitude = Longitude)

datxy_sf1 <- sf::st_as_sf(datxy1, coords = c("Longitude", "Latitude"), #sf object
                         crs = 4326, agr = "constant")

bbox <- make_window(datxy1, buffer = 0.5) # bounding box
  
ggplot() +
    geom_sf(data = aus_shp, fill = "grey", colour = "black") +
    geom_sf(data = datxy_sf, aes(size = Detections), alpha = 1) +
    geom_sf(data = datxy_sf1, aes(size = Captures), alpha = 1.0, colour = "firebrick") +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE) +
    labs(title = "Ballina") +
    theme_bw() +
    annotation_scale(location = "br") +
    annotation_north_arrow(style = north_arrow_nautical, location = "tr") +
    theme(plot.title = element_text(hjust = 0.5))



# git plottin' ------------------------------------------------------------

a <- plot_location_map(wolf, "A) Wolf Rock", aus_shp, topo = topo, buffer = buffer_deg)
b <- plot_location_map(flat,    "B) Nt. Stradbroke Island", aus_shp, topo = topo, buffer = buffer_deg)
c <- plot_location_map(coffs,   "C) Coffs Harbour", aus_shp, topo = topo, buffer = buffer_deg)
d <- plot_location_map(hawks,   "D) Hawks Nest", aus_shp, topo = topo, buffer = buffer_deg)
e <- plot_location_map(sydney,  "E) Sydney", aus_shp, topo = topo, buffer = buffer_deg)

a #did it work ?
b

# save maps individually --------------------------------------------------

ggsave("Outputs/Graphs/Final/detections/250705_WR_map.pdf", plot = a, width = 5, height = 5)
ggsave("Outputs/Graphs/Final/detections/250705_FR_map.pdf", plot = b, width = 5, height = 5)
ggsave("Outputs/Graphs/Final/detections/250705_CH_map.pdf", plot = c, width = 5, height = 5)
ggsave("Outputs/Graphs/Final/detections/250705_HN_map.pdf", plot = d, width = 5, height = 5)
ggsave("Outputs/Graphs/Final/detections/250705_SYD_map.pdf", plot = e, width = 5, height = 5)

# the combo ---------------------------------------------------------------

z <- ggarrange(a, b, c, d, e, ncol = 2, nrow = 3)
z

ggsave("Outputs/Graphs/Final/detections/250705_location-specific_map.pdf",
       plot = z, width = 10, height = 12)

# how do make 5 beautiful maps in 82 lines