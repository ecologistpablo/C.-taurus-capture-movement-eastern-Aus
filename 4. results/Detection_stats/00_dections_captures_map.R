# 4nd July 2025
# Pablo Fuenzalida
# Spatial maps of sites specifically

# libraries ---------------------------------------------------------------

pacman::p_load("tidyverse", "viridis", "ggpubr", "plotly", "sf", "rnaturalearth", "ggspatial",
               "terra", "readr")

# Data --------------------------------------------------------------------

rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
aus_shp <- st_read("Australia_shp/AUS_2021_AUST_GDA94.shp")
catches  <- read_csv("Inputs/240903_SCP_enviro.csv")
dat <- read_csv("inputs/250706_step7.csv")

dat1 <- dat %>% 
  filter(location == "Sunshine Coast") %>%
  distinct(location, date, tag_id, station_name,
           longitude, latitude, movement_type)

SCP <- catches %>% 
  filter(Area %in% c("Sunshine Coast"))


buffer_deg <- 0.3 # how far a box do you want around your data ? 
dat <- dat %>% 
  filter(presence == 1) #only real data shown


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
    group_by(location, latitude, longitude) %>%
    summarise(Detections = n(), .groups = 'drop')

datxy_sf <- sf::st_as_sf(datxy, coords = c("longitude", "latitude"), #sf object
                           crs = 4326, agr = "constant")
  
# make catches an sf object -----------------------------------------------
  
datxy1 <- SCP %>% 
    group_by(Location, Latitude, Longitude) %>% 
    summarise(Captures = n(), .groups = 'drop') %>% 
    rename(latitude = Latitude, 
           longitude = Longitude)

datxy_sf1 <- sf::st_as_sf(datxy1, coords = c("longitude", "latitude"), #sf object
                         crs = 4326, agr = "constant")

bbox <- make_window(datxy1, buffer = 0.5) # bounding box
  
ggplot() +
    geom_sf(data = aus_shp, fill = "grey", colour = "black") +
    geom_sf(data = datxy_sf, aes(size = Detections), alpha = 1) +
    geom_sf(data = datxy_sf1, aes(size = Captures), alpha = 1.0, colour = "firebrick") +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE) +
    labs(title = "Sunshine Coast") +
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