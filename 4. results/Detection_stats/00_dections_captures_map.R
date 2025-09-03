# 4nd July 2025
# Pablo Fuenzalida
# Spatial maps of sites specifically

# libraries ---------------------------------------------------------------

pacman::p_load("tidyverse", "viridis", "ggpubr", "plotly", "sf", "rnaturalearth", "ggspatial",
               "terra", "readr")

# Data --------------------------------------------------------------------

rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
aus_shp <- st_read("Australia_shp/AUS_2021_AUST_GDA94.shp") # try both
#aus_shp <- ne_countries(scale = "large", country = "Australia", returnclass = "sf") # shapefile
catches  <- read_rds("Inputs/250730_SCP_complete.rds") #SCP data
dat <- read_rds("inputs/250827_step9.rds") # detection data
topo <- terra::rast("Inputs/5_AusBathyTopo_250m_2024.nc") # topography

dat1 <- dat %>% 
  filter(presence == 1) %>% #only real data shown
  filter(location == "Port Macquarie") %>%
  distinct(location, datetime, tag_id, station_name,
           longitude, latitude, movement)

SCP <- catches %>% 
  filter(area %in% c("Port Macquarie"))

# bounding box for maps ---------------------------------------------------

make_window <- function(df, buffer = 0.3) { #buffer is in degrees = approx. 30km
  lon_centre <- mean(df$longitude, na.rm = TRUE)
  lat_centre <- mean(df$latitude, na.rm = TRUE)
  list(xlim = c(lon_centre - buffer, lon_centre + buffer),
       ylim = c(lat_centre - buffer, lat_centre + buffer))
}

# make detections an SF object --------------------------------------------
datxy <- dat1 %>%
    group_by(location, latitude, longitude) %>%
    summarise(detections = n(), .groups = 'drop')

datxy_sf <- sf::st_as_sf(datxy, coords = c("longitude", "latitude"), #sf object
                           crs = 4326, agr = "constant")
  
# make catches an sf object -----------------------------------------------
  
datxy1 <- SCP %>% 
    group_by(location, latitude, longitude) %>% 
    summarise(captures = n(), .groups = 'drop')

datxy_sf1 <- sf::st_as_sf(datxy1, coords = c("longitude", "latitude"), #sf object
                         crs = 4326, agr = "constant")

bbox <- make_window(datxy1, buffer = 0.4) # bounding box

  
PM <- ggplot() +
    geom_sf(data = aus_shp, fill = "grey", colour = "black", alpha = 0.5) +
    geom_sf(data = datxy_sf, aes(size = detections), alpha = 1) +
    geom_sf(data = datxy_sf1, aes(size = captures), alpha = 0.8, colour = "firebrick") +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE) +
    labs(title = "Port Macquarie") +
    theme_bw() +
    annotation_scale(location = "br") +
    annotation_north_arrow(style = north_arrow_nautical, location = "tr") +
    theme(plot.title = element_text(hjust = 0.5))


ILL
SYD
HN
PM
CH
EH
BB

za <- ggarrange(BB, EH, CH, PM, HN, SYD, ILL, ncol = 2, nrow = 3)
za

ggsave(path = "outputs/Graphs/Final/detections", "250903_NSW_facet_maps.pdf",
       plot = za, width = 9, height = 9) #in inches because gg weird


ggpubr::ggexport(
  plotlist = list(BB, EH, CH, PM, HN, SYD, ILL),
  filename = "outputs/Graphs/Final/detections/250903_NSW_facet_maps.pdf",
  ncol = 2, nrow = 3, width = 7, height = 9)

ggpubr::ggexport(
  plotlist = list(ILL),
  filename = "outputs/Graphs/Final/detections/250903_ILL_facet_maps.pdf",
  width = 5, height = 5)
