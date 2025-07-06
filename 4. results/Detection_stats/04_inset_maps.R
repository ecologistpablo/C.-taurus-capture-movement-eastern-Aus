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
topo <- terra::rast("Inputs/5_AusBathyTopo_250m_2024.nc")


buffer_deg <- 0.3 # how far a box do you want around your data ? 
dat <- dat %>% 
  filter(presence == 1) #only real data shown

terra::plot(topo, main = "Topography / Bathymetry")
topo[topo > 0] <- NA # remove terrestrial topography

wolf     <- dat %>% filter(location == "Wolf Rock")
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

plot_location_map <- function(df, location_name, aus_shp, topo, buffer = 0.3,
                              north_loc = "tr", title = location_name) {
  # Summarise detections
  datxy <- df %>%
    group_by(location, latitude, longitude, station_name) %>%
    summarise(Detections = n(), .groups = 'drop')
  
  datxy_sf <- sf::st_as_sf(datxy, coords = c("longitude", "latitude"), #sf object
                           crs = 4326, agr = "constant")
  
  bbox <- make_window(df, buffer = buffer) # bounding box
  
  # Convert bbox to spat extent and sf bbox
  bbox_ext <- terra::ext(bbox$xlim[1], bbox$xlim[2], bbox$ylim[1], bbox$ylim[2])
  bbox_sf <- sf::st_as_sfc(sf::st_bbox(c(xmin = bbox$xlim[1], xmax = bbox$xlim[2],
                                         ymin = bbox$ylim[1], ymax = bbox$ylim[2]),
                                       crs = sf::st_crs(4326)))
  
  topo_crop <- terra::crop(topo, bbox_ext) # crop 
  aus_crop <- sf::st_crop(aus_shp, bbox_sf) # crop
  
  topo_df <- as.data.frame(topo_crop, xy = TRUE, na.rm = TRUE) # convert to df
  names(topo_df)[3] <- "bathymetry" # add name to data
  
  # Final plot
  ggplot() +
    geom_raster(data = topo_df, aes(x = x, y = y, fill = bathymetry)) +
    scale_fill_viridis_c(option = "B", na.value = NA, name = "Depth (m)") +
    geom_sf(data = aus_crop, fill = "grey", colour = "black") +
    geom_sf(data = datxy_sf, aes(size = Detections), alpha = 0.5) +
    coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE) +
    labs(title = title) +
    theme_bw() +
    annotation_scale(location = "br") +
    annotation_north_arrow(style = north_arrow_nautical, location = north_loc) +
    theme(plot.title = element_text(hjust = 0.5))
}


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