#19.10.2023

rm(list=ls())
#bring and clean data environment
setwd("~/Documents/USC/Honours/R/data")
pacman::p_load(tidyverse, ggspatial, tidyterra, terra, sf, sp)
dat <- read_csv("Inputs/250728_det_enviro_complete.csv")
Aus <- st_read("Australia_shp/AUS_2021_AUST_GDA94.shp") # try both
IMOS <- read_csv("Inputs/250723_step2.csv") 

IMOS <- IMOS %>% 
  distinct(station_name, .keep_all = T)

dat <- dat[dat$presence != 0, ]

aus

# latitudional bands ------------------------------------------------------

# 1) One row per station with its coords and location label
stations <- IMOS %>%
  distinct(station_name, latitude, longitude)

# 2) Pick the "central" receiver in each location
#    (closest to the location's median latitude — adjust if you prefer mean/other rule)
central_recv <- stations %>%
  group_by(station_name) %>%
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
  summarise(num_det = n(), .groups = 'drop') %>% 
  ungroup()
  
dat1 <- dat1 %>% 
  mutate(location = ifelse(str_starts(location, "deg_"), "other", location))

dat2 <- dat1 %>% 
  mutate(location = fct_relevel(location,"Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                                "Gold Coast", "Ballina",
                                "Coffs Harbour", "Hawks Nest", "Sydney", "Illawarra"))

# shp map -----------------------------------------------------------------

m <- 
  ggplot() +
  geom_rect(data = bands, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "grey90", alpha = 0.3) + 
  geom_sf(data = Aus) +  # Shapefile layer
  geom_point(data = IMOS, aes(x = longitude, y = latitude), colour = "black", size = 0.5) +  # Points layer from dato in grey
  geom_point(data = dat2, aes(x = longitude, y = latitude, colour = location, size = num_det)) +  # Points layer
  scale_size_continuous(range = c(1, 4)) +
  theme_minimal() +
  coord_sf(xlim = c(149, 154), ylim = c(-37, -23.5)) +  # Zoom into specific lat-lon box
  annotation_scale(location = "bl") +
  annotation_north_arrow(style = north_arrow_nautical, location = "tl") + 
  scale_colour_scico_d(palette = "imola", direction = -1)

plot(m)

ggsave(path = "Outputs/Graphs/Final/detections", "250827_det_spatial_map.pdf",
       plot = m, width = 6, height = 8) #in inches because gg weird
