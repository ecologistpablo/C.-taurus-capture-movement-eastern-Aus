#19.10.2023

rm(list=ls())
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#bring and clean data environment
setwd("~/University/2023/Honours/R/data")
dato <- read_csv("Inputs/230906_step2.csv")

dat <- read_csv("Inputs/250212_det_enviro_complete.csv")

dat <- dat[dat$presence != 0, ]

# Aus shpfle --------------------------------------------------------------

Aus <- st_read("Australia_shp/Australia.shp") #Read in our data

Au <- Aus %>% 
  filter(ADMIN_NAME %in% c("Queensland", "New South Wales", "Victoria"))
plot(Au)


# latitudional bands ------------------------------------------------------

key_locations <- dat %>%
  filter(location %in% c("Wolf Rock", "Flat Rock", "Coffs Harbour", "Hawks Nest", "Sydney")) %>%
  distinct(location, latitude, longitude)  # Ensure unique lat/lon pairs

key_locations

# Create a dataframe for rectangular bands, extending 5 km north and south
bands <- key_locations %>%
  mutate(ymin = latitude - 0.045,  # 5km south
         ymax = latitude + 0.045,  # 5km north
         xmin = 149,  # Use the map's x-axis limits
         xmax = 155)  # Adjust if necessary

# munging -----------------------------------------------------------------

dat1 <- dat %>% 
  group_by(location, longitude, latitude) %>% 
  summarise(num_det = n(), .groups = 'drop') %>% 
  ungroup()
  
#dat1 <- dat1 %>% 
#mutate(Location = ifelse(str_starts(Location, "deg_"), "other", Location))

dat1 <- dat1 %>% 
  mutate(location = fct_relevel(location,"Wolf Rock", "Flat Rock", "Coffs Harbour", 
                            "Hawks Nest", "Sydney"))


dat2 <- dat1 %>% 
  filter(location  %in% c("Wolf Rock", "Flat Rock", "Coffs Harbour", 
                         "Hawks Nest", "Sydney"))

# shp map -----------------------------------------------------------------


colour_palette <- c("firebrick", "goldenrod", "aquamarine3", "lightskyblue", "plum" )

m <-
ggplot() +
  geom_rect(data = bands, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "grey0", alpha = 0.3) + 
  geom_sf(data = Au) +  # Shapefile layer
  geom_point(data = dato, aes(x = receiver_deployment_longitude, y = receiver_deployment_latitude), colour = "black", size = 1) +  # Points layer from dato in grey
  geom_point(data = dat2, aes(x = longitude, y = latitude, colour = location, size = num_det)) +  # Points layer
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  scale_colour_manual(values = colour_palette) +   
  coord_sf(xlim = c(149, 155), ylim = c(-37, -25)) +  # Zoom into specific lat-lon box
  annotation_scale(location = "bl") +
  annotation_north_arrow(style = north_arrow_nautical, location = "tl")

plot(m)

ggsave(path = "Outputs/Graphs/Final/detection", "250212_det_spatial_map.pdf",
       plot = m, width = 6, height = 8) #in inches because gg weird
