#9/11/23
  #making some spatial map examples
rm(list=ls())
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

setwd("~/University/2023/Honours/R/data")

Aus <- st_read("Australia_shp/Australia.shp") #Read in our data
cstack <- rast("IMOS/Currents/230912_cstack_12-22.tif")
sstack <- rast("IMOS/SST/GHRSST_12-22.tif")


# sst ---------------------------------------------------------------------

sst <- sstack[[19]] #the least cloudy day


sst <- aggregate(sst, fact = 4.5, 
                        fun = mean, na.rm = TRUE) 
plot(sst, col = viridis(255)) #a little bit more pixelated at 10km but covers more

sst_dat <- as.data.frame(sst, xy = T)
sst_dat <- na.omit(sst_dat)
head(sst_dat) #a beautifully curated df

sst_plot <- ggplot() +
  geom_raster(data = sst_dat, aes(x = x, y = y, fill = SST_20120119)) +
  scale_fill_viridis(name = "Sea surface 
temperature (⁰C)
(19-01-2012)", option = "d", direction = 1) +
  coord_fixed() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Latitude", y = "Longitude")

sst_plot

sst_plot1 <- sst_plot + 
  geom_sf(data = Aus, fill = "grey", colour = "black")  # Add borders of Aus
sst_plot1

sst_plot2 <- sst_plot1 +
  coord_sf(xlim = c(150, 155), ylim = c(-36, -24), expand = FALSE)
sst_plot2

ggsave(path = "Outputs/Graphs/Final/SST", "240220_SST_spatial_map.png",
       plot = sst_plot2, width = 5, height = 7) #in inches because gg weird
  
# vcur --------------------------------------------------------------------

head(names(cstack))

vcur <- cstack[[3]]
vcur_dat <- as.data.frame(vcur, xy = T)
vcur_dat <- na.omit(vcur_dat)
head(vcur_dat)

vcur_plot <- ggplot() +
  geom_raster(data = vcur_dat, aes(x = x, y = y, fill = VCUR_20120101)) +
  scale_fill_viridis(name = "North - south
current direction
(01-01-2012)", option = "d", direction = 1) +
  coord_fixed() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Latitude", y = "Longitude")

vcur_plot

vcur_plot1 <- vcur_plot + 
  geom_sf(data = Aus, fill = "grey", colour = "black")  # Add borders of Aus
vcur_plot1

vcur_plot2 <- vcur_plot1 +
  coord_sf(xlim = c(150, 155), ylim = c(-36, -24), expand = FALSE)
vcur_plot2

ggsave(path = "Outputs/Graphs/Polishing/Currents/VCUR", "231109_VCUR_spatial_map.png",
       plot = vcur_plot2, width = 5, height = 7) #in inches because gg weird


# GSLA --------------------------------------------------------------------

gsla <- cstack[[1]]
gsla_dat <- as.data.frame(gsla, xy = T)
gsla_dat <- na.omit(gsla_dat)
head(gsla_dat)

gsla_plot <- ggplot() +
  geom_raster(data = gsla_dat, aes(x = x, y = y, fill = GSLA_20120101)) +
  scale_fill_viridis(name = "Global sea
level anomaly
(01-01-2012)", option = "d", direction = 1) +
  coord_fixed() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Latitude", y = "Longitude",)

gsla_plot

gsla_plot1 <- gsla_plot + 
  geom_sf(data = Aus, fill = "grey", colour = "black")  # Add borders of Aus
gsla_plot1

gsla_plot2 <- gsla_plot1 +
  coord_sf(xlim = c(150, 155), ylim = c(-36, -24), expand = FALSE)
gsla_plot2

ggsave(path = "Outputs/Graphs/Polishing/Currents/GSLA", "231109_gsla_spatial_map.png",
       plot = gsla_plot2, width = 5, height = 7) #in inches because gg weird



sstack <- rast("IMOS/SST/GHRSST_12-22.tif")

head(names(sstack))
plot(sstack)
