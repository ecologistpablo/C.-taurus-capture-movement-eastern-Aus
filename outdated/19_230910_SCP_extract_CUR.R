#10.09.23
  #paired SCP data
    #extract CUR vals for it

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

pts <- read_csv("shark control/230910_XY_captures_12-22.csv")
WGS84 <- crs("EPSG:4326")# Coordinate reference systems
rstack <- rast("IMOS/Currents/230911_cstack_12-22.tif")

head(pts) #its all there
rstack #nice


pts1 <- pts %>% 
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

str(pts1)


pts.WGS <- st_as_sf(pts1, coords = c("Longitude", #convert to an SF object
                                    "Latitude")) 

st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS

# plot --------------------------------------------------------------------

# Calculate the number of detections at each station
ptsxy <- pts %>%
  group_by( Latitude, Longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

ptsxy_sf <- sf::st_as_sf(ptsxy, coords = c("Longitude", "Latitude"),
                         crs= 4326, agr = "constant")

mapview::mapview(ptsxy_sf, cex = "num_det", fbg = F)

#they're plotting well, everything looks to be in order

# extract -----------------------------------------------------------------

cur.pts <- extract(rstack, pts.WGS, ID = F)
# ID = FALSE otherwise it creates a column with a number for each spoint

sum(is.na(cur.pts))
(1036208 / 1577895) * 100
#65 % NA

# bilinear extraction -----------------------------------------------------

bl <- extract(rstack, pts.WGS, method = "bilinear") # ID = FALSE otherwise it creates a column with a number for each spoint

bl2 <- bl %>% 
  dplyr::select(-ID)

sum(is.na(bl2))
(12080 / 1866975) * 100

#currents can be sampled through clouds so no need to time travel for dat

# fill values from bilinear interpolation into our pts --------------------


#fill vals of bilinear interpolation into our data
cur.pts1 <- fill_vals(cur.pts, bl2)
#this does take a few mins

sum(is.na(cur.pts1)) 

(12080 / 1866975) * 100
#0.64%
# 58% was filled with bilinear extraction

# add station name --------------------------------------------------------

pts <-  pts %>% mutate(RowNumber = row_number()) #make a row number 
cur.pts1 <-  cur.pts1 %>% mutate(RowNumber = row_number()) #make a row number 

cur.pts2 <- left_join(cur.pts1, pts %>% dplyr::select(RowNumber, Location), by = "RowNumber")

#re-order it
cur.pts2 <- cur.pts2 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(Location, everything())

head(names(cur.pts2))

# save --------------------------------------------------------------------

write_csv(cur.pts2, file = "Inputs/230912_capture_CUR.csv")

