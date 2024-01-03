# 11 September 2023
  # P. Fuenzalida
    # extracting currrent data

rm(list=ls())

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

cstack <- rast("IMOS/Currents/230912_cstack_12-22.tif") #currents stack
rcs <- read_csv("Inputs/230909_XY_receivers.csv") #xy points
WGS84 <- crs("EPSG:4326")# Coordinate reference systems

pts.WGS <- st_as_sf(rcs, coords = c("receiver_deployment_longitude", #convert to an SF object
                                   "receiver_deployment_latitude")) 

st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS

# plotting ----------------------------------------------------------------

plot(cstack[[3]], col = viridis(255))
plot(pts.WGS, add = T)
 
# extract -----------------------------------------------------------------

cur.pts <- extract(cstack, pts.WGS, ID = F) # ID = FALSE  

sum(is.na(cur.pts))
114 * 12045
(265480 / 1373130) * 100 
#19.33% is NA, thats pretty good

# nearest temporal neighbour ----------------------------------------------

cur.pts1 <- fill1dneighbour(cur.pts)
sum(is.na(cur.pts1)) 

(264990 / 1373130) * 100 
#still 19.29, only a little


# 5 d mean filling --------------------------------------------------------

cur.pts2 <- t(apply(cur.pts1, 1, mean_5d))
cur.pts2 <- as.data.frame(cur.pts2)
colnames(cur.pts2) <- colnames(cur.pts1)
sum(is.na(cur.pts2))

#all stations that hold data are filled, stations that fall too close to the coast hold only NAs

# add station_name --------------------------------------------------------


# bilinear interpolation --------------------------------------------------

# Extract using bilinear extractione
bl <- extract(cstack, pts.WGS, method = "bilinear")
#bilinear returns values that are interpolated from the four nearest cells

bl1 <-  bl %>% rename(station_name = ID) #make a row number 

sum(is.na(bl1)) 

(60245 / 1373130) * 100
#4.3% is NA with bilinear interpolation

# fill with bilinear interpolation ----------------------------------------

cur.pts3 <- fill_vals(cur.pts2, bl1)
sum(is.na(cur.pts3))

(60229 / 1373130) * 100 
#from 19.29% to 4.38% :o
4.38 - 19.29

#13.47% of our data was filled used bilinear interpolation


# save --------------------------------------------------------------------

write_csv(cur.pts3, file = "Inputs/230912_Currents_vals_12-22.csv")
