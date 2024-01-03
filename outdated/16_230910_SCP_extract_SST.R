#10.09.23
  #paired SCP data
    #extract SST vals for it

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

pts <- read_csv("~/University/2023/Honours/R/data/shark control/231009_SCP_captures.csv")
WGS84 <- crs("EPSG:4326")# Coordinate reference systems

head(pts) #its all there

pts1 <- pts %>% 
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

str(pts1)
         
         
pts.WGS <- st_as_sf(pts1, coords = c("Longitude", #convert to an SF object
                                   "Latitude")) 

st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS


# SST stack ---------------------------------------------------------------

SST <- rast("IMOS/SST/GHRSST_12-22.tif")

# plot --------------------------------------------------------------------

# Calculate the number of detections at each station
ptsxy <- pts %>%
  group_by(Latitude, Longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

ptsxy_sf <- sf::st_as_sf(ptsxy, coords = c("Longitude", "Latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(ptsxy_sf, cex = "num_det", fbg = F)

#they're plotting well, but a few rows should maybe be removed 

# extract -----------------------------------------------------------------

sst.pts <- extract(SST, pts.WGS, ID = F)
# ID = FALSE otherwise it creates a column with a number for each point

sum(is.na(sst.pts))
307*3871
(1186617 / 1188397) * 100

#99.85% NA :(

sst.pts1 <- fill1dneighbour(sst.pts)

sst.pts2 <- mean_5d(sst.pts1)

sum(is.na(sst.pts2))
(1184526 / 1188397) * 100
#99.67%

# Bilinear interpolation --------------------------------------------------

bl <- extract(SST, pts.WGS, method = "bilinear") 

bl2 <- bl %>% 
  dplyr::select(-ID)

sum(is.na(bl2))
307 * 3871

(1158747 / 1188397) * 100
#97% NA, we still have almost nothing

# nearest temporal neighbour ----------------------------------------------

bl3 <- fill1dneighbour(bl2)

bl4 <- mean_5d(bl3)

#fill bilinear interpolation of 2km grid squares into our straight extraction df
sst.pts3 <- fill_vals(sst.pts2, bl4)
sum(is.na(sst.pts3))
(1126475 / 1188397) * 100
#94% as NA

# resize coarseness -------------------------------------------------------

SST #res at 0.02 by 0.02, 2km x 2km

#0.02 = appox 2km, 0.0898 = approx 10km

agg_factor <- 5 #0.02 x 5 = 10km

# Resample the raster to the new resolution
SST10km <- aggregate(SST, fact = agg_factor, #factor multiple original resolution
                        fun = mean, #mean 
                        na.rm = TRUE) #resize into land not sea = TRUE
SST10km
#this is a 10km resize

plot(SST10km, col = viridis(255))

# resample ----------------------------------------------------------------

#direct resampling
sst.pts10km <- extract(SST10km, pts.WGS, ID = F) 

sum(is.na(sst.pts10km)) 
(609472 / 1188397) * 100
#51% NA

# nearest temporal neighbour ----------------------------------------------

sst.pts10km1 <- fill1dneighbour(sst.pts10km)

#5 d mean
sst.pts10km2 <- mean_5d(sst.pts10km1)

#return column names
colnames(sst.pts10km2) <- colnames(sst.pts10km1)

sum(is.na(sst.pts10km2))
(135500 / 1188397) * 100
#still 11%

#fill values of 10km res into our 2km res
sst.pts2 <- fill_vals(sst.pts1, sst.pts10km2)

sum(is.na(sst.pts2))

(54291 / 507101) * 100
#10% NA still. 


# bilinear interpolation of 10km grid -------------------------------------

bl10 <- extract(SST10km, pts.WGS, method = "bilinear") # ID = FALSE otherwise it creates a column with a number for each spoint

pts <-  pts %>% mutate(RowNumber = row_number()) #make a row number 
bl10 <-  bl10 %>% mutate(RowNumber = row_number()) #make a row number 

bl10a <- left_join(bl10, pts %>% dplyr::select(RowNumber, Location), by = "RowNumber")

#re-order it
bl10b <- bl10a %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(Location, everything())

bl10c <- bl10b %>% 
  dplyr::select(-ID, -Location)

# nearest temporal neighbour ----------------------------------------------

bl10d <- fill1dneighbour(bl10c)

sum(is.na(bl10d))

283*3871
(19534 / 1095493) * 100
# 1.78% :D

# 5 d mean ----------------------------------------------------------------

bl10e <- mean_5d(bl10d)

colnames(bl10e) <- colnames(bl10c)


colnames(bl10e)
# fill_gaps of bli into sst.pts -------------------------------------------

#fill values of bilinear interpolation at 10km into our df
sst.pts3 <- fill_vals(sst.pts2, bl10e)

sum(is.na(sst.pts3))
(3925 / 507101) * 100
#0.77 % :)

# add station name --------------------------------------------------------

sst.pts4 <-  sst.pts3 %>% mutate(RowNumber = row_number()) #make a row number 
pts <- pts %>% mutate(RowNumber = row_number()) #make a row number 

sst.pts4 <- left_join(sst.pts4, pts %>% dplyr::select(RowNumber, Location), by = "RowNumber")

#re-order it
sst.pts4 <- sst.pts4 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(Location, everything())

head(names(sst.pts4))

# save --------------------------------------------------------------------

write_csv(sst.pts4, file = "Inputs/230912_capture_SST.csv")

