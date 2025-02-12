# 11 September 2023
  # P. Fuenzalida
    # extracting currrent data

rm(list=ls())

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

cstack <- rast("IMOS/Currents/230912_cstack_12-22.tif") #currents stack
rcs <- read_csv("Inputs/250211_xy_coordinates.csv") #xy points
WGS84 <- crs("EPSG:4326")# Coordinate reference systems

pts.WGS <- st_as_sf(rcs, coords = c("longitude", #convert to an SF object
                                   "latitude")) 

st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS

# plotting ----------------------------------------------------------------

plot(cstack[[3]], col = viridis(255))
plot(pts.WGS, add = T)
 
# extract -----------------------------------------------------------------

cur.pts <- extract(cstack, pts.WGS, ID = F) # ID = FALSE  

sum(is.na(cur.pts))
18 * 12045
(84374 / 216810) * 100 

# nearest temporal neighbour ----------------------------------------------

cur.pts1 <- fill1dneighbour(cur.pts)
sum(is.na(cur.pts1)) 

(84315 / 216810) * 100 

# 5 d mean filling --------------------------------------------------------

cur.pts2 <- t(apply(cur.pts1, 1, mean_5d))
cur.pts2 <- as.data.frame(cur.pts2)
colnames(cur.pts2) <- colnames(cur.pts1)
sum(is.na(cur.pts2))

#all stations that hold data are filled, stations that fall too close to the coast hold only NAs


# bilinear interpolation --------------------------------------------------

# Extract using bilinear extractione
bl <- extract(cstack, pts.WGS, method = "bilinear")
#bilinear returns values that are interpolated from the four nearest cells

bl <- bl[,-1] #remove ID row

sum(is.na(bl)) 

(24098 / 216810) * 100
#11% is NA with bilinear interpolation

# fill with bilinear interpolation ----------------------------------------

cur.pts3 <- fill_vals(cur.pts2, bl)
sum(is.na(cur.pts3))

38 - 11
#27% bilinear interpolation

# add location row --------------------------------------------------------

rcs <-  rcs %>% mutate(RowNumber = row_number()) #make a row number 
cur.pts3 <-  cur.pts3 %>% mutate(RowNumber = row_number()) #make a row number 


cur.pts4 <- left_join(cur.pts3, rcs 
                      %>% dplyr::select(RowNumber, location), by = "RowNumber")

cur.pts5 <- cur.pts4 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(location, everything())


# save --------------------------------------------------------------------

write_csv(cur.pts3, file = "Inputs/250212_Currents_vals_12-22.csv")
