# 04 September 2023
# Who needs remora anyway
# Interpolating NAs in SST data

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")
sst.pts2 <- read_csv("Inputs/230909_SST_vals_12-22_pts2.csv")
rcs <- read_csv("Inputs/230909_XY_receivers.csv")
WGS84 <- crs("EPSG:4326")# Coordinate reference systems
rstack <- rast("IMOS/SST/GHRSST_12-22.tif")

head(rcs) #its all there

pts.sp <- st_as_sf(rcs, coords = c("receiver_deployment_longitude", #convert to an SF object
                                   "receiver_deployment_latitude")) 

st_crs(pts.sp) <- crs(WGS84) #remember to assign crs 
pts.sp

# bilinear extrapolation --------------------------------------------------

bl <- extract(rstack, pts.sp, method = "bilinear")

rcs <-  rcs %>% mutate(RowNumber = row_number()) #make a row number 
bl <-  bl %>% mutate(RowNumber = row_number()) #make a row number 

bl1 <- left_join(bl, rcs %>% dplyr::select(RowNumber, station_name), by = "RowNumber")

#re-order it
bl1 <- bl1 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(station_name, everything())

bl2 <- bl1 %>% 
  dplyr::select(-ID, -station_name)

# nearest temporal neighbour ----------------------------------------------

bl3 <- as.data.frame(bl2)

sum(is.na(bl3))

114*3871
(271482 / 441294) * 100
# 61% NA

# 5 d mean ----------------------------------------------------------------


# Apply the function to each row and save the new values in sst.pts2
bl4 <- t(apply(bl3, 1, mean_5d))

bl4 <- as.data.frame(bl4)
colnames(bl4) <- colnames(bl2)

sum(is.na(bl3)) - sum(is.na(bl4))  #115198 were filled

sum(is.na(bl4)) 
(135497 / 441294) * 100
#30%

write_csv(bl4, file = "Inputs/230911_SST_bl_vals_12-22.csv")
