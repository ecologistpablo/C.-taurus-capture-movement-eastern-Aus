#19.09.23
  #find dist_to_EAC

rm(list=ls())


# load libraries and data -------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("~/University/2023/Honours/R/data")

#load stacks
cur_stack <- rast("IMOS/Currents/230912_cstack_12-22.tif")
sst_stack <- rast("IMOS/SST/GHRSST_12-22_0.2.tif") 

#pts
rcs <- read_csv("Inputs/230909_XY_receivers.csv")
WGS84 <- crs("EPSG:4326")# Coordinate reference systems

pts.WGS <- st_as_sf(rcs, coords = c("receiver_deployment_longitude", #convert to an SF object
                                    "receiver_deployment_latitude")) 

st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS

# gradients ---------------------------------------------------------------

# Calculate gradients for SST
grad_sst <- terra::focal(sst_stack, w=matrix(1,3,3), fun=function(x) diff(range(x)))

# Calculate gradients for GSLA, UCUR, and VCUR
grad_cur <- terra::focal(cur_stack, w=matrix(1,3,3), fun=function(x) diff(range(x)))


# convert to vectore ------------------------------------------------------

# Flatten rasters into data frames for PCA
df_sst <- as.data.frame(values(grad_sst), xy=TRUE)
df_cur <- as.data.frame(values(grad_cur), xy=TRUE)

#save 
write_csv(df_sst, file = "Inputs/SST_stack_gradient_230919.csv")
write_csv(df_cur, file = "CUR_stack_gradient_230919.csv")
#load
sst <- read_csv("Inputs/SST_stack_gradient_230919.csv")
cur <- read_csv("Inputs/CUR_stack_gradient_230919.csv")

# Combine these into a single data frame
alldat <- data.frame(sst, cur)

# Run the PCA
pca <- prcomp(alldat[, 3:ncol(alldat)], center = TRUE, scale. = TRUE)

# Extract the first principal component (PC1)
pc1 <- pca$x[,1]

# calculate distance ------------------------------------------------------

# Initialize an empty list to store results
distance_list <- list()

# Loop through each unique date
for(date in unique_dates) {
  
  # Extract PC1 for the current date
  pc1_current <- pc1[[date]]
  
  # Convert PC1 to a SpatRaster object again
  pc1_raster <- rast(pc1_current)  # You'll need to make sure the dimensions match the original raster
  
  # Find max value of PC1 at the latitude corresponding to each point
  max_pc1_lat <- apply(st_coordinates(pts.WGS), 1, function(coord) {
    lat <- coord[2]
    # Extract values along the latitude and find the max PC1
    lat_values <- extract(pc1_raster, y=lat)
    return(max(lat_values, na.rm = TRUE))
  })
  
  # Calculate distance to max PC1 at the corresponding latitude
  distance_to_max_pc1 <- st_distance(pts.WGS, max_pc1_lat)
  
  # Store the result
  distance_list[[date]] <- distance_to_max_pc1
}

# Combine the results into a single data frame or other suitable object


