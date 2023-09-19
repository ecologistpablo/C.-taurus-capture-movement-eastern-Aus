#19.09.23
  #find dist_to_EAC

rm(list=ls())

# load current stack ------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

cur_stack <- rast("IMOS/Currents/230912_cstack_12-22.tif")


# load SST ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# combine all stacks  -----------------------------------------------------

setwd("~/University/2023/Honours/R/data/IMOS/SST")
list.files()

# Generate file names for the years
file_names <- paste0("SST_stack_", 2012:2022, ".tif")

# Coordinate reference systems
WGS84 <- crs("EPSG:4326")

# Initialize an empty list to store the rasters
rasters_list <- list()

# Loop through each file name
for (file_name in file_names) {
  if (file.exists(file_name)) {  # Check if the file exists
    r <- rast(file_name) # Read in the raster stack
    crs(r) <- WGS84 # Assign CRS
    rasters_list[[length(rasters_list) + 1]] <- r # Append to the list
  }
}

# Combine the individual rasters into one stack
sst_stack <- do.call(c, rasters_list)


# Check the CRS of the combined stack
sst_stack

#resample
sst_stack1 <- resample(sst_stack, cur_stack)

plot(sst_stack1[[19]])
plot(cur_stack[[3]])

writeRaster(sst_stack, filename = "GHRSST_12-22.tif", overwrite = T)
writeRaster(sst_stack1, filename = "GHRSST_12-22_0.2.tif", overwrite = T)

# combine stacks ----------------------------------------------------------



# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

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

write_csv(df_sst, file = "SST_stack_gradient_230919.csv")
write_csv(df_cur, file = "CUR_stack_gradient_230919.csv")

# Combine these into a single data frame
df_all <- data.frame(df_sst, df_cur)

# Run the PCA
pca_result <- prcomp(df_all[, 3:ncol(df_all)], center = TRUE, scale. = TRUE)

# Extract the first principal component (PC1)
pc1 <- pca_result$x[,1]

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


