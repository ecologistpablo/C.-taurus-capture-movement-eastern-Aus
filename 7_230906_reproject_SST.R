#28.08.23
#automating the workflow to crop, stack and reproject our data
rm(list=ls())

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# combine all stacks  -----------------------------------------------------

setwd("~/University/2023/Honours/R/data/IMOS/SST")
list.files()

# Generate file names for the years
file_names <- paste0("SST_stack_", 2012:2022, ".tif")

# Coordinate reference systems
UTM56S <- crs("EPSG:32756")

# Initialize an empty list to store the rasters
rasters_list <- list()

# Loop through each file name
for (file_name in file_names) {
  if (file.exists(file_name)) {  # Check if the file exists
    r <- rast(file_name) # Read in the raster stack
    crs(r) <- UTM56S # Assign CRS
    rasters_list[[length(rasters_list) + 1]] <- r # Append to the list
  }
}

# Combine the individual rasters into one stack
SST_stack <- do.call(c, rasters_list)


# Check the CRS of the combined stack
crs(SST_stack)

plot(SST_stack, col = viridis(255))
head(names(SST_stack))
tail(names(SST_stack))

# Save the combined stack
writeRaster(SST_stack, "GHRSSTp_12-22.tif")
