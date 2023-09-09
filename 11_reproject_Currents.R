#07.09.23
  #automating the workflow to crop, stack and reproject our data
    #for current data

rm(list=ls())

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# combine all stacks  -----------------------------------------------------

setwd("~/University/2023/Honours/R/data/IMOS/Currents")
list.files()

 # Generate file names for the years
file_names <- paste0("Currents_stack_", 2012:2022, ".tif")

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
rstack <- do.call(c, rasters_list)


# Check the CRS of the combined stack
crs(rstack)

plot(rstack[[3]], col = viridis(255))
head(names(rstack))
tail(names(rstack))

# Save the combined stack
writeRaster(rstack, filename = "Currents_12-22.tif", overwrite = T)


