#28.08.23
  #combine our stacks, and reproject them

# we will now combine all years into one file, and reproject them
# because the earth is round and our data is not accounting for the curvature of the earth yet
# soz flat-earthers, ya wrong

rm(list=ls())

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# combine all stacks  -----------------------------------------------------

setwd("/Volumes/LaCie_PF/SST/")
list.files()

# Generate file names for the years
file_names <- paste0("SST_stack_", 2012:2024, ".tif")

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
rstack <- do.call(c, rasters_list)


# Check the CRS of the combined stack
crs(rstack)

plot(rstack[[1]], col = viridis(255))
head(names(rstack))
tail(names(rstack))

# nice

# rm duplicates -----------------------------------------------------------

#just as a precaution to ensure we didn't download multiple layers / days
#we shall rm anything with the same name

removeDuplicateLayers <- function(raster_stack) {
  layer_names <- names(raster_stack)
  unique_names <- unique(layer_names)
  
  # Identify the index of the unique layers based on their names
  unique_layer_indices <- match(unique_names, layer_names)
  
  # Subset the raster stack to include only the unique layers
  return(raster_stack[[unique_layer_indices]])
}

# Apply the function
rstack1 <- removeDuplicateLayers(rstack)

print(paste("Original number of layers: ", length(names(rstack))))
print(paste("Number of layers after removing duplicates: ", length(names(rstack1)))) 

#we have 0 duplicates, good 

# save --------------------------------------------------------------------

# Save the combined stack
writeRaster(rstack, filename = "GHRSST_12-24.tif", overwrite = T)
