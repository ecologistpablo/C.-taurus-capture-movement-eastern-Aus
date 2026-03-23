#28.08.23
  #combine our stacks, and reproject them

# we will now combine all years into one file, and reproject them
# because the earth is round and our data is not accounting for the curvature of the earth yet
# soz flat-earthers, ya wrong

rm(list=ls())

# Packages ----------------------------------------------------------------

pacman::p_load('tidyverse', 'purrr', 'furrr', 'terra', 'viridis')

# combine all stacks  -----------------------------------------------------
setwd("~/Documents/USC/Honours/R/data/IMOS/SST") #my working directory, change to yours"/")
setwd("/Volumes/LaCie_PF/IMOS/SST")

list.files()

# Generate file names for the years
file_names <- paste0("SST_stack_", 2012:2025, ".tif")
#file_names <- paste0("GHRSST_12-24.tif", "SST_stack_2025.tif") # doing it manually

# Coordinate reference systems
WGS84 <- terra::crs("EPSG:4326")

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
writeRaster(rstack, filename = "GHRSST_12-25.tif", overwrite = T)


# 2025 data add -----------------------------------------------------------

setwd("~/Documents/USC/Honours/R/data/IMOS/SST/update") #my working directory, change to yours"/")
list.files()
# Generate file names for the years

# Load the two stacks directly
rstack_existing <- rast("GHRSST_12-24.tif")
crs(rstack_existing) <- WGS84

r_2025 <- rast("SST_stack_2025.tif")
crs(r_2025) <- WGS84

# Reproject 2025 to match existing stack
r_2025_reproj <- project(r_2025, rstack_existing)

# Combine
rstack_new <- c(rstack_existing, r_2025_reproj)
print(paste("Total layers:", nlyr(rstack_new)))

# Remove duplicates
removeDuplicateLayers <- function(raster_stack) {
  layer_names <- names(raster_stack)
  unique_names <- unique(layer_names)
  unique_layer_indices <- match(unique_names, layer_names)
  return(raster_stack[[unique_layer_indices]])
}

rstack_final <- removeDuplicateLayers(rstack_new)
print(paste("After removing duplicates:", nlyr(rstack_final)))

# Save
writeRaster(rstack_final, filename = "GHRSST_12-25.tif", overwrite = TRUE)
# nice