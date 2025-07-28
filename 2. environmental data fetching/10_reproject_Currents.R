#07.09.23
  #automating the workflow to crop, stack and reproject our data
    #for current data

rm(list=ls())

# Packages ----------------------------------------------------------------

pacman::p_load("tidyverse", "ncdf4", 'purrr', 'furrr','future', 'terra', 'sf', 'sp', 'viridis')

# combine all stacks  -----------------------------------------------------

setwd("/Volumes/LaCie_PF/Currents")
list.files()

# Generate file names for the years
file_names <- paste0("Currents_stack_", 2012:2024, ".tif")

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

plot(rstack[[3]], col = viridis(255))
head(names(rstack))
tail(names(rstack))


# rm duplicates -----------------------------------------------------------

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

#I had a 60 duplicates, this was due to downloading data multiple times for certain layers / days

# save --------------------------------------------------------------------

# Save the combined stack
writeRaster(rstack1, filename = "250728_cstack_12-24.tif", overwrite = T)



