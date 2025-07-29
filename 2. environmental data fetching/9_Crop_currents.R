#28.08.23
  #automating the workflow to crop, stack and reproject our data
    #For currents


# Packages ----------------------------------------------------------------

pacman::p_load("tidyverse", "ncdf4", 'purrr', 'furrr','future', 'terra', 'sf', 'sp', 'viridis')

# metadata finding --------------------------------------------------------
rm(list=ls())
setwd("/Volumes/LaCie_PF/Currents/2024")
list.files()
nc <- nc_open("IMOS_OceanCurrent_HV_20231231T000000Z_GSLA_FV02_NRT.nc")

print(nc)
# old fashioned way: 2012 -------------------------------------------------

plan(multisession, workers = 6) # Use 6 cores

# List all the .nc files in the directory
file_list <- list.files(pattern = "\\.nc$", full.names = TRUE)

# Read each NetCDF file as a SpatRaster
r_list <- lapply(file_list, rast)

#stack them
rstack <- rast(c(r_list))

# select UCUR, VCUR means and GSLA ----------------------------------------

subset_selected_layers <- function(rstack) {
  # Create a regular expression pattern to match the layer names you want to keep
  pattern <- "^(UCUR_TIME|VCUR_TIME|GSLA_TIME)"
  
  # Use grep to find the indices of layers that match the pattern
  selected_indices <- grep(pattern, names(rstack))
  
  # Subset the SpatRaster to keep only those layers
  rstack_subset <- subset(rstack, selected_indices)
  
  return(rstack_subset)
}

# Run
rstack1 <- subset_selected_layers(rstack)

names(rstack1)

plot(rstack1, col = viridis(255))

#nice

# renaming ----------------------------------------------------------------

head(names(rstack1))

# Extract the date part (YYYYMMDD) from each .nc file name
date_str <- substr(file_list, 24, 31) #if you're download different data, count where the filename is that you want

head(date_str) #check

# Check if the lengths match
if ((length(date_str) * 3) != nlyr(rstack1)) {
  stop("The number of dates does not match the number of layers in the SpatRaster.")
}

# Initialize an empty vector to hold the new names
new_names <- vector("character", length = nlyr(rstack1))

# Loop through each date to rename the corresponding layers
for (i in seq_along(date_str)) {
  
  # Calculate the index positions for the three layers corresponding to this date
  idx_GSLA <- (i - 1) * 3 + 1
  idx_UCUR_MEAN <- (i - 1) * 3 + 2
  idx_VCUR_MEAN <- (i - 1) * 3 + 3
  
  # Rename the layers
  new_names[idx_GSLA] <- paste0("GSLA_", date_str[i]) #so it will come out as GSLA_20120101 for 01 Jan 2012
  new_names[idx_UCUR_MEAN] <- paste0("UCUR_", date_str[i])
  new_names[idx_VCUR_MEAN] <- paste0("VCUR_", date_str[i])
}

# Apply the new names to the SpatRaster
names(rstack1) <- new_names

# Check the first few names to make sure they were renamed correctly
head(names(rstack1))

plot(rstack1, col = viridis(255))

# crop --------------------------------------------------------------------

# Define the extent
e <- ext(c(150, 155, -36, -24)) #xmin, xmax, ymin, ymax for your studysite

# Crop the stack
rstack2 <- terra::crop(rstack1, e) # cropping takes awhile, be patient

plot(rstack2, col = viridis(255))

names(rstack2)

# save progress -----------------------------------------------------------

writeRaster(rstack2, "Current_stack_2023.tif")

setwd("~/University/2023/Honours/R/data/IMOS/Currents")
writeRaster(rstack2, "Currents_stack_2012.tif")

# looped ------------------------------------------------------------------

# Define a function to process a specific year
process_year <- function(year) {
  
  setwd(paste0("/Volumes/LaCie_PF/Currents/", year))  # Set working directory to the year's folder
  
  plan(multisession, workers = 6) # Use 6 cores
  
  # List all the .nc files in the directory
  file_list <- list.files(pattern = "\\.nc$", full.names = TRUE)
  
  # Read each NetCDF file as a SpatRaster
  r_list <- lapply(file_list, rast)
  
  # Stack the rasters
  rstack <- rast(c(r_list))
  
  # Subset and keep only layers named UCUR_TIME, VCUR_TIME, and GSLA_TIME
  rstack1 <- subset(rstack, grep("^(UCUR_TIME|VCUR_TIME|GSLA_TIME)", names(rstack)))
  
  # Extract the date part (YYYYMMDD) from each .nc file name
  date_str <- substr(file_list, 24, 31)
  
  # Check if the lengths match
  if ((length(date_str) * 3) != nlyr(rstack1)) {
    stop("The number of dates does not match the number of layers in the SpatRaster.")
  }
  
  # Initialize an empty vector to hold the new names
  new_names <- vector("character", length = nlyr(rstack1))
  
  # Loop through each date to rename the corresponding layers
  for (i in seq_along(date_str)) {
    idx_GSLA <- (i - 1) * 3 + 1
    idx_UCUR_MEAN <- (i - 1) * 3 + 2
    idx_VCUR_MEAN <- (i - 1) * 3 + 3
    
    new_names[idx_GSLA] <- paste0("GSLA_", date_str[i])
    new_names[idx_UCUR_MEAN] <- paste0("UCUR_", date_str[i])
    new_names[idx_VCUR_MEAN] <- paste0("VCUR_", date_str[i])
  }
  
  # Rename the layers
  names(rstack1) <- new_names
  
  # Crop the stack
  e <- ext(c(150, 155, -36, -24)) #xmin, xmax, ymin, ymax
  rstack2 <- terra::crop(rstack1, e)
  
  # Save the processed stack as a TIFF file
  tif_file <- paste0("Current_stack_", year, ".tif")
  writeRaster(rstack2, tif_file)
  
  # Return the processed stack
  return(rstack2)
}

# Process each year
years <- c("2024")  # Edit the years as needed

for (year in years) {
  rstack3 <- process_year(year)
  # You can save the processed_stack if needed for each year
}


#check it worked
plot(rstack3, col = viridis(255))

writeRaster(rstack3, "Currents_stack_2024.tif")


