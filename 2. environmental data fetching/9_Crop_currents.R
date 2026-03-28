#28.08.23
  #automating the workflow to crop, stack and reproject our data
    #For currents


# Packages ----------------------------------------------------------------

pacman::p_load("tidyverse", "ncdf4", 'purrr', 'furrr','future', 'terra', 'sf', 'sp', 'viridis')

# metadata finding --------------------------------------------------------
rm(list=ls())
setwd("/Volumes/LaCie_PF/IMOS/Currents/2012")
list.files()
# nc <- nc_open("IMOS_OceanCurrent_HV_20241231T000000Z_GSLA_FV02_NRT.nc")
# nc <- rast("Current_stack_2024.tif")
# plot(nc)
# old fashioned way: 2012 -------------------------------------------------
 
plan(multisession, workers = 6) # Use 6 cores

# List all the .nc files in the directory
file_list <- list.files(pattern = "\\.nc$", full.names = TRUE)

r_list_raw <- lapply(file_list, function(f) {
  tryCatch(rast(f), error = function(e) { message("Skipping: ", basename(f)); NULL })
})

good_idx <- !sapply(r_list_raw, is.null)  # TRUE for every file that loaded
r_list <- Filter(Negate(is.null), r_list_raw)  # clean list for stacking
good_files <- file_list[good_idx]  # only files that actually loaded

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

#plot(rstack1, col = viridis(255))

#nice

# renaming ----------------------------------------------------------------

head(names(rstack1))

# Extract the date part (YYYYMMDD) from each .nc file name
date_str   <- substr(basename(good_files), 21, 29)  # dates from good files only
# if you're download different data, count where the filename is that you want
# in this case, the date is in the format YYYYMMDD and starts at character 23 and ends at character 31 in the file name
# you can edit this and play with it, check using head afterwards
# it's not so computationally expensive so I encourage trial and error with this step
head(date_str) #check

# # Check if the lengths match
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
  new_names[idx_GSLA] <- paste0("GSLA", date_str[i]) #so it will come out as GSLA_20120101 for 01 Jan 2012
  new_names[idx_UCUR_MEAN] <- paste0("UCUR", date_str[i])
  new_names[idx_VCUR_MEAN] <- paste0("VCUR", date_str[i])
}

# Apply the new names to the SpatRaster
names(rstack1) <- new_names

# Check the first few names to make sure they were renamed correctly
head(names(rstack1))

#plot(rstack1, col = viridis(255))

# crop --------------------------------------------------------------------

# Define the extent
e <- ext(c(149, 155, -37, -22)) #xmin, xmax, ymin, ymax for your studysite

# Crop the stack
rstack2 <- terra::crop(rstack1, e) # cropping takes awhile, be patient

plot(rstack2, col = viridis(255))

names(rstack2)

# save progress -----------------------------------------------------------
setwd("/Volumes/LaCie_PF/IMOS/Currents")
writeRaster(rstack2, "Current_stack_2012.tif")

rasta <- rast("Current_stack_2012.tif")

# combine stack -----------------------------------------------------------
rm(list=ls())
list.files()

# List all the .nc files in the directory
file_list <- list.files(pattern = "\\.tif$", full.names = TRUE)
combo_rstack <- rast(c(file_list))
head(names(combo_rstack))
tail(names(combo_rstack))

plot(combo_rstack[[10000:10010,]])


# reproject ---------------------------------------------------------------


WGS84 <- crs("EPSG:4326")

crs(combo_rstack) <- WGS84 # Assign CRS
crs(combo_rstack)

wrteRaster(combo_rstack, "Current_stack_2012-2025.tif")

