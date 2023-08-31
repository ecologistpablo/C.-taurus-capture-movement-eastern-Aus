#28.08.23
#automating the workflow to crop, stack and reproject our data

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# old fashioned way: 2014 -------------------------------------------------

setwd("E:/Pablo/2023_hons_dat/Current/2012")

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
  pattern <- "^(UCUR_MEAN_TIME|VCUR_MEAN_TIME|GSLA_TIME)"
  
  # Use grep to find the indices of layers that match the pattern
  selected_indices <- grep(pattern, names(rstack))
  
  # Subset the SpatRaster to keep only those layers
  rstack_subset <- subset(rstack, selected_indices)
  
  return(rstack_subset)
}

# Run
rstack1 <- subset_selected_layers(rstack)

names(rstack1)

#nice

# renaming ----------------------------------------------------------------

head(names(rstack1))

# Extract the date part (YYYYMMDD) from each .nc file name
date_str <- substr(file_list, 24, 31)

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
  new_names[idx_GSLA] <- paste0("GSLA_", date_str[i])
  new_names[idx_UCUR_MEAN] <- paste0("UCUR_", date_str[i])
  new_names[idx_VCUR_MEAN] <- paste0("VCUR_", date_str[i])
}

# Apply the new names to the SpatRaster
names(rstack1) <- new_names

# Check the first few names to make sure they were renamed correctly
head(names(rstack1))

plot(rstack1[[2]], col = viridis(255))

# crop --------------------------------------------------------------------

# Define the extent
e <- ext(c(150, 155, -36, -24)) #xmin, xmax, ymin, ymax

# Crop the stack
rstack2 <- terra::crop(rstack1, e)

plot(rstack2, col = viridis(255))

# more munging ------------------------------------------------------------

# Convert from Kelvin to Celsius
rstack3 <- app(rstack2, function(x) x - 273.15)

# Check if it worked
plot(rstack3, col = viridis(255))

names(rstack3)


# save progress -----------------------------------------------------------

writeRaster(rstack3, "Current_stack_2014.tif")



# looped ------------------------------------------------------------------

# Define a function to process a specific year
process_year <- function(year) {
  
  # Set working directory to the year's folder
  setwd(paste0("E:/Pablo/2023_hons_dat/Current/", year))
  
  plan(multisession, workers = 6) # Use 6 cores
  
  # List all the .nc files in the directory
  file_list <- list.files(pattern = "\\.nc$", full.names = TRUE)
  
  # Read each NetCDF file as a SpatRaster
  r_list <- lapply(file_list, rast)
  
  # Stack the rasters
  rstack <- rast(c(r_list))
  
  # Select and keep only the layers with the specified name pattern
  rstack1 <- subset(rstack, grep("^sea_surface_temperature_day_night", names(rstack)))
  
  # Extract the date part (YYYYMMDD) from each .nc file name
  date_str <- substr(file_list, 3, 10)
  
  # Create new names by adding "Current_" prefix to the date
  new_names <- paste0("Current_", date_str)
  
  # Rename the layers
  if (length(new_names) == nlyr(rstack1)) {
    names(rstack1) <- new_names
  } else {
    stop("The number of names does not match the number of layers in the SpatRaster.")
  }
  
  # Crop the stack
  e <- ext(c(150, 155, -36, -24)) #xmin, xmax, ymin, ymax
  rstack2 <- terra::crop(rstack1, e)
  
  # Convert from Kelvin to Celsius
  rstack3 <- app(rstack2, function(x) x - 273.15)
  
  # Save the processed stack as a TIFF file
  tif_file <- paste0("Current_stack_", year, ".tif")
  writeRaster(rstack3, tif_file)
  
  
  # Return the processed stack
  return(rstack3)
}

# Process each year
years <- c("2019")

for (year in years) {
  rstack4 <- process_year(year)
  # You can save the processed_stack if needed for each year
}


#check it worked
plot(rstack4, col = viridis(255))

setwd("~/University/2023/Honours/R/data/IMOS/Current")

writeRaster(rstack4, "Current_stack_2018.tif")



