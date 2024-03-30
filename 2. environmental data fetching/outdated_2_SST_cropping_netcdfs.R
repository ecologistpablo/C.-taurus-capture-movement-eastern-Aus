# cropping data -----------------------------------------------------------

setwd("E:/Pablo/2023_hons_dat/SST/2012")

plan(multisession, workers = 6) # Use 6 cores

# List all the .nc files in the directory
file_list <- list.files(pattern = "\\.nc$", full.names = TRUE)

# Read each NetCDF file as a SpatRaster
r_list <- lapply(file_list, rast)

#stack them
rstack <- rast(c(r_list))

# Select and keep only the layers with the specified name pattern
rstack1 <- subset(rstack, grep("^sea_surface_temperature", names(rstack)))

# renaming ----------------------------------------------------------------

head(names(rstack))

# Extract the date part (YYYYMMDD) from each .nc file name
date_str <- substr(file_list, 3, 10)

head(date_str) #check

# Create new names by adding "SST_" prefix to the date
new_names <- paste0("SST_", date_str)

# Rename the layers in your SpatRaster object
if (length(new_names) == nlyr(rstack1)) {
  names(rstack1) <- new_names
} else {
  stop("The number of names does not match the number of layers in the SpatRaster.")
}

# Display the first few names
head(names(rstack1))

plot(rstack1)

# crop --------------------------------------------------------------------

# Define the extent
e <- ext(c(150, 155, -36, -24)) #xmin, xmax, ymin, ymax

# Crop the stack
rstack2 <- terra::crop(rstack1, e)

plot(rstack2, col = viridis(255))


# metric converisons ------------------------------------------------------

# Convert from Kelvin to Celsius
rstack3 <- app(rstack2, function(x) x - 273.15)

# Check if it worked
plot(rstack3, col = viridis(255))

names(rstack3)


# save progress -----------------------------------------------------------

writeRaster(rstack3, "SST_stack_2014.tif")


# loop --------------------------------------------------------------------
