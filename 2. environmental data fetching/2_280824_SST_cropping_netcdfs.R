#28.08.23
  #automating the workflow to crop, stack and reproject our data
rm(list=ls())

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# metadata finding --------------------------------------------------------

setwd("E:/Pablo/2023_hons_dat/SST/2012") #where did you download all your net cdfs?
list.files() #copy and paste a string below
nc <- nc_open("20121231092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn-v02.0-fv02.0.nc") #read it in

print(nc) #what variables do we choose need?


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

#now that we know how to process a single year, we can loop it into processing all our data

# Define a function to process a specific year
process_year <- function(year) {
  
  # Set working directory to the year's folder
  setwd(paste0("E:/Pablo/2023_hons_dat/SST/", year))
  
  plan(multisession, workers = 6) # Use 6 cores
  
  # List all the .nc files in the directory
  file_list <- list.files(pattern = "\\.nc$", full.names = TRUE)
  
  # Read each NetCDF file as a SpatRaster
  r_list <- lapply(file_list, rast)
  
  # Stack the rasters
  rstack <- rast(c(r_list))
  
  # Select and keep only the layers with the specified name pattern
  rstack1 <- subset(rstack, grep("^sea_surface_temperature", names(rstack))) 
  
  # Extract the date part (YYYYMMDD) from each .nc file name
  date_str <- substr(file_list, 3, 10)
  
  # Create new names by adding "SST_" prefix to the date
  new_names <- paste0("SST_", date_str)
  
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
  tif_file <- paste0("SST_stack_", year, ".tif")
  writeRaster(rstack3, tif_file)
  
  
  # Return the processed stack
  return(rstack3)
}

# Process each year
years <- c("2022") #change as needed, processing 1 year at a time is chunky enough as they're 70mb and I only have 8 cores on 16gb of ram 

for (year in years) {
  rstack4 <- process_year(year)
  # You can save the processed_stack if needed for each year
}


#check it worked
plot(rstack4, col = viridis(255))

setwd("~/University/2023/Honours/R/data/IMOS/SST")

writeRaster(rstack4, "SST_stack_2022.tif")


# combine all stacks  -----------------------------------------------------

rm(list=ls())
setwd("~/University/2023/Honours/R/data/IMOS/SST")
list.files()

# Generate file names for the years
file_names <- paste0("SST_stack_", 2012:2022, ".tif")

# Read in existing files and combine them into one stack
SST_stack <- rast(lapply(file_names, function(x) if(file.exists(x)) rast(x)))


rstack <- project(rstack, UTM56S)
head(rstack)
