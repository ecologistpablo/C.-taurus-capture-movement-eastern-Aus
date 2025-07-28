#28.08.23
  #cropping our netcdfs

# now that we have 11 years worth of data, we need to crop to our studysite
# we do this by setting our max x and y coordinates
# so we cut a big netcdf into a smaller one, to reduce the size of data needed on our computers
# we also will rename them to make sure we know what day we're wrestling

rm(list=ls())

# Packages ----------------------------------------------------------------

pacman::p_load("tidyverse", "ncdf4", 'purrr', 'furrr','future', 'terra', 'sf', 'sp', 'viridis')

# metadata finding --------------------------------------------------------

setwd("/Volumes/LaCie_PF/SST/")
setwd("E:/Pablo/2023_hons_dat/SST/2012") #where did you download all your net cdfs?
list.files() #copy and paste a string below
nc <- nc_open("20231231092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn-v02.0-fv01.0.nc") #read it in

print(nc) #what variables do we choose need?
plot(nc)

# loop --------------------------------------------------------------------

# Define a function to process a specific year
process_year <- function(year) {
  
  # Set working directory to the year's folder
  setwd(paste0("/Volumes/LaCie_PF/SST/", year))
  
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


# process years independently ---------------------------------------------

# change 2022 to whichever year you need

# Process each year
years <- c("2024") #change as needed, processing 1 year at a time is chunky enough as they're 70mb and I only have 8 cores on 16gb of ram 

for (year in years) {
  rstack4 <- process_year(year)
  # You can save the processed_stack if needed for each year
}


#check it worked
plot(rstack4[[4,]], col = viridis(255)) # of course it did we're wizards

setwd("/Volumes/LaCie_PF/SST/2024")
list.files()
writeRaster(rstack4, "SST_stack_2024.tif")

