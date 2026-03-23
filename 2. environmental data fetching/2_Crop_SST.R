#28.08.23
  #cropping our netcdfs

# now that we have 11 years worth of data, we need to crop to our studysite
# we do this by setting our max x and y coordinates
# so we cut a big netcdf into a smaller one, to reduce the size of data needed on our computers
# we also will rename them to make sure we know what day we're wrestling

rm(list=ls())

# Packages ----------------------------------------------------------------

pacman::p_load("tidyverse", "ncdf4", 'purrr', 'furrr',
               'future', 'terra', 'sf', 'sp', 'viridis',
               "future.apply")

# metadata finding --------------------------------------------------------

setwd("/Volumes/LaCie_PF/IMOS/SST")
#setwd("E:/Pablo/2023_hons_dat/SST/2012") #where did you download all your net cdfs?
setwd("~/Documents/USC/Honours/R/data") #my working directory, change to yours"/")
list.files() #copy and paste a string below
nc <- nc_open("20250606092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn-v02.0-fv01.0.nc") #read it in

nc
plot(nc)

# loop --------------------------------------------------------------------

# Define a function to process a specific year
process_year <- function(year) {
  
  setwd(paste0("/Volumes/LaCie_PF/IMOS/SST/", year)) # set dir to change with year
  
  # List all the .nc files in the directory
  file_list <- list.files(pattern = "\\.nc$", full.names = TRUE)
  
  # Read each NetCDF file as a SpatRaster
  r_list <- future_lapply(file_list, rast) # using parallel processing
  
  # Stack the rasters
  rstack <- rast(c(r_list))
  
  # Select and keep only the layers with the specified name pattern
  rstack1 <- subset(rstack, which(names(rstack) == "sea_surface_temperature"))
  
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
  e <- ext(c(150, 155, -37, -22)) #xmin, xmax, ymin, ymax
  rstack2 <- terra::crop(rstack1, e)
  
  # Convert from Kelvin to Celsius
  rstack3 <- app(rstack2, function(x) x - 273.15)
  
  # Save the processed stack as a TIFF file
  tif_file <- paste0("SST_stack_", year, ".tif")
  writeRaster(rstack3, tif_file, overwrite = T)
  
  
  # Return the processed stack
  return(rstack3)
}


# process years independently ---------------------------------------------

# c
years <- 2012

# Set up once before the loop
plan(multisession, workers = 6)

for (year in years) {
  rstack <- process_year(year)
}

# Clean up after
plan(sequential)

# re-trying ---------------------------------------------------------------

process_year <- function(year) {
  
  setwd(paste0("/Volumes/LaCie_PF/IMOS/SST/", year))
  
  file_list <- list.files(pattern = "\\.nc$", full.names = TRUE)
  
  # Sequential read - disk I/O doesn't benefit from parallelism
  r_list <- lapply(file_list, function(f) {
    tryCatch(rast(f), error = function(e) {
      message("Skipping: ", f)
      NULL
    })
  })
  
  failed <- sapply(r_list, is.null)
  if (any(failed)) {
    message(sum(failed), " file(s) skipped for year ", year)
    r_list    <- r_list[!failed]
    file_list <- file_list[!failed]
  }
  
  if (length(r_list) == 0) stop("No valid .nc files found for year: ", year)
  
  rstack  <- rast(c(r_list))
  rstack1 <- subset(rstack, which(names(rstack) == "sea_surface_temperature"))
  
  date_str  <- substr(file_list, 3, 10)
  new_names <- paste0("SST_", date_str)
  
  if (length(new_names) == nlyr(rstack1)) {
    names(rstack1) <- new_names
  } else {
    stop(paste("Name mismatch:", length(new_names), "names vs", nlyr(rstack1), "layers."))
  }
  
  e       <- ext(c(150, 155, -37, -22))
  rstack2 <- terra::crop(rstack1, e)
  
  # CPU-bound conversion - parallelism helps here
  rstack3 <- app(rstack2, function(x) x - 273.15, cores = 6)
  
  tif_file <- paste0("SST_stack_", year, ".tif")
  writeRaster(rstack3, tif_file, overwrite = TRUE)
  
  return(rstack3)
}

years <- 2018:2025
tic()
for (year in years) {
  rstack2 <- process_year(year)
}
toc()
plot(rstack[[19]], col = viridis(255))
