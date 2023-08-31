#Written to scrape GHRSST & AODN current data 
# Written by Dave Schoeman for Scales Lab tutorial, modified by P. Fuenzalida
# August 2023
#Who needs remora anyway

rm(list=ls())
setwd("~/University/2023/Honours/R/data")


# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# Get the links -----------------------------------------------------------

#set an output folder
output_folder <- "E:/Pablo/2023_hons_dat/SST/2014"

#what variable in the nc file are you wrangling (manually download n check)
#var_to_get <- "sea_surface_temperature" # What variable are we interested in

#set ul
# url <- "https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/2012/20120101092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn-v02.0-fv02.0.nc" # The BRAN catalog URL html : hyper text marker language
yurl <- "https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/2014/" #year url
# durl <- "https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/" #product url


html <- rvest::read_html(yurl) # Read as html

#f <- html[1] just get the first one

file_tibble <- html %>% 
  html_node("table") %>% # Get the table of messy (html-formatted) links
  html_table() # Write it as a cleaned tibble

str(file_tibble)

#only keep files that have GHRSST in the Name as the rest are irrelevant
files <- file_tibble %>% 
  dplyr::select(Name) %>% 
  filter(str_detect(Name, "GHRSST")) %>%
  pull(Name)

#if the function crashes, only download string files that are not in the output folder
existing_files <- dir(output_folder)
files <- files[!(files %in% existing_files)

# Function to download each file ------------------------------------------

#options(time = 600) #60 * 10 is 10min

get_file <- function(filename) {
  # Construct the URL for downloading the file
  url <- paste0(yurl, filename)
  
  # Destination where the file should be saved
  destination <- file.path(output_folder, filename) # Make sure this line is correct
  
  message("Attempting to download from: ", url)
  message("Destination: ", destination)
  
  tryCatch({
    curl_download(url, destfile = destination, quiet = FALSE)
  }, error = function(e) {
    message(sprintf("Failed to download %s. Error: %s", url, e$message))
  })
}

test_file <- files[1]
get_file(test_file)


# I aint a furry but I do urrr --------------------------------------------

#purrr (walk)
purrr::walk(files, get_file)

# Set up for parallel processing
plan(multisession, workers = 7) # Using 2 cores

#furrr (walking into the future)
future_walk(files, get_file)



# stack and crop ----------------------------------------------------------

#list
sst2012 <- list.files("IMOS/SST/2012", pattern = "\\.nc$", full.names = TRUE)

head(sst2012)

# extent
lat_min <- -37
lat_max <- -23
lon_min <- 150
lon_max <- 155


# Set up parallel processing
plan(multisession, workers = detectCores() - 2)

# List all the .nc files in the directory
file_list <- list.files(pattern = "*.nc")

read_raster <- function(file) {
  # Read the file as a SpatRaster
  r <- rast(file)
  
  # Extract the date from the filename and create a label
  date <- str_sub(file, 1, 8) # Extracting first 8 characters
  label <- paste0("SST", date)
  
  # Set the name for the SpatRaster
  names(r) <- label
  
  return(r)
}

# Read rasters in parallel and then combine them into one SpatRaster
spat_rasters <- future_map(file_list, read_raster)
s_combined <- terra::rast(spat_rasters)
str(s_combined)

# Define and crop to the extent
e <- ext(-36, 155, -24, 150)
s_cropped <- crop(s_combined, e)
