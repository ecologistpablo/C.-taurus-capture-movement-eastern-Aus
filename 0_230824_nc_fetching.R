#Written to scrape GHRSST & AODN current data 
  # Written by Dave Schoeman for Scales Lab tutorial, modified by P. Fuenzalida
    # August 2023
      #Who needs remora anyway

setwd("~/University/2023/Honours/R/data/IMOS/SST/2012")


# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# Get the links -----------------------------------------------------------

#set an output folder
output_folder <- "C:/Users/pablo/Documents/University/2023/Honours/R/data/IMOS/SST/2012"

#what variable in the nc file are you wrangling (manually download n check)
var_to_get <- "sea_surface_temperature" # What variable are we interested in

#set ul
# url <- "https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/2012/20120101092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn-v02.0-fv02.0.nc" # The BRAN catalog URL html : hyper text marker language
yurl <- "https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/2012/" #year url
# durl <- "https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/" #product url


html <- rvest::read_html(yurl) # Read as html
html
f <- html[1] # just get the first one

file_tibble <- html %>% 
  html_node("table") %>% # Get the table of messy (html-formatted) links
  html_table() # Write it as a cleaned tibble

str(file_tibble)

files <- file_tibble %>% 
  dplyr::select(Name) %>% # Select just the dataset names
  filter(str_detect(Name, var_to_get)) %>%  # Filter for the specified variable
  pull(Name) # Pull out the contents of the variable Dataset as a vector
# Of course, if you want to get a subset of files, say for 2016, you add "%>% str_subset("2016")"  
# %>% str_subset("2016", negate = TRUE) #means that we want everything but 2016

files <- files %>%
  str_subset(dir(output_folder, pattern = var_to_get), negate = TRUE)
#If you're function crashes, you can add this clause in to say go look in your dir
#and if the file exists, don't download it then
#negate = not


# Function to download each file ------------------------------------------

#options(time = 600) #60 * 10 is 10min

get_file <- function(filename) {
  # Construct the URL for downloading the file
  url <- paste0(yurl, "2012/", filename)
  
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
plan(multisession, workers = 6) # Using 2 cores

#furrr (walking into the future)
future_walk(files, get_file)



