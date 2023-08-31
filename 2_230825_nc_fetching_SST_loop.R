#Written to scrape GHRSST & AODN current data 
# Written by Dave Schoeman for Scales Lab tutorial, modified by P. Fuenzalida
# August 2023
#Who needs remora anyway

setwd("~/University/2023/Honours/R/data/IMOS/SST/2012")


# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")


# le loop -----------------------------------------------------------------

# Set up parallel processing
plan(multisession, workers = 6) # Using x cores

# Loop through years from 2016 to 2022
for (year in 2013:2022) {  
  
  # Set output folder and URL based on the year
  #output_folder <- "C:/Users/pablo/Documents/University/2023/Honours/R/data/IMOS/SST/2014"
  output_folder <- paste0("E:/Pablo/2023_hons_dat/", year)
  yurl <- paste0("https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/", year, "/")
  
  # Read HTML content
  html <- rvest::read_html(yurl)
  
  # Extract table
  file_tibble <- html %>% 
    html_node("table") %>% 
    html_table()
  
  # Filter files
  files <- file_tibble %>% 
    dplyr::select(Name) %>% 
    filter(str_detect(Name, "GHRSST")) %>%
    pull(Name)
  
  # Filter out existing files
  existing_files <- dir(output_folder, pattern = var_to_get)
  files <- files[!(files %in% existing_files)]
  
  # Download files using parallel processing
  future_walk(files, function(filename) {
    url <- paste0(yurl, filename)
    destination <- file.path(output_folder, filename)
    
    message("Attempting to download from: ", url)
    message("Destination: ", destination)
    
    tryCatch({
      curl_download(url, destfile = destination, quiet = FALSE)
    }, error = function(e) {
      message(sprintf("Failed to download %s. Error: %s", url, e$message))
    })
  })
}
