#Written to scrape GHRSST & IMOS current data from the AODN thredds server 
  # Bones written by Prof. Dave Schoeman, modified by P. Fuenzalida
    # August 2023
      #Who needs REMORA anyway

setwd("~/University/2023/Honours/R/data/IMOS/SST/2012")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")



# preamble ----------------------------------------------------------------

# we are going to talk to IMOS' environmental product website
# to downloada data for SST from 2012 - 2022
# purrr, furrr and dplyr can help us with this
# we specify output folder, the URL string onto the directory before we enter years
# https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/
# to then specify which years we want
# we then scrape each year url to convert it to a datatable
# then tell r go download every file in this table
# we do this for GHRSST data first, then current data next

# purrr and furrr our way thru --------------------------------------------

parellel::detectCores()

# Set up parallel processing
plan(multisession, workers = 6) # Using x cores

# Loop through years for your study period
for (year in 2012:2022) {  
  
  # Set output folder and URL based on the year
  #output_folder <- "C:/Users/pablo/Documents/University/2023/Honours/R/data/IMOS/SST/2014" 
  output_folder <- paste0("E:/Pablo/2023_hons_dat/", year) #my output folder
  yurl <- paste0("https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/", year, "/") #the beginning string for year url 
  
  # Read HTML content
  html <- rvest::read_html(yurl) #rvest helps us access websites
  
  # Extract table
  file_tibble <- html %>% 
    html_node("table") %>% #turn it into a table 
    html_table() #present it as a table
  
  # Filter files
  files <- file_tibble %>% #turn table into tibble 
    dplyr::select(Name) %>%  #read the Name column
    filter(str_detect(Name, "GHRSST")) %>% 
    pull(Name)
  
  # if this function stops, don't redownload data to the same output folder
  existing_files <- dir(output_folder, pattern = var_to_get)
  files <- files[!(files %in% existing_files)]
  
  # Download files using parallel processing
  future_walk(files, function(filename) {
    url <- paste0(yurl, filename)
    destination <- file.path(output_folder, filename)
    
    message("Attempting to download from: ", url) #because we are civilised return msgs
    message("Destination: ", destination)
    
    tryCatch({
      curl_download(url, destfile = destination, quiet = FALSE)
    }, error = function(e) {
      message(sprintf("Failed to download %s. Error: %s", url, e$message))
    })
  })
}
