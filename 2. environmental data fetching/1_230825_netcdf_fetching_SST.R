#Written to scrape GHRSST & IMOS current data from the AODN thredds server 
  # Bones written by Prof. Dave Schoeman, modified by P. Fuenzalida
    # August 2023
      #Who needs REMORA anyway

setwd("~/Documents/USC/Honours/R/data/IMOS/SST/2012")
> setwd("~/Documents/USC/Honours/R/data/IMOS")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# preamble ----------------------------------------------------------------

# we are going to talk to IMOS' environmental product website
# to download data for sea surface temperature from everyday inbetween 2012 - 2022
# purrr, furrr and dplyr can help us with this
# we do this by specifying output folder, the URL string onto the directory before we enter years
# https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/
# to then specify which years we want
# we then scrape each year url to convert it to a table
# then tell r go download every file in this table
# we do this for GHRSST data first (the product), then current data next
# you can do this with any product on any website by tweaking it a bit

# purrr and furrr our way thru --------------------------------------------

parallel::detectCores() #how much power do you have? I got 8 so I will use 6

# never use all of your cores, your computer needs at least one to function

# Set up parallel processing
plan(multisession, workers = 6) # Using x cores

# Loop through years for your study period
for (year in 2012:2022) {  #change if you have different study period
  
  # Set output folder and URL based on the year
  #output_folder <- "C:/Users/pablo/Documents/University/2023/Honours/R/data/IMOS/SST/2014" 
  output_folder <- paste0("E:/Pablo/2023_hons_dat/", year) #my output folder
  yurl <- paste0("https://mrs-data.csiro.au/imos-srs/sst/ghrsst/L3S-1d/dn/", year, "/") #the beginning string for year url 
  
  # Read HTML content
  html <- rvest::read_html(yurl) #rvest helps us talk to websites
  
  # Extract table
  file_tibble <- html %>% 
    html_node("table") %>% #turn it into a table 
    html_table() #present it as a table
  
  # Filter files
  files <- file_tibble %>% #turn table into tibble 
    dplyr::select(Name) %>%  #read the Name column
    filter(str_detect(Name, "GHRSST")) %>% 
    pull(Name) #pull the names into the files tibble
  
  # if this function stops, don't re-download data to the same output folder
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

# nice
