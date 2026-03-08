#27.08.23
  #Downloading all near real time (NRT) OceanCurrent data
    #2021 - 2022


# Packages ----------------------------------------------------------------

pacman::p_load("purrr", "furrr", "future", "tidyverse", "rvest", "curl") #rvest and curl help us talk to websites

# for loop de loop --------------------------------------------------------

#for detailed annotations, see the GHRSST fetching script


setwd("/Volumes/LaCie_PF")
setwd("~/Documents/USC/Honours/R/data/IMOS/Currents/2025")


# Set up parallel processing
plan(multisession, workers = 6) # Using 7 cores

# Loop through years from 2021 to 2022
for (year in 2021:2022) {
#(year in year in 2023:2024) for multiple years
    # Set output folder and URL based on the year
  #output_folder <- paste0("Currents/", year)
  yurl <- paste0("https://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/NRT/", year, "/catalog.html")
  
  # Read HTML content
  html <- rvest::read_html(yurl)
  
  # Extract table
  file_tibble <- html %>% 
    html_node("table") %>% 
    html_table()
  
  # Filter files
  files <- file_tibble %>% 
    dplyr::select(Dataset) %>% 
    filter(str_detect(Dataset, "GSLA")) %>%
    pull(Dataset)
  
  # Filter out existing files
  existing_files <- dir(output_folder)
  files <- files[!(files %in% existing_files)]
  
  abs_output_folder <- normalizePath(output_folder, mustWork = TRUE) # future walk has been updated
  # this will make sure that if you have already downloaded some files, it won't try to download them again, which can save you a lot of time and prevent errors
  
  # Download files using parallel processing
  future_walk(files, function(filename) {
    url <- paste0("https://thredds.aodn.org.au/thredds/fileServer/IMOS/OceanCurrent/GSLA/NRT/", year, "/", filename)
    destination <- file.path(abs_output_folder, filename)  
    
    message("Attempting to download from: ", url)
    message("Destination: ", destination)
    
    tryCatch({
      curl_download(url, destfile = destination, quiet = FALSE, mode = "wb")
    }, error = function(e) {
      message(sprintf("Failed to download %s. Error: %s", url, e$message))
    })
  })
}
