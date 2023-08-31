#27.08.23
  #Downloading all gm OceanCurrent data
    #2012 - 2020


# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")


# for loop de loop --------------------------------------------------------

# Set up parallel processing
plan(multisession, workers = 7) # Using 7 cores

# Loop through years from 2012 to 2020
for (year in 2012:2020) {
  
  # Set output folder and URL based on the year
  output_folder <- paste0("E:/Pablo/2023_hons_dat/Current", year)
  yurl <- paste0("https://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/DM/", year, "/catalog.html")
  
  # Read HTML content
  html <- rvest::read_html(yurl)
  
  # Extract table
  file_tibble <- html %>% 
    html_node("table") %>% 
    html_table()
  
  # Filter files
  files <- file_tibble %>% 
    dplyr::select(Name) %>% 
    filter(str_detect(Name, "GSLA")) %>%
    pull(Name)
  
  # Filter out existing files
  existing_files <- dir(output_folder)
  files <- files[!(files %in% existing_files)]
  
  # Download files using parallel processing
  future_walk(files, function(filename) {
    url <- paste0("https://thredds.aodn.org.au/thredds/fileServer/IMOS/OceanCurrent/GSLA/DM/", year, "/", filename)
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
