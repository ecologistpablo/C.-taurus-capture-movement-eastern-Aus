#27.08.23
  #Downloading all near real time (NRT) OceanCurrent data
    #2021 - 2022


# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# for loop de loop --------------------------------------------------------

#for detailed annotations, see the GHRSST fetching script

# Set up parallel processing
plan(multisession, workers = 7) # Using 7 cores

# Loop through years from 2021 to 2022
for (year in 2021:2022) {

    # Set output folder and URL based on the year
  output_folder <- paste0("E:/Pablo/2023_hons_dat/Current/", year)
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
  
  # Download files using parallel processing
  future_walk(files, function(filename) {
    url <- paste0("https://thredds.aodn.org.au/thredds/fileServer/IMOS/OceanCurrent/GSLA/NRT/", year, "/", filename)
    destination <- file.path(output_folder, filename)  
    
    message("Attempting to download from: ", url)
    message("Destination: ", destination)
    
    tryCatch({
      curl_download(url, destfile = destination, quiet = FALSE, mode = "wb")
    }, error = function(e) {
      message(sprintf("Failed to download %s. Error: %s", url, e$message))
    })
  })
}
