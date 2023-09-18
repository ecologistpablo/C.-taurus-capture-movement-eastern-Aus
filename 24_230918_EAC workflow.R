

# load SST ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# combine all stacks  -----------------------------------------------------

setwd("~/University/2023/Honours/R/data/IMOS/SST")
list.files()

# Generate file names for the years
file_names <- paste0("SST_stack_", 2012:2022, ".tif")

# Coordinate reference systems
WGS84 <- crs("EPSG:4326")

# Initialize an empty list to store the rasters
rasters_list <- list()

# Loop through each file name
for (file_name in file_names) {
  if (file.exists(file_name)) {  # Check if the file exists
    r <- rast(file_name) # Read in the raster stack
    crs(r) <- WGS84 # Assign CRS
    rasters_list[[length(rasters_list) + 1]] <- r # Append to the list
  }
}

# Combine the individual rasters into one stack
sst_stack <- do.call(c, rasters_list)


# Check the CRS of the combined stack
crs(rstack)


# load current stack ------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

cur_stack <- rast("IMOS/Currents/230912_cstack_12-22.tif")


# pts ---------------------------------------------------------------------


rcs <- read_csv("Inputs/230909_XY_receivers.csv")
WGS84 <- crs("EPSG:4326")# Coordinate reference systems

pts.WGS <- st_as_sf(rcs, coords = c("receiver_deployment_longitude", #convert to an SF object
                                    "receiver_deployment_latitude")) 

st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS




# define the EAC ----------------------------------------------------------

# Initialize an empty dataframe to store results
results <- data.frame()

# Get the column names (dates) from the current stack
date_columns <- names(cur_stack)

# Loop through each date column
for (date_col in date_columns) {
  # Extract the current layer for this date from the current stack
  cur_layer <- cur_stack[[date_col]]
  sst_layer <- sst_stack[[date_col]]
  
  # Extract coordinates and values
  xy <- xy(cur_layer)
  lon_lat_data <- data.frame(
    x = xy[, 1],
    y = xy[, 2],
    cur_values = values(cur_layer),
    sst_values = values(sst_layer)
  )
  
  # Calculate the magnitude of ocean current speed
  cur_speed_layer <- sqrt(lon_lat_data$cur_values[1:(nrow(lon_lat_data) / 3)]^2 +
                            lon_lat_data$cur_values[(nrow(lon_lat_data) / 3 + 1):(2 * nrow(lon_lat_data) / 3)]^2)
  
  # Compute a 30 km moving average for speed
  f <- matrix(c(0, 1, 1), nrow = 1, ncol = 3, byrow = TRUE)
  cur_speed_grad <- focal(cur_layer, w = f, fun = function(x, ...) x[3] - x[2], na.rm = TRUE)
  cur_speed_av <- focal(cur_speed_grad, w = matrix(1, ncol = 3, nrow = 1), fun = mean, na.rm = TRUE)
  
  # Apply the inner and outer longitude thresholds to cur_speed_av
  cur_speed_av[which(lon_lat_data$x < inner_threshold | lon_lat_data$x > outer_threshold)] <- NA
  
  # Calculate distances from points to the EAC edge (based on thresholds)
  lon_lat_data$distance_to_edge <- NA
  
  for (j in 1:nrow(lon_lat_data)) {
    if (!is.na(lon_lat_data$cur_values[j])) {
      lon <- lon_lat_data$x[j]
      lat <- lon_lat_data$y[j]
      
      # Check if the point is within the EAC edge
      if (lon > inner_threshold && lon < outer_threshold) {
        # Calculate distance to EAC edge
        edge_lon <- ifelse(lon > (inner_threshold + outer_threshold) / 2, outer_threshold, inner_threshold)
        distance <- distVincentySphere(c(lon, lat), c(edge_lon, lat))
        lon_lat_data$distance_to_edge[j] <- distance
      }
    }
  }
  
  # Extract the date from the column name (e.g., "GSLA_20120101" becomes "20120101")
  date <- gsub("[^0-9]", "", date_col)
  
  # Add the date as a column in the dataframe
  lon_lat_data$date <- date
  
  # Combine results for this date with the overall results
  results <- rbind(results, lon_lat_data)
}

