# 04 September 2023
# Who needs remora anyway
# Interpolating NAs in SST data

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")
sst.pts2 <- read_csv("Inputs/230909_SST_vals_12-22_pts2.csv")
rcs <- read_csv("Inputs/230909_XY_receivers.csv")
UTM56S <- crs("EPSG:32756")# Coordinate reference systems

head(rcs) #its all there

pts.sp <- st_as_sf(rcs, coords = c("receiver_deployment_longitude", #convert to an SF object
                                   "receiver_deployment_latitude")) 

st_crs(pts.sp) <- crs(UTM56S) #remember to assign crs
pts.sp

pts.UTM <- st_transform(pts.sp, UTM56S) #reproject our data
pts.UTM


# bilinear extrapolation --------------------------------------------------

bl <- extract(rstack, pts.UTM, method = "bilinear") # ID = FALSE otherwise it creates a column with a number for each spoint


rcs <-  rcs %>% mutate(RowNumber = row_number()) #make a row number 
bl <-  bl %>% mutate(RowNumber = row_number()) #make a row number 

bl1 <- left_join(bl, rcs %>% dplyr::select(RowNumber, station_name), by = "RowNumber")

#re-order it
bl1 <- bl1 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(station_name, everything())

bl2 <- bl1 %>% 
  dplyr::select(-ID, -station_name)

# nearest temporal neighbour ----------------------------------------------

bl3 <- t(apply(bl2, 1, function(row) { 
  for (j in 1:length(row)) {  
    if (is.na(row[j])) {  
      neighbors <- c()
      if(j > 1) {
        neighbors <- c(neighbors, row[j-1])
      }
      if(j < length(row)) {
        neighbors <- c(neighbors, row[j+1])
      }
      non_na_neighbors <- neighbors[!is.na(neighbors)] 
      if (length(non_na_neighbors) > 0) { 
        row[j] <- non_na_neighbors[1] 
      }
    }
  }
  
  # New loop to replace NaN with NA
  for (j in 1:length(row)) {
    if (is.nan(row[j])) {
      row[j] <- NA
    }
  }
  
  return(row)  
}))

bl3 <- as.data.frame(bl3)

sum(is.na(bl3))

114*3871
(135519 / 441294) * 100
# 61% NA to 31% :o

# 5 d mean ----------------------------------------------------------------

# Function to fill NAs with 5-day rolling mean
mean_5d <- function(row) {
  n <- length(row)  # Get the length of the row
  new_row <- numeric(n)   # Initialize an empty vector to store the new values
  
  for (j in 1:n) {  
    if (is.na(row[j])) {  # If value is NA
      # Find the 5-day window around the NA
      start_window <- max(1, j - 2)  # Window start (making sure it's not < 1)
      end_window <- min(n, j + 2)  # Window end (making sure it's not > n)
      mean_window <- mean(row[start_window:end_window], na.rm = TRUE) # Calculate the mean of the window, excluding NA
      new_row[j] <- ifelse(is.na(mean_window), NA, mean_window) # If mean is still NA (i.e., all values in the window were NA), keep it as NA
    } else {  # If value is not NA, keep it as is
      new_row[j] = row[j]
    }
  }
  return(new_row)
}

# Apply the function to each row and save the new values in sst.pts2
bl4 <- t(apply(bl3, 1, mean_5d))

bl4 <- as.data.frame(bl4)
colnames(bl4) <- colnames(bl2)

sum(is.na(bl3)) - sum(is.na(bl4))  #only 22 were filled :o

sum(is.na(bl4)) 
(135497 / 441294) * 100
#30%

write_csv(bl4, file = "Inputs/230909_SST_bl_vals_12-22.csv")
