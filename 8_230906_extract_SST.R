# 04 September 2023
  # Who needs remora anyway
    # Interpolating NAs in SST data

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

rcs <- read_csv("Inputs/230909_XY_receivers.csv")
UTM56S <- crs("EPSG:32756")# Coordinate reference systems

head(rcs) #its all there

pts.sp <- st_as_sf(rcs, coords = c("receiver_deployment_longitude", #convert to an SF object
                                   "receiver_deployment_latitude")) 

st_crs(pts.sp) <- crs(UTM56S) #remember to assign crs
pts.sp

pts.UTM <- st_transform(pts.sp, UTM56S) #reproject our data
pts.UTM

# plotting ----------------------------------------------------------------

plot(rstack[[19]], col = viridis(255))
plot(pts.UTM, add = T)

# extract -----------------------------------------------------------------

sst.pts <- extract(rstack, pts.UTM, ID = F) # ID = FALSE otherwise it creates a column with a number for each spoint


sum(is.na(sst.pts))
3871 * 114 #how many obs in total
(307253 / 441294) * 100 

#69.62% is NA

# nearest temporal neighbour ----------------------------------------------

sst.pts1 <- t(apply(sst.pts, 1, function(row) { # Apply a function to each row of 'sst.pts'.
  for (j in 1:length(row)) {  # Loop through each element of the row.
    if (is.na(row[j])) {  # Check if the element is NA.
      neighbors <- c(row[j-1], row[j+1]) # Find neighbors of the NA value (i.e., the previous and next values in the row).
      non_na_neighbors <- neighbors[!is.na(neighbors)] # Remove NAs from the neighbors.
      if (length(non_na_neighbors) > 0) { # If there are non-NA neighbors...
        row[j] <- non_na_neighbors[1] # Replace the NA with the first non-NA neighbor.
  
      }
    }
  }
  
  return(row)  # Return the modified row.
}))

sst.pts1 <- as.data.frame(sst.pts1)

sum(is.na(sst.pts1)) #216812

(181977 / 441294) * 100
# 41.23% NA


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
sst.pts2 <- t(apply(sst.pts1, 1, mean_5d))

sst.pts2 <- as.data.frame(sst.pts2)
colnames(sst.pts2) <- colnames(sst.pts1)

sum(is.na(sst.pts2)) - sum(is.na(sst.pts1))  #only 24 were filled :o

sum(is.na(sst.pts2)) 
(181953 / 441294) * 100 #41.23% are NA at a 2km resolution

#with nearest neighbour single day interpolation & 5 d mean

write_csv(sst.pts2, file = "Inputs/230909_SST_vals_12-22_pts2.csv")


# increase coarseness -----------------------------------------------------

rstack #res at 0.02 by 0.02, 2km x 2km

rstack10km <- aggregate(rstack, fact = 5.5, #factor of whatever your resolution is in the OG raster / stack
                        fun = mean, na.rm = TRUE)
#this is a 10km sample

# resample ----------------------------------------------------------------

sst.pts10km <- extract(rstack10km, pts.UTM, ID = F) 

# nearest temporal neighbour ----------------------------------------------

sst.pts10km1 <- t(apply(sst.pts10km, 1, function(row) { # Apply a function to each row of 'sst.pts'.
  for (j in 1:length(row)) {  # Loop through each element of the row.
    if (is.na(row[j])) {  # Check if the element is NA.
      neighbors <- c(row[j-1], row[j+1]) # Find neighbors of the NA value (i.e., the previous and next values in the row).
      non_na_neighbors <- neighbors[!is.na(neighbors)] # Remove NAs from the neighbors.
      if (length(non_na_neighbors) > 0) { # If there are non-NA neighbors...
        row[j] <- non_na_neighbors[1] # Replace the NA with the first non-NA neighbor.
        
      }
    }
  }
  
  return(row)  # Return the modified row.
}))

sst.pts1 <- as.data.frame(sst.pts1) #convert back to df

sum(is.na(sst.pts10km1)) #still got 81324 NAs
(65843 / 441294) * 100
#14%

# 5 d mean ----------------------------------------------------------------

# Apply the function to each row and save the new values in sst.pts2
sst.pts10km2 <- t(apply(sst.pts10km1, 1, mean_5d))

#more munging
sst.pts10km2 <- as.data.frame(sst.pts10km2)
colnames(sst.pts10km2) <- colnames(sst.pts10km1)

sum(is.na(sst.pts10km2))
(65819 / 441294) * 100
#still 14
  
  
# fill_gaps ---------------------------------------------------------------

fill_vals <- function(sst_pts2, sst_pts10km2) {
  if (nrow(sst_pts2) != nrow(sst_pts10km2) || ncol(sst_pts2) != ncol(sst_pts10km2)) {
    stop("The dimensions of the two data frames must be identical.")
  }
  
  for (i in 1:nrow(sst_pts2)) {
    for (j in 1:ncol(sst_pts2)) {
      if (is.na(sst_pts2[i, j]) && !is.na(sst_pts10km2[i, j])) {
        sst_pts2[i, j] <- sst_pts10km2[i, j]
      }
    }
  }
  
  return(sst_pts2)
}

#fill values of 10km res into our 2km res
sst.pts3 <- fill_vals(sst.pts2, sst.pts10km2)

sum(is.na(sst.pts3))

(65819 / 441294) * 100
#16% of our data is still NA :(

41 - 16
#25% of our data are sampled at 10km spatial resolution


# add station name --------------------------------------------------------

rcs <-  rcs %>% mutate(RowNumber = row_number()) #make a row number 
sst.pts3 <-  sst.pts3 %>% mutate(RowNumber = row_number()) #make a row number 


sst.pts4 <- left_join(sst.pts3, rcs %>% dplyr::select(RowNumber, station_name), by = "RowNumber")

#re-order it
sst.pts4 <- sst.pts4 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(station_name, everything())

# save --------------------------------------------------------------------

write_csv(sst.pts4, file = "Inputs/230909_SST_vals_12-22.csv")

write_csv(NA3, file = "Inputs/NA_rows_12-22.csv")
