#10.09.23
  #paired SCP data
    #extract SST vals for it

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

pts <- read_csv("~/University/2023/Honours/R/data/shark control/230910_XY_captures_12-22.csv")
UTM56S <- crs("EPSG:32756")# Coordinate reference systems

head(pts) #its all there

pts1 <- pts %>% 
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

str(pts1)
         
         
pts.UTM <- st_as_sf(pts1, coords = c("Longitude", #convert to an SF object
                                   "Latitude")) 

st_crs(pts.UTM) <- crs(UTM56S) #remember to assign crs
pts.UTM


# plot --------------------------------------------------------------------

plot(pts.UTM)
# Calculate the number of detections at each station
ptsxy <- pts %>%
  group_by( Latitude, Longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

ptsxy_sf <- sf::st_as_sf(ptsxy, coords = c("Longitude", "Latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(ptsxy_sf, cex = "num_det", fbg = F)

#they're plotting well, but a few rows should maybe be removed 

# extract -----------------------------------------------------------------

sst.pts <- extract(rstack, pts.UTM, ID = F) # ID = FALSE otherwise it creates a column with a number for each spoint


sum(is.na(sst.pts))
131*3871


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

131*3871
sum(is.na(sst.pts1)) #216812

(596137 / 600005) * 100
#99% NA, isn't that wonderful :()

sst.pts2 <- sst.pts1

# Bilinear interpolation --------------------------------------------------

bl <- extract(rstack, pts.UTM, method = "bilinear") # ID = FALSE otherwise it creates a column with a number for each spoint

bl2 <- bl %>% 
  dplyr::select(-ID)


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
#14,000 were filled :(, still 54,000 to go



fill_vals <- function(sst.pts2, bl3) {
  if (nrow(sst.pts2) != nrow(bl3) || ncol(sst.pts2) != ncol(bl3)) {
    stop("The dimensions of the two data frames must be identical.")
  }
  
  for (i in 1:nrow(sst.pts2)) {
    for (j in 1:ncol(sst.pts2)) {
      if (is.na(sst.pts2[i, j]) && !is.na(bl3[i, j])) {
        sst.pts2[i, j] <- bl3[i, j]
      }
    }
  }
  
  return(sst.pts2)
}

#fill vals of bilinear interpolation into our data
sst.pts3 <- fill_vals(sst.pts2, bl3)

sum(is.na(sst.pts3)) 

(464557 / 507101) * 100
#100 to 91%... Still horrible 


# resize coarseness -------------------------------------------------------

rstack #res at 0.02 by 0.02, 2km x 2km

rstack10km <- aggregate(rstack, fact = 5.5, #factor of whatever your resolution is in the OG raster / stack
                        fun = mean, na.rm = TRUE)
#this is a 10km resize

# resample ----------------------------------------------------------------

sst.pts10km <- extract(rstack10km, pts.UTM, ID = F) 

sum(is.na(sst.pts10km)) 
(255323 / 507101) * 100


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

sst.pts10km1 <- as.data.frame(sst.pts10km1) #convert back to df

sum(is.na(sst.pts10km1)) 
(73766 / 507101) * 100

#14% :D

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
sst.pts10km2 <- t(apply(sst.pts10km1, 1, mean_5d))

#more munging
sst.pts10km2 <- as.data.frame(sst.pts10km2)
colnames(sst.pts10km2) <- colnames(sst.pts10km1)

sum(is.na(sst.pts10km2))

(73626 / 507101) * 100
#still 12 but a tiny lil bit better

# fill_gaps ---------------------------------------------------------------

fill_vals <- function(df1, df2) {
  # Check if both data frames have the same dimensions
  if (nrow(df1) != nrow(df2) || ncol(df1) != ncol(df2)) {
    stop("The dimensions of the two data frames must be identical.")
  }
  
  # Loop through each row and column to fill NA values
  for (i in 1:nrow(df1)) {
    for (j in 1:ncol(df1)) {
      if (is.na(df1[i, j]) && !is.na(df2[i, j])) {
        df1[i, j] <- df2[i, j]
      }
    }
  }
  
  return(df1)
}


#fill values of 10km res into our 2km res
sst.pts4 <- fill_vals(sst.pts3, sst.pts10km2)

sum(is.na(sst.pts4))

(54291 / 507101) * 100

#10% NA still. Can bilinear interpolation help ?


# bli ---------------------------------------------------------------------

bl <- extract(rstack10km, pts.UTM, method = "bilinear") # ID = FALSE otherwise it creates a column with a number for each spoint

pts <-  pts %>% mutate(RowNumber = row_number()) #make a row number 
bl <-  bl %>% mutate(RowNumber = row_number()) #make a row number 

bl1 <- left_join(bl, pts %>% dplyr::select(RowNumber, Location), by = "RowNumber")

#re-order it
bl1 <- bl1 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(Location, everything())

bl2 <- bl1 %>% 
  dplyr::select(-ID, -Location)

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

131*3871
(4052 / 507101) * 100
# 0.79 % :D

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

# Apply the function to each row 
bl4 <- t(apply(bl3, 1, mean_5d))

bl4 <- as.data.frame(bl4)
colnames(bl4) <- colnames(bl2)



# fill_gaps of bli into sst.pts -------------------------------------------

#fill values of bilinear interpolation at 10km into our df
sst.pts5 <- fill_vals(sst.pts4, bl4)

sum(is.na(sst.pts5))
(3925 / 507101) * 100
#0.77 % :)

# add station name --------------------------------------------------------

sst.pts6 <-  sst.pts4 %>% mutate(RowNumber = row_number()) #make a row number 
pts <-  pts %>% mutate(RowNumber = row_number()) #make a row number 


sst.pts6 <- left_join(sst.pts6, pts %>% dplyr::select(RowNumber, Location), by = "RowNumber")

#re-order it
sst.pts6 <- sst.pts6 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(Location, everything())
ss

# save --------------------------------------------------------------------

write_csv(sst.pts6, file = "Inputs/230911_capture_SST.csv")

