#10.09.23
  #paired SCP data
    #extract CUR vals for it

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

pts <- read_csv("shark control/230910_XY_captures_12-22.csv")
UTM56S <- crs("EPSG:32756")# Coordinate reference systems
rstack <- rast("IMOS/Currents/Currents_u_12-22.tif")

head(pts) #its all there
rstack #nice


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

# Calculate the number of detections at each station
ptsxy <- pts %>%
  group_by( Latitude, Longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

ptsxy_sf <- sf::st_as_sf(ptsxy, coords = c("Longitude", "Latitude"),
                         crs= 4326, agr = "constant")

mapview::mapview(ptsxy_sf, cex = "num_det", fbg = F)

#they're plotting well, everything looks to be in order

# extract -----------------------------------------------------------------

cur.pts <- extract(rstack, pts.UTM, ID = F) # ID = FALSE otherwise it creates a column with a number for each spoint

sum(is.na(cur.pts))
(1036208 / 1577895) * 100
#65 % NA

# nearest temporal neighbour ----------------------------------------------

cur.pts1 <- t(apply(cur.pts, 1, function(row) { # Apply a function to each row of 'cur.pts'.
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

cur.pts1 <- as.data.frame(cur.pts1)

sum(is.na(cur.pts1))

(1120185 / 1866975) * 100
#60% still NA :o

cur.pts2 <- cur.pts1

# Bilinear interpolation --------------------------------------------------

bl <- extract(rstack, pts.UTM, method = "bilinear") # ID = FALSE otherwise it creates a column with a number for each spoint

bl2 <- bl %>% 
  dplyr::select(-ID)

sum(is.na(bl2))
(12080 / 1866975) * 100


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
#only 12045 :o
(12045 / 1866975) * 100
#0.6%  NA :o


# fill values from bilinear interpolation into our pts --------------------

fill_vals <- function(cur.pts2, bl3) {
  if (nrow(cur.pts2) != nrow(bl3) || ncol(cur.pts2) != ncol(bl3)) {
    stop("The dimensions of the two data frames must be identical.")
  }
  
  for (i in 1:nrow(cur.pts2)) {
    for (j in 1:ncol(cur.pts2)) {
      if (is.na(cur.pts2[i, j]) && !is.na(bl3[i, j])) {
        cur.pts2[i, j] <- bl3[i, j]
      }
    }
  }
  
  return(cur.pts2)
}

#fill vals of bilinear interpolation into our data
cur.pts3 <- fill_vals(cur.pts2, bl3)

sum(is.na(cur.pts3)) 

(12045 / 1866975) * 100
#0.64%
# 58% was filled with bilinear interpolation :\

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

# Apply the function to each row and save the new values in cur.pts2
cur.pts4 <- t(apply(cur.pts3, 1, mean_5d))

#more munging
cur.pts4 <- as.data.frame(cur.pts4)
colnames(cur.pts4) <- colnames(cur.pts3)

sum(is.na(cur.pts4))

#the same as before, which means a NA row is all thats left


# add station name --------------------------------------------------------

pts <-  pts %>% mutate(RowNumber = row_number()) #make a row number 
cur.pts4 <-  cur.pts4 %>% mutate(RowNumber = row_number()) #make a row number 


cur.pts5 <- left_join(cur.pts4, pts %>% dplyr::select(RowNumber, Location), by = "RowNumber")

#re-order it
cur.pts5 <- cur.pts5 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(Location, everything())

head(cur.pts5)
# save --------------------------------------------------------------------

write_csv(cur.pts5, file = "Inputs/230911_capture_CUR.csv")

