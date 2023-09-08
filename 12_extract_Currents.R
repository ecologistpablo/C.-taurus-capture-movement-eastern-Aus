# 04 September 2023
# Who needs remora anyway
# Interpolating NAs in SST data

rm(list=ls())

setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

rstack <- rast("IMOS/Currents/Currents_12-22.tif")

rcs <- read_csv("Inputs/receiver_station_XY_230822.csv")
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

cur.pts <- extract(rstack, pts.UTM, ID = F) # ID = FALSE otherwise it creates a column with a number for each spoint


sum(is.na(cur.pts))
13020 * 127 #how many obs in total
(365083 / 1653540) * 100 

#22.07% is NA, wow nice

# nearest temporal neighbour ----------------------------------------------

cur.pts1 <- t(apply(cur.pts, 1, function(row) { # Apply a function to each row of 'sst.pts'.
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

(364560 / 1653540) * 100 
#22.04 now, not much :o

# increase coarseness -----------------------------------------------------

#dont think I want to increase coarseness of 20km

rstack #res at 0.02 by 0.02, 2km x 2km

rstack10km <- aggregate(rstack, fact = 5.5, #factor of whatever your resolution is in the OG raster / stack
                        fun = mean, na.rm = TRUE)

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


# 5 d mean ----------------------------------------------------------------

# Apply the function to each row and save the new values in sst.pts2
sst.pts10km2 <- t(apply(sst.pts10km1, 1, mean_5d))

#more munging
sst.pts10km2 <- as.data.frame(sst.pts10km2)
colnames(sst.pts10km2) <- colnames(sst.pts10km1)


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

(81301 / 491617) * 100
#16% of our data is still NA :(

44 - 16
#28 % is sampled at 10km spatial resolution

# rm NA rows --------------------------------------------------------------

#we plotted the spatial points of all rows that contain majority of NAs
#They're in Syd and Jervis bay in corners, can we remove them ?!


rcs <-  rcs %>% mutate(RowNumber = row_number()) #make a row number 
cur.pts1 <-  cur.pts1 %>% mutate(RowNumber = row_number()) #make a row number 

cur.pts1 <- left_join(cur.pts1, rcs %>% dplyr::select(RowNumber, station_name), by = "RowNumber")

cur.pts1 <- cur.pts1 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(station_name, everything())


# Remove rows that have more than 10 NA values
cur.pts2 <- cur.pts1[apply(cur.pts1, 1, function(x) sum(is.na(x)) <= 10), ]

sum(is.na(cur.pts2)) #0 obs out of 491,000 is pretty good


#to find out which rows we removed:
NA1 <- cur.pts1 
NA2 <- NA1[apply(NA1, 1, function(x) sum(is.na(x)) > 10), ] #bring only rows with + 10NAs 
NA3 <- NA2[1:1] #only keep RowNumber column

# save --------------------------------------------------------------------

write_csv(cur.pts2, file = "Inputs/230908_Currents_vals_12-22.csv")
write_csv(NA3, file = "Inputs/230908_Currents_NAs_12-22.csv")
