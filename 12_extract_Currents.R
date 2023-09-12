# 11 September 2023
  # P. Fuenzalida
    # extracting currrent data

rm(list=ls())

setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

rstack <- rast("IMOS/Currents/230912_cstack_12-22.tif")
rcs <- read_csv("Inputs/230909_XY_receivers.csv")
WGS84 <- crs("EPSG:4326")# Coordinate reference systems

pts.WGS <- st_as_sf(rcs, coords = c("receiver_deployment_longitude", #convert to an SF object
                                   "receiver_deployment_latitude")) 

st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS

# plotting ----------------------------------------------------------------

plot(rstack[[3]], col = viridis(255))
plot(pts.WGS, add = T)
 
# extract -----------------------------------------------------------------

cur.pts <- extract(rstack, pts.WGS, ID = F) # ID = FALSE otherwise it creates a column with a number for each spoint


sum(is.na(cur.pts))
114 * 12045
(265480 / 1373130) * 100 
#19.33% is NA

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

(264990 / 1373130) * 100 
#still 19


# add station_name --------------------------------------------------------

rcs <-  rcs %>% mutate(RowNumber = row_number()) #make a row number 
cur.pts1 <-  cur.pts1 %>% mutate(RowNumber = row_number()) #make a row number 

#join station name into cur.pts
cur.pts1 <- left_join(cur.pts1, rcs %>% dplyr::select(RowNumber, station_name), by = "RowNumber")

#re-order it
cur.pts1 <- cur.pts1 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(station_name, everything())

# bilinear interpolation --------------------------------------------------

# Extract using bilinear interpolation
bl <- extract(rstack, pts.WGS, method = "bilinear")
#bilinear returns values that are interpolated from the four nearest cells

bl1 <-  bl %>% rename(station_name = ID) #make a row number 

sum(is.na(cur.pts1)) 

(264990 / 1373130) * 100
#19%


# fill with bilinear interpolation ----------------------------------------

fill_na_with_bl1 <- function(cur.pts1, bl1) {
  # Check if dimensions match
  if (dim(cur.pts1)[1] != dim(bl1)[1] || dim(cur.pts1)[2] != dim(bl1)[2]) {
    stop("Dimensions of the two data frames must match.")
  }
  
  # Check if column names match
  if (!all(colnames(cur.pts1) == colnames(bl1))) {
    stop("Column names of the two data frames must match.")
  }
  
  # Replace NA values in cur.pts1 with corresponding values in bl1
  cur.pts1[is.na(cur.pts1)] <- bl1[is.na(cur.pts1)]
  
  return(cur.pts1)
}

# Usage
cur.pts2 <- fill_na_with_bl1(cur.pts1, bl1)

sum(is.na(cur.pts2))

(60229 / 1373130) * 100 
#from 17.85% to 4% :o

# # rm NA rows --------------------------------------------------------------
# 
# # Remove rows that have more than 10 NA values
# cur.pts3 <- cur.pts2[apply(cur.pts1, 1, function(x) sum(is.na(x)) <= 10), ]
# 
# sum(is.na(cur.pts3)) #0 obs out of 491,000 is pretty good
# 
# 
# #to find out which rows we removed:
# NA1 <- cur.pts2 
# NA2 <- NA1[apply(NA1, 1, function(x) sum(is.na(x)) > 10), ] #bring only rows with + 10NAs 
# NA3 <- NA2[1:1] #only keep RowNumber column
# 
# #just montague island 

# save --------------------------------------------------------------------

write_csv(cur.pts2, file = "Inputs/230912_Currents_vals_12-22.csv")
write_csv(NA3, file = "Inputs/230909_Currents_NAs_12-22.csv")
