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
rcs <- read_csv("Inputs/230909_XY_receivers.csv")
UTM56S <- crs("EPSG:32756")# Coordinate reference systems

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
13020 * 114 #how many obs in total
(286930 / 1484280) * 100 

#19.33% is NA, wow nice

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

(286440 / 1484280) * 100 
#19.29 now, not much :o


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
blv <- extract(rstack, pts.UTM, method = "bilinear")
#bilinear returns values that are interpolated from the four nearest cells

blv1 <-  blv %>% rename(station_name = ID) #make a row number 

head(rstack)

# why is there duplicate rows ? -------------------------------------------

# Initialize an empty data frame
result <- data.frame()

# Loop through each point
for (i in 1:nrow(pts.UTM)) {
  point_values <- extract(rstack, pts.UTM[i,], method = "bilinear")
  
  # Check for duplicated column names within each point's extracted data
  if (any(duplicated(names(point_values)))) {
    print(paste("Duplicated columns found for point:", i))
  }
  
  # Append the results to the main data frame
  result <- rbind(result, point_values)
}




#join station name into cur.pts
cur.pts1 <- left_join(cur.pts1, rcs %>% dplyr::select(RowNumber, station_name), by = "RowNumber")

#re-order it
cur.pts1 <- cur.pts1 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(station_name, everything())

#join station name into cur.pts
blv <- blv(cur.pts1, rcs %>% dplyr::select(RowNumber, station_name), by = "RowNumber")

#re-order it
cur.pts1 <- cur.pts1 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(station_name, everything())


# where are the NAs ? -----------------------------------------------------

CG <- pts.UTM %>% 
  filter(station_name == "Cod Grounds")

# Extract x and y coordinates
x <- cg_coords[, 'X']
y <- cg_coords[, 'Y']

# Determine the extent around CG where you want to zoom in
xlims <- c(x - 1, x + 1)  # Change 1 to the desired distance in the x-direction
ylims <- c(y - 1, y + 1)  # Change 1 to the desired distance in the y-direction

# Plot the first layer of the raster stack
plot(rstack[[1]], xlim=xlims, ylim=ylims, axes=TRUE)

# Add the point to the plot
points(x, y, pch=19, col="red")

# rm NA rows --------------------------------------------------------------

#we plotted the spatial points of all rows that contain majority of NAs
#They're in Syd and Jervis bay in corners, can we remove them ?!


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
