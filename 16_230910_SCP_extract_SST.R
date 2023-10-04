#10.09.23
  #paired SCP data
    #extract SST vals for it

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

pts <- read_csv("~/University/2023/Honours/R/data/shark control/231004_SCP_captures.csv")
WGS84 <- crs("EPSG:32756")# Coordinate reference systems

head(pts) #its all there

pts1 <- pts %>% 
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

str(pts1)
         
         
pts.WGS <- st_as_sf(pts1, coords = c("Longitude", #convert to an SF object
                                   "Latitude")) 

st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS


# SST stack ---------------------------------------------------------------

list.files("IMOS/SST")

SST <- rast("IMOS/SST/GHRSST_12-22.tif")

# plot --------------------------------------------------------------------

# Calculate the number of detections at each station
ptsxy <- pts %>%
  group_by(Latitude, Longitude) %>%
  summarise(num_det = n(), .groups = 'drop')

ptsxy_sf <- sf::st_as_sf(ptsxy, coords = c("Longitude", "Latitude"),
                          crs= 4326, agr = "constant")

mapview::mapview(ptsxy_sf, cex = "num_det", fbg = F)

#they're plotting well, but a few rows should maybe be removed 

# extract -----------------------------------------------------------------

sst.pts <- extract(SST, pts.WGS, ID = F)
# ID = FALSE otherwise it creates a column with a number for each point

sum(is.na(sst.pts))
283*3871

#100% NA :(

#no point don't 1 d or 5 d filling approaches with 100% NAs

# Bilinear interpolation --------------------------------------------------

bl <- extract(SST, pts.WGS, method = "bilinear") 

bl2 <- bl %>% 
  dplyr::select(-ID)

sum(is.na(bl2))
283 * 3871

(1086257 / 1095493) * 100
#99% NA, we have almost nothing

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
(1076158 / 1095493) * 100
#98% as NA

# resize coarseness -------------------------------------------------------

SST #res at 0.02 by 0.02, 2km x 2km

#0.02 = appox 2km, 0.0898 = approx 10km

# Resample the raster to the new resolution
SST10km <- aggregate(SST, fact = 4.49, #factor multiple original resolution
                        fun = mean, #mean 
                        na.rm = TRUE) #resize into land not sea = TRUE
#this is a 10km resize

plot(SST10km[[3]], col = viridis(255))

# resample ----------------------------------------------------------------

#direct resampling
sst.pts10km <- extract(SST10km, pts.WGS, ID = F) 

sum(is.na(sst.pts10km)) 
(757156 / 1095493) * 100
#69% NA

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
(456872 / 1095493) * 100
#41% :\ 

#5 d mean
sst.pts10km2 <- t(apply(sst.pts10km1, 1, mean_5d))

#more munging
sst.pts10km2 <- as.data.frame(sst.pts10km2)
colnames(sst.pts10km2) <- colnames(sst.pts10km1)

sum(is.na(sst.pts10km2))
(456814 / 1095493) * 100
#still 41%

#fill values of 10km res into our 2km res
sst.pts2 <- fill_vals(sst.pts1, sst.pts10km2)

sum(is.na(sst.pts2))

(54291 / 507101) * 100
#10% NA still. 


# bilinear interpolation of 10km grid -------------------------------------

bl10 <- extract(SST10km, pts.WGS, method = "bilinear") # ID = FALSE otherwise it creates a column with a number for each spoint

pts <-  pts %>% mutate(RowNumber = row_number()) #make a row number 
bl10 <-  bl10 %>% mutate(RowNumber = row_number()) #make a row number 

bl10a <- left_join(bl10, pts %>% dplyr::select(RowNumber, Location), by = "RowNumber")

#re-order it
bl10b <- bl10a %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(Location, everything())

bl10c <- bl10b %>% 
  dplyr::select(-ID, -Location)

# nearest temporal neighbour ----------------------------------------------

bl10d <- t(apply(bl10c, 1, function(row) { 
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

bl10d <- as.data.frame(bl10d)

sum(is.na(bl10d))

283*3871
(19534 / 1095493) * 100
# 1.78% :D

# 5 d mean ----------------------------------------------------------------

# Apply the function to each row 
bl10e <- t(apply(bl10d, 1, mean_5d))

bl10e <- as.data.frame(bl10e)
colnames(bl10e) <- colnames(bl10c)


colnames(bl10e)
# fill_gaps of bli into sst.pts -------------------------------------------

#fill values of bilinear interpolation at 10km into our df
sst.pts3 <- fill_vals(sst.pts2, bl10e)

sum(is.na(sst.pts3))
(3925 / 507101) * 100
#0.77 % :)

# add station name --------------------------------------------------------

sst.pts4 <-  sst.pts3 %>% mutate(RowNumber = row_number()) #make a row number 
pts <- pts %>% mutate(RowNumber = row_number()) #make a row number 

sst.pts4 <- left_join(sst.pts4, pts %>% dplyr::select(RowNumber, Location), by = "RowNumber")

#re-order it
sst.pts4 <- sst.pts4 %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(Location, everything())

head(names(sst.pts4))

# save --------------------------------------------------------------------

write_csv(sst.pts4, file = "Inputs/230912_capture_SST.csv")

