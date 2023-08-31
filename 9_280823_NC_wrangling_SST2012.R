#NC stacking
  #Lets practice w one year, then automate it
    #27.08.23

#wd <-
setwd("~/University/2023/Honours/R/data/IMOS/SST/2012")
setwd("E:/Pablo/2023_hons_dat")

#r_list <- dir(wd, pattern = "*")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")


# begin munging -----------------------------------------------------------

plan(multisession, workers = 6) # Use 6 cores

# List all the .nc files in the directory
file_list <- list.files(pattern = "*.nc")

# Use future_map to read SpatRasters in parallel
r_list <- future_map(file_list, ~rast(.x))

# Combine the rasters into a single SpatRaster
rstack <- do.call(c, r_list)

# Select only layers named "sea_surface_temperature_day_night"
head(names(rstack))

rstack1 <- subset(rstack, 2:2)

head(names(rstack1)) #did it work ?

# Assign new names with an index
names(rstack1) <- paste("sea_surface_temperature_day_night", 1:length(rstack1), sep="_")

# renaming ----------------------------------------------------------------

# Create a sequence of dates for 2012
date_seq <- seq(from = as.Date("2012-01-01"), to = as.Date("2012-12-31"), by = "1 day")

# Generate new names based on the date sequence
new_names <- sapply(date_seq, function(date) {
  date_str <- format(date, "%Y%m%d")
  paste0("SST_", date_str)
})

# Rename the layers in your SpatRaster object
if(length(new_names) == nlyr(rstack1)) {
  names(rstack1) <- new_names
} else {
  stop("The number of names does not match the number of layers in the SpatRaster.")
}

head(names(rstack1))


# crop --------------------------------------------------------------------

# Define the extent
e <- ext(c(150, 155, -36, -24)) #xmin, xmax, ymin, ymax

# Crop the stack
rstack2 <- terra::crop(rstack1, e)

plot(rstack2)

# more munging ------------------------------------------------------------

# Convert from Kelvin to Celsius
rstack3 <- app(rstack2, function(x) x - 273.15)

# Check if it worked
plot(rstack3)

names(rstack3)


# save progress -----------------------------------------------------------

writeRaster(rstack3, "SST_stack_2012.tif")

# resample our coarseness -------------------------------------------------

#set CRS 
UTM56S <- crs("EPSG:32756")
lonlat <- crs("EPSG:4326") # This is the same as coding lonlat <- CRS("EPSG:4326")

rstack4 <- project(rstack3, UTM56S)
stack4 #nice

setwd("~/University/2023/Honours/R/data/Inputs")

XY <- read_csv("receiver_station_XY_230822.csv")

# Convert pts from a data frame into an sf object, using x and y as coordinates
XY.sp <- st_as_sf(XY, coords = c("receiver_deployment_longitude", "receiver_deployment_latitude")) 

# Make sure that we assign the lon-lat CRS so that R knows how this object is projected
st_crs(XY.sp) <- crs(lonlat) 
XY.sp #nice

XY.UTM <- st_transform(XY.sp, UTM56S)
XY.UTM #now its in UTM the same as our environmental data

# increasing coarseness ---------------------------------------------------



#our resolution is 2175 x 2175 which is pretty fine, we could try sampling with that
#or make it more coarse


res.new <- c(10000, 10000) #new 5km x 5km resolution grid
newgrid <- rast(ext = ext(rstack4), crs = UTM56S, res = res.new)
rstack5 <- resample(rstack4, newgrid)
rstack5
plot(rstack5[[19]])

#a bit bulkier but we will try and work with this to see what it produces


# plot vals --------------------------------------------------------------

#to check we're doing okay, let's plot something
plot(rstack4[[19]], col = viridis(255))
plot(XY.UTM, add = T)

#whelp they're there, but the fucking clouds are in our way



# Buffer size for filling gaps (e.g., 5000 meters)
buffer_5km <- 5000

# Extract values using the original resolution
rstack_pts <- extract(rstack4, XY.UTM, ID = F) 

# Extract median values using a buffer to fill gaps
rstack_buff <- extract(rstack4, XY.UTM, buffer = buffer_5km, fun = median, ID = F)

# Replace NA values in the original extraction with the buffered median values
rstackf <- ifelse(is.na(rstack_pts), rstack_buff, rstack_pts)

sum(sapply(rstackf, function(x) sum(is.na(x))))
#4217416


sum(is.na(rstack.pts))
# [1] 33208
sum(is.na(rstackf1))
#4217416

# extract vals ------------------------------------------------------------




rstack.pts_15km <- extract(rstack5, XY.UTM, ID = F) 
# ID = FALSE otherwise it creates a column with a number for each point

sum(is.na(rstack.pts_15km))


# > sum(is.na(rstack.pts_2km))
# [1] 33208
# > sum(is.na(rstack.pts_5km))
# [1] 30764
# > sum(is.na(rstack.pts_10km))
# [1] 27776
# > sum(is.na(rstack.pts_15km))
# [1] 36200


127*366 #observations x number of days
#[1] 46482

#total number of observations - NAs = total num. of values you have
46482 - 33208 
#13274 for 2km
46482 - 30764
#15718 for 5km
46482 - 27776
#18706 for 10km
46482 - 36200
#10282 for 15km

#if we go with 10km spatial resolution, approx. 1 in 4 values will be an NA
#Which is no beuno 