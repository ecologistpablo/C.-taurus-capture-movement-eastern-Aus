# Using BlueLink OFAM3 to map the EAC inner edge and produce daily output - final method (Method 1)

source("/Empty edge dataframes.R")
source("/Packages + Coordinates.R")

# Load the BRAN (Bluelink) montly data
setwd() #setwd to where BRAN data is stored
temps <- dir()[grep("temp", dir())] #retrieve names of OFAM temp files
vs <- dir()[grep("v_", dir())]
us <- dir()[grep("u_", dir())]


for(i in 1:length(temps)) {   #monthly for loop
  
  #make monthly stacks of the variables, and crop them
  sst <- stack(temps[i]); v <- stack(vs[i]); u <- stack(us[i]); speed <- sqrt(u^2 + v^2)
  area <- extent(c(xmin=152.55,xmax=155.05,ymin=-28.45,ymax=-25.55)) #extent for Southeast Queensland 
  sst <- crop(sst, area); v <- crop(v, area); u <- crop(u, area); speed <- crop(speed, area)
  
  outputs <- list()
  inverselists <- list()
  
  for(k in 1:30) {    #daily for loop 
    
    #Create object for date of interest in Y-M-D combination
    yearmonth <- strsplit(temps[[i]], "ocean_temp_")
    yearmonth <- strsplit(yearmonth[[1]][2], ".nc")
    yearmonth <- unlist(yearmonth)    
    yearmonth <- gsub("_", "-", yearmonth)  
    yearmonth <- paste0(yearmonth, "-",  if (k < 10) { paste("0", k, sep = "") } else (paste0(k)))
    
    #This creates a matrix used for moving average calculations, calculates the longitudinal
    #gradient for each cell, calculates a 30 km moving average smooth. 
    f <- matrix(c(0, 1, 1),nrow=1,ncol=3,byrow = TRUE) 
    SSTgrad <- focal(sst[[k]],w=f,fun=function(x,...) x[3]-x[2], na.rm=T) 
    sstav <- focal(SSTgrad, w = matrix(1, ncol = 3, nrow = 1), fun=mean, na.rm = T)
    Vgrad <- focal(v[[k]],w=f,fun=function(x,...) x[3]-x[2], na.rm=T) 
    vav <- focal(Vgrad, w = matrix(1, ncol = 3, nrow = 1), fun=mean, na.rm = T) 
    SPEEDgrad <- focal(speed[[k]],w=f,fun=function(x,...) x[3]-x[2], na.rm=T) 
    speedav <- focal(SPEEDgrad, w = matrix(1, ncol = 3, nrow = 1), fun=mean, na.rm = T) 
    
    #The moving averages created pixels on land. Mask them out using original raw rasters. 
    speedav <- mask(speedav, stackApply(speed, nlayers(speed), mean, na.rm = TRUE))
    sstav <- mask(sstav, stackApply(sst, nlayers(sst), mean, na.rm = TRUE))
    vav <- mask(vav, stackApply(speed, nlayers(speed), mean, na.rm = TRUE))
    
    # Coerce rasters into vectors, for input to principal component's analysis. 
    sst.v <- sstav[]
    v.v <- vav[] * -1  #Raw V data is in radians. Inverse data so negative values are southwards current direction
    speed.v <- speedav[]
    # Extract coordinates
    xy <- data.frame(coordinates(sstav)) 
    # Stick them in a data frame
    dat <- data.frame(sst = sst.v, v = v.v, speed = speed.v, x = xy$x, y = xy$y)
    dat <- na.omit(dat) 
    
    # Run the PCA
    pca1 <- prcomp(dat[,-c(4:5)], retx = TRUE, center = FALSE, scale. = TRUE) 
    #scaled to mean and unit variance, did not centre as the value of 0 needs to be meaningful (ie. no gradient)
    
    #If loadings are different signs (ie. not all positive, or all negative), then write them to inverse dataframe.
    #If all loadings are negative, then invert.
    if (pca1$rotation["sst","PC1"] < 0 & pca1$rotation["v","PC1"] < 0 & pca1$rotation["speed","PC1"] < 0) {
      pc1 <- as.vector(pca1$x[,1]) *-1  #if all negative, then invert
    } else if (pca1$rotation["sst","PC1"] > 0 & pca1$rotation["v","PC1"] > 0 & pca1$rotation["speed","PC1"] > 0) { 
      pc1 <- as.vector(pca1$x[,1]) 
    } else {
      pc1 <- as.vector(pca1$x[,1])
      inverse[nrow(inverse) + 1,] <- c(yearmonth, pca1$rotation[1,1], pca1$rotation[2,1], pca1$rotation[3,1],
                                       pca1$rotation[1,2], pca1$rotation[2,2], pca1$rotation[3,2],
                                       pca1$rotation[1,3], pca1$rotation[2,3], pca1$rotation[3,3],
                                       round(pca1$sdev^2/sum(pca1$sdev^2)[1], 4)[1], #variance PC1
                                       round(pca1$sdev^2/sum(pca1$sdev^2)[1], 4)[2], #variance PC2
                                       round(pca1$sdev^2/sum(pca1$sdev^2)[1], 4)[3]) #variance PC3
    }
    
    #Add PCA information to this dataframe anyway
    df[nrow(df) + 1,] <- c(yearmonth, pca1$rotation["sst","PC1"], pca1$rotation["v","PC1"], pca1$rotation["speed","PC1"], #loadings PC1
                           pca1$rotation["sst","PC2"], pca1$rotation["v","PC2"], pca1$rotation["speed","PC2"], #loadings PC2
                           pca1$rotation["sst","PC3"], pca1$rotation["v","PC3"], pca1$rotation["speed","PC3"], #loadings PC3
                           round(pca1$sdev^2/sum(pca1$sdev^2)[1], 4)[1], 
                           round(pca1$sdev^2/sum(pca1$sdev^2)[1], 4)[2], 
                           round(pca1$sdev^2/sum(pca1$sdev^2)[1], 4)[3]) 
    
    pc1.map <- v[[k]] # Make a copy of a mappable raster
    pc1.map[as.numeric(row.names(dat))] <- pc1 # Place the values of PC1 into the raster with indices corresponding to original rows (before deleting NAs)
    pc1.map_copy <- pc1.map
    
    #Write variances to a dataframe, and indicate if signs are all the same direction, or different
    stuff <- data.frame(Date = yearmonth,
                        variance = round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1],
                        Direction = 
                          if(pca1$rotation[1,1] > 0 & pca1$rotation[2,1] > 0 & pca1$rotation[3,1] > 0 | 
                             pca1$rotation[1,1] < 0 & pca1$rotation[2,1] < 0 & pca1$rotation[3,1] < 0){"Same"} 
                        else {"Different"} )
    all.stuff <- rbind(all.stuff, stuff) #Each iteration of daily for loop, add a row
    
    ##### Mask thresholds 153.45 AND 154.45 
    
    #impose inner and outer threshold over entire PC to find the maximum value of PC1
    #this threshold is where I expect the edge to range between
    #Inner threshold = 153.34 longitude (East)
    #Outer threshold = 154.45 longitude (East)
    
    for (f in 1:nrow(pc1.map)) { #for each row in raster
      rows <- f
      values <- pc1.map[rows,] #gets actual pc1 values for every cell in the shark net row (these are combined max environmental gradients)
      longtest <- xFromCol(pc1.map, col = 1:ncol(pc1.map)) #gets corresponding longitudes 
      innerthreshold <- 153.45 
      outerthreshold <- 154.25 
      KILL <- which(longtest[]<innerthreshold | longtest[] > outerthreshold) #get indices of cells before/after my thresholds
      values[KILL] <- NA  #overwrite cells past thresholds to an NA
      pc1.map[f,] <- values #put these new values back into the mappable raster
    }
    
    #Make dataframe of new values and matching coordinates
    pc1_newvals <- pc1.map@data@values 
    pcdat <- setNames(data.frame(cbind(coordinates(pc1.map), pc1_newvals)), c("lon", "lat", "PC1"))
    lats <- unique(pcdat$lat) #get latitudes of BRAN raster
    close.lats <- vector() #Create empty vector to put shark net closest BRAN lats into
    
    #Find the closest BRAN latitude correspodning with the actual latitude of my shark net
    for(p in 1:length(latlons_nets$lat)){
      a <- latlons_nets$lat[p] # Get each longitude from the shark net sites
      b <- lats[which(abs(lats - a) == min(abs(lats-a)))] # Get the closest longitude in raster layer to BRAN
      close.lats[p] <- b
    }
    
    latlons_nets$close.lats <- close.lats # join with the original dataframe
    pcdat2 <- pcdat[pcdat$lat%in%latlons_nets$close.lats,] # keep only the longitudes that are present in my shark netnet dataframe
    # Now, let's split by longitude and find the maximum
    max.points <- data.frame(lon = numeric(), lat = numeric(), PC1 = numeric()) #build a frame
    
    ### Find maximum PC1 for each shark net 
    for(q in 1:nrow(latlons_nets)){
      it <- subset(pcdat2, lat == latlons_nets$close.lats[q])
      it <- na.omit(it)
      max.point <- it[it$PC1 == max(it$PC1),] #get the points where PC1 is maximum, omit NAs from threshold
      max.points[q,] <- max.point #insert into dataframe
    }
    
    names(max.points) <- c("max.lon", "net.lat", "PC1") # give adequate names
    #max.points <- merge(max.points, latlons_nets[,c("site", "close.lats", "lon")], by.x = "net.lat", by.y = "close.lats")
    max.points <- cbind(max.points, latlons_nets)
    
    names(max.points)[5] <- "net.lon"
    max.points$date <- yearmonth
    max.points <- max.points[,c("site", "date", "net.lat", "net.lon", "max.lon", "PC1")] #re-order everything
    
    #Compute distances between max PC1 gradient and shark net
    max.points$distedge <- NA 
    for (d in 1:length(max.points$site)) {
      edgedist <- distm(c(max.points$max.lon[d], max.points$net.lat[d]), c(max.points$net.lon[d], max.points$net.lat[d]))
      max.points$distedge[d] <- edgedist/1000
    }
    
    #crs(pc1.map_copy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    #png(paste0("SUBFOLDER WITHIN WORKING DIRECTORY", yearmonth,
    #           # if (yearmonth %in% inverse$date) {paste0("_FAIL")}, 
    #           # ".png"))
    par(las=1,cex.axis=0.8,mgp=c(0.3,0.9,0),mar=c(5,3,1.5,2))
    plot(pc1.map_copy, col = matlab.like(300),xaxt = "n", yaxt = "n")
    #axis(1, at = 152:155, cex.axis= 0.9, font.lab = 2)
    #axis(2, at = -25:-28, cex.axis = 0.9)
    plot(cropped, add = TRUE, lty = "dashed")
    points(max.points$max.lon, max.points$net.lat, pch = 16)
    abline(v = 153.45, col = "black", lwd = 2)
    map("worldHires",add=TRUE, fill = T, col = "grey")
    #mtext(paste0(yearmonth),
    #    side = 1, line = 4.9, padj = -9, adj = 0.09, cex = 1.2, font = 2)
    #mtext(paste0("V = ", round(pca1$sdev^2/sum(pca1$sdev^2)[1], 2)[1]), side = 1, 
    #   line = 3.0, padj = -9, adj=0.09, cex = 1.2, font = 2)
    abline(v = 154.25, col = "black", lwd = 2)
    box(lwd = 2)
    # dev.off()
    
    outputs[[k]] <- max.points  #write daily output to 'outputs'
    print(yearmonth)
  }
  
  all.month <- do.call("rbind", outputs)  
  all.months[[i]] <- all.month
  
}

#inverse = PC1 opposing signs 
length(unique(inverse$date)); nrow(inverse) #106 inversed loadings

#all.months is list of environmental + entanglement data
all.monthh <- do.call("rbind", all.months)
nrow(all.monthh) #14280 (3570 days in study period * 4 sites)