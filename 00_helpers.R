#20.07.23
  #helpers


# packages ----------------------------------------------------------------

library(tidyverse)
library(lunar)
library(remora)
library(RColorBrewer)
library(lubridate)
library(ggpubr)
library(furrr)
library(viridis)
library(viridisLite)
library(scico)
library(sf)
library(tidyr)
library(stringr)
library(raster)
library(terra)
library(ggspatial)
#library(VTrack)
library(readxl)
library(data.table)
library(devtools)
library(geosphere)
library(ncdf4)
library(gstat)
library(sp)
library(mapview)
library(rvest)
library(curl)
library(parallel)
library(httr)
library(lme4)
library(car)
library(mgcv)
library(visreg)
library(patchwork)
library(gridExtra)
library(gamm4)
library(rnaturalearth)
#library(colorblindr)
library(MuMIn)
library(partykit)
library(bbmle)
library(data.table)
library(ggeffects)

# NC Wrestling ------------------------------------------------------------

# for this little function, df1 should be the df that you want to fill
# ie if I have NAs in df1 and df2 has values, it will put those values into df1
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

# mean 5 d for NAs --------------------------------------------------------

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

# 1 d nearest neighbour function ------------------------------------------

fill1dneighbour <- function(df) {
  new_df <- as.data.frame(t(apply(df, 1, function(row) { 
    for (j in 1:length(row)) {  
      if (is.na(row[j])) {  
        neighbors <- c()
        if (j > 1) {
          neighbors <- c(neighbors, row[j-1])
        }
        if (j < length(row)) {
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
  })))
  
  colnames(new_df) <- colnames(df)
  new_df <- as.data.frame(new_df)
  
  return(new_df)
}


# CIS for mixed models ----------------------------------------------------

# A function to build a plotting data frame for mixed-effects model (mod), data (d) and alpha
# Modified from https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html
# Written by Prof. Dave Schoeman
# August 2020
pltmm <- function(mod, d, alpha = 0.05) {
  m <- formula(mod,fixed.only = TRUE)[-2] # Fixed effects from mod
  # Build predition data frame
  mc <- as.character(m)[2] # Make the formula character, instead
  # Split out fixed effects terms, ignoring interactions
  fs <- unlist(strsplit(mc, " * "))[which(unlist(lapply(unlist(strsplit(mc, " * ")), nchar)) > 1)]
  if(length(grep(":", fs)) > 0) fs <- fs[-grep(":", fs)] 	
  fs <- gsub("\\(", "", gsub("\\)", "", fs)) # [DS] New line: gets rid of braces
  # Figure out which effects are factor and which are continuous, and fill, as necessary
  out <- list()
  for(i in 1:length(fs)) {
    if(eval(parse(text = paste0("with(d,is.factor(", fs[i], "))")))) {
      out[[fs[i]]] <- eval(parse(text = paste0("with(d, ", fs[i], " <- levels(", fs[i], "))")))
    } else {
      out[[fs[i]]] <- eval(parse(text = paste0("with(d, ", fs[i], " <- seq(min(", fs[i], "), max(", fs[i], "), length.out = 50))")))	
    }
  }
  p <- expand.grid(out) # Make preditor data frame
  mm <- model.matrix(m, p) # Model matrix for predictors
  beta <- fixef(mod) # Fixed-effects coefficients
  y <- mm %*% beta # Predicted values 
  V <- vcov(mod) # Variance-covariance matrix of beta
  pred.se <- sqrt(diag(mm %*% V %*% t(mm))) # Std errors of predictions
  linv <- family(mod)$linkinv # Extract the inverse-link function
  # Construct 95% Normal CIs on the link scale and transform back to the response (probability) scale
  crit <- -qnorm(alpha/2)
  fits <- data.frame(y = y, # Predicted value from model
                     se.hi =  y + pred.se,
                     se.lw =  y - pred.se,
                     upr = y + crit*pred.se, # Approx upper 95% conf limit for fit
                     lwr = y - crit*pred.se # Approx lower 95% conf limit for fit
  )
  Fits <- as.data.frame(lapply(fits, linv))
  return(cbind(p, Fits))
}


# pltmm1 ------------------------------------------------------------------

pltmm1 <- function(mod, d, fixed_effects, alpha = 0.05) {
  require(parallel)
  
  # Extract the formula and get fixed effects
  m <- formula(mod, fixed.only = TRUE)
  mc <- as.character(m)[2] # Make the formula character
  
  # Filter only specified fixed effects
  fs <- unlist(strsplit(mc, " * "))[unlist(strsplit(mc, " * ")) %in% fixed_effects]
  
  # Prepare data for parallel processing
  out <- list()
  for(i in 1:length(fs)) {
    if(eval(parse(text = paste0("with(d,is.factor(", fs[i], "))")))) {
      out[[fs[i]]] <- eval(parse(text = paste0("with(d, ", fs[i], " <- levels(", fs[i], "))")))
    } else {
      out[[fs[i]]] <- eval(parse(text = paste0("with(d, ", fs[i], " <- seq(min(", fs[i], "), max(", fs[i], "), length.out = 250))")))  
    }
  }
  
  p <- expand.grid(out)
  mm <- model.matrix(m, p)
  
  # Set up parallel backend to use specified number of cores
  no_cores <- detectCores() - 2
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("mm", "mod"))
  clusterEvalQ(cl, library(lme4))
  
  # Parallel computation for predictions
  beta <- fixef(mod)
  y <- parLapply(cl, split(mm, seq(nrow(mm))), function(x) x %*% beta)
  y <- unlist(y)
  
  V <- vcov(mod)
  pred.se <- parLapply(cl, split(mm, seq(nrow(mm))), function(x) sqrt(diag(x %*% V %*% t(x))))
  pred.se <- unlist(pred.se)
  
  stopCluster(cl)
  
  # Remaining calculations
  linv <- family(mod)$linkinv
  crit <- -qnorm(alpha/2)
  fits <- data.frame(y = y,
                     se.hi = y + pred.se,
                     se.lw = y - pred.se,
                     upr = y + crit*pred.se,
                     lwr = y - crit*pred.se
  )
  Fits <- as.data.frame(lapply(fits, linv))
  return(cbind(p, Fits))
}


