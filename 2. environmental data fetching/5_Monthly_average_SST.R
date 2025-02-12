#06 September 2023
  #we have our sst vals
    #lets generate monthly averages

rm(list=ls())
#setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

dat <- read_csv("Inputs/250211_SST_vals_12-22.csv")
#rcs <- read_csv("Inputs/230909_XY_receivers.csv")

# wrestle monthly averages ------------------------------------------------

calc_monthly_avg <- function(year, df) {
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  
  map_dfc(months, function(month) {
    pattern <- paste0("SST_", year, month)
    cols <- grep(pattern, colnames(df))
    
    avg_column <- rowMeans(df[, cols], na.rm = TRUE)
    
    new_col_name <- paste0("SST_", year, month)
    tibble(!!new_col_name := avg_column)
  })
}

# Assuming you have data from 2012 to 2022
m_avg <- map_dfc(2012:2022, ~ calc_monthly_avg(.x, dat))

# Combine the original data frame with the new columns
m_avg <- bind_cols(dat %>% dplyr::select(location), m_avg)

# View the first few rows of the final data
head(m_avg)

# 12 - 22 avrg ------------------------------------------------------------

calc_overall_monthly_mean <- function(df, prefixes) {
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  
  # Initialize result dataframe with just the station_name column
  result_df <- df %>% dplyr::select(location)
  
  for (month in months) {
    
    # Initialize an empty character vector to collect relevant columns
    all_cols_for_month <- character(0)
    
    for (prefix in prefixes) {
      
      # Regex pattern to match prefix, any year, and the specific month
      pattern <- paste0(prefix, "_", "....", month, "..")
      cols <- grep(pattern, colnames(df), value = TRUE)
      
      if (length(cols) == 0) {
        cat("No columns found for prefix:", prefix, " and month:", month, "\n")
        next
      }
      
      all_cols_for_month <- c(all_cols_for_month, cols)
    }
    
    # Skip to next iteration if no columns are found for the month
    if (length(all_cols_for_month) == 0) {
      cat("No columns found for month:", month, "\n")
      next
    }
    
    # Calculate mean across all relevant columns
    monthly_mean <- rowMeans(df[, all_cols_for_month, drop = FALSE], na.rm = TRUE)
    
    # Add the calculated mean to the result dataframe
    new_col_name <- paste0("MonthlyMean_", month)
    result_df <- result_df %>% dplyr::mutate(!!new_col_name := monthly_mean)
  }
  
  return(result_df)
}

# Make sure that "prefixes" is a vector, even if it contains only one element
prefixes <- c("SST")

# Usage
dat1 <- calc_overall_monthly_mean(dat, prefixes)

head(dat1)

# save --------------------------------------------------------------------

write_csv(dat1, file = "Inputs/250211_SST_m_avrg_12-22.csv")


