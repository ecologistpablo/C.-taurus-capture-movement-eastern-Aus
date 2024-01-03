#10 September 2023
  #we have our cur vals
    #lets generate monthly averages

rm(list=ls())
setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

dat <- read_csv("Inputs/230912_capture_CUR.csv")
pts <- read_csv("shark control/230910_XY_captures_12-22.csv")


# wrestle monthly averages ------------------------------------------------

calc_monthly_avg <- function(years, df, prefixes) {
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  
  all_results <- map_dfc(years, ~{
    year <- .x
    map_dfc(prefixes, ~{
      prefix <- .x
      map_dfc(months, ~{
        month <- .x
        pattern <- paste0(prefix, "_", year, month)
        cols <- grep(pattern, colnames(df))
        
        if (length(cols) == 0) {
          return(tibble())
        }
        
        avg_column <- rowMeans(df[, cols], na.rm = TRUE)
        new_col_name <- paste0(prefix, "_", year, month, "_avg")
        
        tibble(avg_column) %>% setNames(new_col_name)
      })
    })
  })
  
  return(all_results)
}


years <- 2012:2022
prefixes <- c("GSLA", "UCUR", "VCUR")
dat1 <- calc_monthly_avg(years, dat, prefixes)

# Add the station_name and the calculated averages
dat2 <- bind_cols(dat %>% dplyr::select(Location), dat1)

# View the first few rows
head(dat2)


# climatologies -----------------------------------------------------------

calc_climatology <- function(df, prefixes) {
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  
  # Initialize result dataframe with just the Location column
  result_df <- df %>% dplyr::select(Location)
  
  for (month in months) {
    
    # Initialize an empty character vector to collect relevant columns
    all_cols_for_month <- character(0)
    
    for (prefix in prefixes) {
      
      # Regex pattern to match prefix, any year, and the specific month
      pattern <- paste0(prefix, ".*", month, "_avg")
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

# Usage
dat3 <- calc_climatology(dat2, prefixes)

head(dat3)

# save --------------------------------------------------------------------

write_csv(dat3, file = "Inputs/230912_capture_CUR_m_avrg.csv")

