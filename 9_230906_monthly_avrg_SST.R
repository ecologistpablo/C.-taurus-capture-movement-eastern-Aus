#06 September 2023
  #we have our sst vals
    #lets generate monthly averages

rm(list=ls())
#setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

dat <- read_csv("Inputs/SST_vals_12-22.csv")
rcs <- read_csv("Inputs/receiver_station_XY_230822.csv")

# enter the ring ----------------------------------------------------------

rcs <-  rcs %>% mutate(RowNumber = row_number()) #make a row number 

# Join dat with rcs to add the station_name based on RowNumber
dat <- left_join(dat, rcs %>% dplyr::select(RowNumber, station_name), by = "RowNumber")

dat <- dat %>%
  dplyr::select(-RowNumber) %>% 
  dplyr::select(station_name, everything())

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
m_avg <- bind_cols(dat %>% dplyr::select(station_name), m_avg)

# View the first few rows of the final data
head(m_avg)

# save --------------------------------------------------------------------

write_csv(m_avg, file = "Inputs/230907_SST_m_avrg_12-22.csv")
