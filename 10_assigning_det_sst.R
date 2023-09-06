#06 September 2023
  #we have our sst vals: Both daily and monthly values
    #let's get sst values, and anomalies into our detection dataset

rm(list=ls())
#setwd("~/University/2023/Honours/R/data/git/NC-wrestling")

# Packages ----------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

sst <- read_csv("Inputs/SST_vals_12-22.csv")
m_avg <- read_csv("Inputs/SST_m_avrg_12-22_230906.csv")
det <- read_csv("Inputs/230816_step5.csv")
