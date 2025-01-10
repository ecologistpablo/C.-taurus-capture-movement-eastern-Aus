#10.11.23
#keep on keepin on

# helpers -----------------------------------------------------------------


source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

rm(list=ls())
#bring and clean dat1a environment
setwd("~/University/2023/Honours/R/data")
dat <- read.csv("Inputs/240806_cleaned_model_dat.csv", stringsAsFactors = TRUE)

dat1 <- dat %>% 
  mutate(Location = factor(Location),
         Sex = factor(Sex),
         Tag_ID = factor(Tag_ID),
         Presence = factor(Presence)) %>% 
  filter(Location == "Moreton Island") %>% 
  filter(movement == "Arrival") %>% 
  filter(Sex == "F") %>% 
  filter(Direction == "South")

unique(dat1$Tag_ID) #not enough unique tags for this model
