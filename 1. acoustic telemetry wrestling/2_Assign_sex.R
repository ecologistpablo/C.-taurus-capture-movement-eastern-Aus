#20th 10 2023
  #assigning sex to detections
    #if no sex is assigned, we cannot be certain it's carcharias taurus, and omit
      #important data curation step!

# load library & data ----------------------------------------------------------

rm(list=ls())
pacman::p_load("tidyverse")
setwd("/Users/owuss/Documents/USC/Honours/R/data")
IMOS <- read_csv("Inputs/250708_step1.csv")
dat <- read_csv("Inputs/250722_qc_data.csv")

dat$tag_id <- str_extract(dat$transmitter_id, "\\d{4,5}$") #shorten tag ID strings

dat <- dat %>% 
  dplyr::select(-transmitter_id) %>% 
  mutate(tag_id = as.double(tag_id))

IMOS1 <- bind_rows(IMOS,dat)

IMOS2 <- IMOS1 %>% 
  dplyr::distinct(tag_id, sex, datetime, longitude, latitude, station_name, receiver_name)
  

IMOS <- IMOS2

# munging -----------------------------------------------------------------

sex_vector <- c("27907" = "F", "29124" = "F", "29125" = "F", "29127" = "F", 
                "29128" = "F", "29132" = "M", "29138" = "M", "29139" = "M", 
                "29141" = "F", "29142" = "F", "29143" = "F", "29144" = "F", 
                "29145" = "M", "29146" = "M", "6388" = "M", "6392" = "M", 
                "6393" = "M", "6395" = "F", "6398" = "M", "6401" = "M", 
                "6402" = "M", "5762" = "F", "11133" = "F", "5763" = "F")

# Assign the "animal_sex"
IMOS$sex <- sex_vector[as.character(IMOS$tag_id)]

#If you have new data, find the tags that have no animal_sex assigned
unique(IMOS$tag_id[is.na(IMOS$sex)])

ID <- read_csv("Inputs/Tag_ID_sex_CKilpatrick_230605.csv") #3858 obs
ID1 <- read_csv("Inputs/GNS Transmitter ID data_20221220.csv")
ID2 <- read_csv("Inputs/IMOS_Tag_ID_230718.csv") %>% 
  rename(sex = animal_sex)
#Lets use match() to read ID, and find rows with corresponding Tag_IDs
IMOS$sex[is.na(IMOS$sex)] <- ID$Sex[match(IMOS$tag_id[is.na(IMOS$sex)], ID$`Tag ID`)]
IMOS$sex[is.na(IMOS$sex)] <- ID1$Sex[match(IMOS$tag_id[is.na(IMOS$sex)], ID1$`ID code`)]
IMOS$sex[is.na(IMOS$sex)] <- ID2$sex[match(IMOS$tag_id[is.na(IMOS$sex)], ID2$`tag_id`)]

anyNA(IMOS$sex) #do we have more work?
unique(IMOS$sex)

IMOS1 <- IMOS %>%
  mutate(sex = if_else(sex == "FEMALE", "F", sex))

unique(IMOS1$sex)

#remove all data that does not have a Sex assigned
IMOS1 %>% 
  distinct(tag_id, .keep_all = T) %>% 
  count(sex) #only a few tags have no sex assigned

# since we aren't able to verify they are C. taurus
# we remove NAs and unknowns, we want to be 100% sure

IMOS2 <- IMOS1 %>% 
  filter(!is.na(sex), #filter NAs
         !sex %in% c("U", "UNKNOWN", NA)) #filter U for unknowns

IMOS2 %>% 
  distinct(tag_id, .keep_all = T) %>% 
  count(sex) # 28 females, 25 males

# save --------------------------------------------------------------------
write_csv(IMOS2, "Inputs/250723_step2.csv")

# 1,696,261
