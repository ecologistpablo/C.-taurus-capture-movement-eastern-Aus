#23_10_20
  #assigning sex to detections
    #if no sex is assigned, we cannot be certain it's carcharias taurus, and omit
      #important data curation step!

# load library & data ----------------------------------------------------------

rm(list=ls())
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")
setwd("~/University/2023/Honours/R/data")
IMOS <- read_csv("Inputs/230906_step1.csv")


# munging -----------------------------------------------------------------

IMOS$Tag_ID <- str_extract(IMOS$transmitter_id, "\\d{4,5}$") #shorten tag ID strings

sex_vector <- c("27907" = "F", "29124" = "F", "29125" = "F", "29127" = "F", 
                "29128" = "F", "29132" = "M", "29138" = "M", "29139" = "M", 
                "29141" = "F", "29142" = "F", "29143" = "F", "29144" = "F", 
                "29145" = "M", "29146" = "M", "6388" = "M", "6392" = "M", 
                "6393" = "M", "6395" = "F", "6398" = "M", "6401" = "M", 
                "6402" = "M", "5762" = "F", "11133" = "F", "5763" = "F")

# Assign the "animal_sex"
IMOS$animal_sex <- sex_vector[as.character(IMOS$Tag_ID)]

#If you have new data, find the tags that have no animal_sex assigned
unique(IMOS$Tag_ID[is.na(IMOS$animal_sex)])

ID <- read_csv("Inputs/Tag_ID_sex_CKilpatrick_230605.csv") #3858 obs
ID1 <- read_csv("Dwyer dat/GNS Transmitter ID data_20221220.csv")
ID2 <- read_csv("Inputs/IMOS_Tag_ID_230718.csv")
#Lets use match() to read ID, and find rows with corresponding Tag_IDs
IMOS$animal_sex[is.na(IMOS$animal_sex)] <- ID$Sex[match(IMOS$Tag_ID[is.na(IMOS$animal_sex)], ID$`Tag ID`)]
IMOS$animal_sex[is.na(IMOS$animal_sex)] <- ID1$Sex[match(IMOS$Tag_ID[is.na(IMOS$animal_sex)], ID1$`ID code`)]
IMOS$animal_sex[is.na(IMOS$animal_sex)] <- ID2$animal_sex[match(IMOS$Tag_ID[is.na(IMOS$animal_sex)], ID2$`tag_id`)]

#do we have more work?
anyNA(IMOS$animal_sex)

unique(IMOS$animal_sex)


IMOS <- IMOS %>%
  mutate(animal_sex = if_else(animal_sex == "FEMALE", "F", animal_sex))

unique(IMOS$animal_sex)

#remove all data that does not have a Sex assigned
table(IMOS$animal_sex)
#only 12 / 16304 don't

IMOS <- IMOS %>% 
  filter(!is.na(animal_sex), #filter NAs
         animal_sex != "U") #filter U for unknowns

anyNA(IMOS$animal_sex) #this should be false

unique_sum <- IMOS %>%
  distinct(transmitter_id, animal_sex) %>%  # Keep unique TRANSMITTEDID values
  group_by(animal_sex) %>% #group by sex
  summarise(count = n())  # Count them

unique_sum #what's the split?

# save --------------------------------------------------------------------
write_csv(IMOS, "Inputs/240806_step2.csv")
