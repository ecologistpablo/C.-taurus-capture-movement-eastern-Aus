#27.12.23
  #more stats on locations 

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#bring and clean data environment
rm(list=ls())
setwd("~/University/2023/Honours/R/data")
#dat <- read_csv("Inputs/230726_step8.csv")
dat <- read_csv("Inputs/230912a_complete_det_enviro.csv")

unique(dat$Location)

#dat munging -------------------------------------------------------------------

# Remove rows where Presence is equal to 0
dat1 <- dat[dat$Presence != 0, ]

dat2 <- dat1 %>% 
  filter(Location %in% c("Wolf Rock", "Moreton Island", "Flat Rock", "Coffs Harbour",
                         "Hawks Nest", "Sydney", "Jervis Bay"))

# WR ----------------------------------------------------------------------

WR <- dat2 %>% 
  filter(Location == "Wolf Rock")

summary(WR)
unique(WR$Tag_ID)
table(WR$Sex)


MR <- dat2 %>% 
  filter(Location == "Moreton Island")
summary(MR)
unique(MR$Tag_ID)
table(MR$Sex)


FR <- dat2 %>% 
  filter(Location == "Flat Rock")
summary(FR)
unique(FR$Tag_ID)
table(FR$Sex)

CH <- dat2 %>% 
  filter(Location == "Coffs Harbour")
summary(CH)
unique(CH$Tag_ID)
table(CH$Sex)


HN <- dat2 %>% 
  filter(Location == "Hawks Nest")
summary(HN)
unique(HN$Tag_ID)
table(HN$Sex)

SYD <- dat2 %>% 
  filter(Location == "Sydney")
summary(SYD)
unique(SYD$Tag_ID)
table(SYD$Sex)

JB <- dat2 %>% 
  filter(Location == "Jervis Bay")
summary(JB)
unique(JB$Tag_ID)
table(JB$Sex)

