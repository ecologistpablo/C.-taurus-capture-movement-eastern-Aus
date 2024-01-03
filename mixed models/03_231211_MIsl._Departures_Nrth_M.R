#10.11.23
#keep on keepin on

# helpers -----------------------------------------------------------------


source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

rm(list=ls())

#bring and clean dat1a environment
setwd("~/University/2023/Honours/R/data")
dat <- read.csv("Inputs/231110_cleaned_pfuenzalida_dat.csv", stringsAsFactors = TRUE)

dat1 <- dat %>% 
  mutate(Location = factor(Location),
         Sex = factor(Sex),
         Tag_ID = factor(Tag_ID),
         Presence = factor(Presence)) %>% 
  filter(Location == "Moreton Island") %>% 
  filter(movement == "Departure") %>% 
  filter(Sex == "M") %>% 
  mutate(Tag_ID = as.factor(Tag_ID))

dat1 <-dat1 %>% 
  filter(Direction == "North")

unique(dat1$Tag_ID)
table(dat1$Tag_ID)
str(dat1)

# gamm --------------------------------------------------------------------

# Starting model with all four variables
m1 <- gamm4(Presence ~ s(SST_anomaly) + s(lunar.illumination) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

# Models with combinations of three variables
m2 <- gamm4(Presence ~ s(SST_anomaly) + s(lunar.illumination) + s(anomaly_VCUR),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m3 <- gamm4(Presence ~ s(SST_anomaly) + s(lunar.illumination) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m4 <- gamm4(Presence ~ s(SST_anomaly) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m5 <- gamm4(Presence ~ s(lunar.illumination) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

# two variables
m6 <- gamm4(Presence ~ s(SST_anomaly) + s(lunar.illumination),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m7 <- gamm4(Presence ~ s(SST_anomaly) + s(anomaly_VCUR),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m8 <- gamm4(Presence ~ s(SST_anomaly) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m9 <- gamm4(Presence ~ s(lunar.illumination) + s(anomaly_VCUR),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m10 <- gamm4(Presence ~ s(lunar.illumination) + s(anomaly_GSLA),
             random = ~(1|Tag_ID),
             data = dat1,
             family = binomial)

m11 <- gamm4(Presence ~ s(anomaly_VCUR) + s(anomaly_GSLA),
             random = ~(1|Tag_ID),
             data = dat1,
             family = binomial)

# single variables
m12 <- gamm4(Presence ~ s(SST_anomaly),
             random = ~(1|Tag_ID),
             data = dat1,
             family = binomial)

m13 <- gamm4(Presence ~ s(lunar.illumination),
             random = ~(1|Tag_ID),
             data = dat1,
             family = binomial)

m14 <- gamm4(Presence ~ s(anomaly_VCUR),
             random = ~(1|Tag_ID),
             data = dat1,
             family = binomial)

m15 <- gamm4(Presence ~ s(anomaly_GSLA),
             random = ~(1|Tag_ID),
             data = dat1,
             family = binomial)

# Null model
mnull <- glmer(Presence ~ 1 + (1|Tag_ID),
               data = dat1,
               family = binomial)

#first, are all estimated degrees of freedom linear? if so move to glmms
summary(m1$gam)

#edf = 1

#non linear models

# Using the mixed model components for AIC comparison
MuMIn::AICc(m1$mer, m2$mer, m3$mer, m4$mer, m5$mer, m6$mer,
       m7$mer, m8$mer, m9$mer, m10$mer, m11$mer,
       m12$mer, m13$mer, m14$mer, m15$mer, mnull)

summary(m6$gam)



# GLMM --------------------------------------------------------------------

m8 <- glmer(Presence ~ SST_anomaly +
              anomaly_GSLA +
              anomaly_VCUR +
              lunar.illumination + 
              (1|Tag_ID),
            data = dat1,  
            family = binomial)

m9 <- glmer(Presence ~ SST_anomaly +
              anomaly_GSLA +
              anomaly_VCUR +
              (1|Tag_ID),
            data = dat1,  
            family = binomial)

m10 <- glmer(Presence ~ SST_anomaly +
               anomaly_GSLA +
               (1|Tag_ID),
             data = dat1,  
             family = binomial)

m11 <- glmer(Presence ~ SST_anomaly +
               anomaly_GSLA +
               lunar.illumination +
               (1|Tag_ID),
             data = dat1,  
             family = binomial)

m12 <- glmer(Presence ~ SST_anomaly +
               anomaly_VCUR +
               lunar.illumination +
               (1|Tag_ID),
             data = dat1,  
             family = binomial)

m13 <- glmer(Presence ~ SST_anomaly +
               lunar.illumination +
               (1|Tag_ID),
             data = dat1,  
             family = binomial)

m14 <- glmer(Presence ~ SST_anomaly +
               (1|Tag_ID),
             data = dat1,  
             family = binomial)

m15 <- glmer(Presence ~ SST_anomaly +
               anomaly_GSLA +
               anomaly_VCUR +
               (1|Tag_ID),
             data = dat1,  
             family = binomial)


m16 <- glmer(Presence ~ SST_anomaly +
               anomaly_VCUR + 
               (1|Tag_ID),
             data = dat1,  
             family = binomial)

anova(m8, m9, m10, m11, m12, m13, m14, m15, m16, test = "Chi")
summary(m16)
#both VCUR and GSLA anomalies are statistically significant in two models
#both models hold same AIC value

