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
  filter(Location == "Flat Rock") %>% 
  filter(movement == "Departure") %>% 
  filter(Sex == "M") %>% 
  mutate(Tag_ID = as.factor(Tag_ID))

dat1 <-dat1 %>% 
  filter(Direction == "South")

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
summary(m15$gam)

#edf = 1

#all models linear, move to GLMMs

# GLMM --------------------------------------------------------------------

# Starting model with all four variables
m1 <- glmer(Presence ~ SST_anomaly + lunar.illumination + anomaly_VCUR + anomaly_GSLA + (1|Tag_ID),
            data = dat1,
            family = binomial)

# Models with combinations of three variables
m2 <- glmer(Presence ~ SST_anomaly + lunar.illumination + anomaly_VCUR + (1|Tag_ID),
            data = dat1,
            family = binomial)

m3 <- glmer(Presence ~ SST_anomaly + lunar.illumination + anomaly_GSLA + (1|Tag_ID),
            data = dat1,
            family = binomial)

m4 <- glmer(Presence ~ SST_anomaly + anomaly_VCUR + anomaly_GSLA + (1|Tag_ID),
            data = dat1,
            family = binomial)

m5 <- glmer(Presence ~ lunar.illumination + anomaly_VCUR + anomaly_GSLA + (1|Tag_ID),
            data = dat1,
            family = binomial)

# Models with two variables
m6 <- glmer(Presence ~ SST_anomaly + lunar.illumination + (1|Tag_ID),
            data = dat1,
            family = binomial)

m7 <- glmer(Presence ~ SST_anomaly + anomaly_VCUR + (1|Tag_ID),
            data = dat1,
            family = binomial)

m8 <- glmer(Presence ~ SST_anomaly + anomaly_GSLA + (1|Tag_ID),
            data = dat1,
            family = binomial)

m9 <- glmer(Presence ~ lunar.illumination + anomaly_VCUR + (1|Tag_ID),
            data = dat1,
            family = binomial)

m10 <- glmer(Presence ~ lunar.illumination + anomaly_GSLA + (1|Tag_ID),
             data = dat1,
             family = binomial)

m11 <- glmer(Presence ~ anomaly_VCUR + anomaly_GSLA + (1|Tag_ID),
             data = dat1,
             family = binomial)

# Models with a single variable
m12 <- glmer(Presence ~ SST_anomaly + (1|Tag_ID),
             data = dat1,
             family = binomial)

m13 <- glmer(Presence ~ lunar.illumination + (1|Tag_ID),
             data = dat1,
             family = binomial)

m14 <- glmer(Presence ~ anomaly_VCUR + (1|Tag_ID),
             data = dat1,
             family = binomial)

m15 <- glmer(Presence ~ anomaly_GSLA + (1|Tag_ID),
             data = dat1,
             family = binomial)

# Null model
mnull <- glmer(Presence ~ 1 + (1|Tag_ID),
               data = dat1,
               family = binomial)


MuMIn::AICc(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, mnull)

summary(m12)

# predict -----------------------------------------------------------------

#Are the SST NAs rm'ed ?
pdat <- expand.grid(
  SST_anomaly = seq(min(dat1$SST_anomaly, na.rm = T), max(dat1$SST_anomaly, na.rm = T), length.out = 100),
  Tag_ID = unique(dat1$Tag_ID, na.rm = T)
)

# Predict using the model
pdat$predicted_presence <- predict(m12, newdata = pdat, type = "response")


head(pdat)

# Plotting
p1 <- ggplot(pdat, aes(x = SST_anomaly, y = predicted_presence)) +
  geom_line(size = 2, colour = "firebrick") +
  labs(title = "Male departures at Flat Rock going south (n = 93)",
       x = "Sea surface temperature climatological anomaly",
       y = "Predicted probability of presence") +
  theme_grey()

p1


#save
ggsave(path = "Outputs/Graphs/Polishing/Models", "231220_FR_Departures_Sth_Males.png",
       plot = p1, width = 5, height = 5) #in inches because gg weird


