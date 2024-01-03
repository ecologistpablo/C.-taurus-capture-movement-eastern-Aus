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
  filter(Location == "Coffs Harbour") %>% 
  filter(movement == "Arrival") %>% 
  filter(Sex == "M") %>% 
  mutate(Tag_ID = as.factor(Tag_ID))

dat1 <-dat1 %>% 
  filter(Direction == "North")

unique(dat1$Tag_ID)
table(dat1$Tag_ID)

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
summary(m5$gam)

#we have non linearity


# Using the mixed model components for AIC comparison
MuMIn::AICc(m1$mer, m2$mer, m3$mer, m4$mer, m5$mer, m6$mer,
       m7$mer, m8$mer, m9$mer, m10$mer, m11$mer,
       m12$mer, m13$mer, m14$mer, m15$mer, mnull)

summary(m12$mer)


# predict -----------------------------------------------------------------

#Are the SST NAs rm'ed ?
pdat <- expand.grid(
  SST_anomaly = seq(min(dat1$SST_anomaly, na.rm = T), max(dat1$SST_anomaly, na.rm = T), length.out = 100),
  Tag_ID = as.factor(unique(dat1$Tag_ID))
)

str(pdat)

# Predict using the model
pdat$predicted_presence <- predict(m12$gam, newdata = pdat, type = "response") #is it lm or am ?

head(pdat)

# Plotting
p1 <- ggplot(pdat, aes(x = SST_anomaly, y = predicted_presence)) +
  geom_line(size = 2, colour = "firebrick") +
  labs(title = "Male arrivals at Coffs Harbour going north (n = 78)",
       x = "Sea surface temperature climatological anomaly",
       y = "Predicted probability of presence") +
  theme_grey()

p1

#save
ggsave(path = "Outputs/Graphs/Polishing/Models", "231221_CH_Arrivals_Nrth_Male.png",
       plot = p1, width = 5, height = 5) #in inches because gg weird


