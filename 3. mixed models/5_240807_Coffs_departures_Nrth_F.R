#07.08.24
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
  filter(Location == "Coffs Harbour") %>% 
  filter(movement == "Departure") %>% 
  filter(Sex == "F") %>% 
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
mnull <- gamm4(Presence ~ 1 + s(Tag_ID, bs = "re"), 
               data = dat1, 
               family = binomial)

#first, are all estimated degrees of freedom linear? if so move to glmms
summary(m15$gam)

#we have non linearity

# Using the mixed model components for AIC comparison
MuMIn::AICc(m1$mer, m2$mer, m3$mer, m4$mer, m5$mer, m6$mer,
            m7$mer, m8$mer, m9$mer, m10$mer, m11$mer,
            m12$mer, m13$mer, m14$mer, m15$mer, mnull$mer)

summary(m13$mer)
#Lunar illumination is a winner

# predict -----------------------------------------------------------------

# fitting mixed effects models with sometimes two interaction terms in gamm4 and glmer
# is harder than expected?!!!? (:O)
# luckily D. Schoeman knows what package can help
# ggeffects
# https://strengejacke.github.io/ggeffects/articles/practical_logisticmixedmodel.html

# for logistic mixed effects model w interaction terms
# Model contains splines or polynomial terms. Consider using terms="var_cont [all]" to get smooth plots.

moon <- ggpredict(m13, c("lunar.illumination[all]")) %>% plot() #var_contin (what you want), #varbinom (2nd var)
moon

#clean up x - y labels and breaks
moon1 <- moon + 
  theme_minimal() +
  labs(x = "Lunar Illumination (0 = new moon, 1 = full moon)",
       y = "Predicted probability of departure",
       title = "Female departures north at Coffs Harbour (n = 27)") +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    limits = c(0, 1))+
  #geom_smooth(size = 1, colour = black) +
  theme(plot.background = element_rect(fill = "white"))

moon1

#save
ggsave(path = "outputs/Graphs/Final/Models", "240807_CH_Male_Arrivals_Nrth.pdf",
       plot = moon1, width = 5, height = 5) #in inches because gg weird

