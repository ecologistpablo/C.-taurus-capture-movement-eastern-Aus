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
  filter(movement == "Arrival") %>% 
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
mnull <- gamm4(Presence ~ 1 + s(Tag_ID, bs = "re"), 
               data = dat1, 
               family = binomial)

#first, are all estimated degrees of freedom linear? if so move to glmms
summary(m8$gam)

#we have a non linear model

# Using the mixed model components for AIC comparison
MuMIn::AICc(m1$mer, m2$mer, m3$mer, m4$mer, m5$mer, m6$mer,
       m7$mer, m8$mer, m9$mer, m10$mer, m11$mer,
       m12$mer, m13$mer, m14$mer, m15$mer, mnull$mer)

summary(m8$gam)


# predictive model --------------------------------------------------------

# fitting mixed effects models with sometimes two interaction terms in gamm4 and glmer
# is harder than expected?!!!? (:O)
# luckily D. Schoeman knows what package can help
# ggeffects
# https://strengejacke.github.io/ggeffects/articles/practical_logisticmixedmodel.html

# for logistic mixed effects model w interaction terms
# Model contains splines or polynomial terms. Consider using terms="var_cont [all]" to get smooth plots.

SST <- ggpredict(m8, c("SST_anomaly[all]")) %>% plot() #var_contin (what you want), #varbinom (2nd var)
SST #does it work?
GSLA <- ggpredict(m8, c("anomaly_GSLA[all]")) %>% plot() #var_contin (what you want), #varbinom (2nd var)
GSLA

#clean up x - y labels and breaks
SST1 <- SST + 
  theme_minimal() +
  labs(x = "Sea surface temperature (⁰C) temporal anomaly",
       y = "Predicted probability of arrival",
       title = "Male arrivals from south at Flat Rock (n = 126)") +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    limits = c(0, 1))+
  geom_line(size = 1)

SST1

#clean up x - y labels and breaks
GSLA1 <- GSLA + 
  theme_minimal() +
  labs(x = "Temporal anomaly of gridded sea level anomaly",
       y = "Predicted probability of arrival",
       title = "Male arrivals from south at Flat Rock (n = 126)") + 
  scale_y_continuous( breaks = c(0, 0.25, 0.5, 0.75, 1),
                      labels = c("0%", "25%", "50%", "75%", "100%"),
                      limits = c(0, 1)) +
  geom_line(size = 1)
GSLA1

#ggcombine that up bby
p <- ggarrange(GSLA1 + SST1) #ncol / nrow = 1 to specify if u want 1 row or column
p

#save
ggsave(path = "Outputs/Graphs/Polishing/Models", "240123_FR_Male_Arrival_Nrth.pdf",
       plot = p, width = 10, height = 5) #in inches because gg weird
  
