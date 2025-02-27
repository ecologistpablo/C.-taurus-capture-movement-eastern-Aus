#10.11.23
#keep on keepin on

# helpers -----------------------------------------------------------------

source("~/Documents/USC/Honours/R/data/git/C.-taurus-capture-movement-eastern-Aus/00_helpers.R")
rm(list=ls())

setwd("~/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250212_det_enviro_complete.csv")

dat1 <- dat %>% 
  mutate(Location = factor(location),
         Sex = factor(sex),
         tag_id = factor(tag_id),
         Presence = factor(presence)) %>% 
  filter(Location == "Flat Rock") %>% 
  filter(movement == "Arrival") %>% 
  filter(sex == "M") %>% 
  filter(direction == "North")

#all from southern aggregation sites in our study so all heading north when arriving

unique(dat1$tag_id)  #ensure there is more than 3 tags
table(dat1$tag_id)
str(dat1)

# gamm --------------------------------------------------------------------

# Starting model with all four variables
m1 <- gamm4(Presence ~ s(sst_anomaly) + s(lunar.illumination) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|tag_id),
            data = dat1,
            family = binomial)

# Models with combinations of three variables
m2 <- gamm4(Presence ~ s(sst_anomaly) + s(lunar.illumination) + s(anomaly_VCUR),
            random = ~(1|tag_id),
            data = dat1,
            family = binomial)

m3 <- gamm4(Presence ~ s(sst_anomaly) + s(lunar.illumination) + s(anomaly_GSLA),
            random = ~(1|tag_id),
            data = dat1,
            family = binomial)

m4 <- gamm4(Presence ~ s(sst_anomaly) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|tag_id),
            data = dat1,
            family = binomial)

m5 <- gamm4(Presence ~ s(lunar.illumination) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|tag_id),
            data = dat1,
            family = binomial)

# two variables
m6 <- gamm4(Presence ~ s(sst_anomaly) + s(lunar.illumination),
            random = ~(1|tag_id),
            data = dat1,
            family = binomial)

m7 <- gamm4(Presence ~ s(sst_anomaly) + s(anomaly_VCUR),
            random = ~(1|tag_id),
            data = dat1,
            family = binomial)

m8 <- gamm4(Presence ~ s(sst_anomaly) + s(anomaly_GSLA),
            random = ~(1|tag_id),
            data = dat1,
            family = binomial)

m9 <- gamm4(Presence ~ s(lunar.illumination) + s(anomaly_VCUR),
            random = ~(1|tag_id),
            data = dat1,
            family = binomial)

m10 <- gamm4(Presence ~ s(lunar.illumination) + s(anomaly_GSLA),
             random = ~(1|tag_id),
             data = dat1,
             family = binomial)

m11 <- gamm4(Presence ~ s(anomaly_VCUR) + s(anomaly_GSLA),
             random = ~(1|tag_id),
             data = dat1,
             family = binomial)

# single variables
m12 <- gamm4(Presence ~ s(sst_anomaly),
             random = ~(1|tag_id),
             data = dat1,
             family = binomial)

m13 <- gamm4(Presence ~ s(lunar.illumination),
             random = ~(1|tag_id),
             data = dat1,
             family = binomial)

m14 <- gamm4(Presence ~ s(anomaly_VCUR),
             random = ~(1|tag_id),
             data = dat1,
             family = binomial)

m15 <- gamm4(Presence ~ s(anomaly_GSLA),
             random = ~(1|tag_id),
             data = dat1,
             family = binomial)

# Null model with random effect
mnull <- gamm4(Presence ~ 1,
               random = ~(1|tag_id),
               data = dat1,
               family = binomial)

#first, are all estimated degrees of freedom linear? if so move to glmms
summary(m1$gam)

#ay caramba, m1 has non-linearity in VCUR

# Using the mixed model components for AIC comparison
MuMIn::AICc(m1$mer, m2$mer, m3$mer, m4$mer, m5$mer, m6$mer,
       m7$mer, m8$mer, m9$mer, m10$mer, m11$mer,
       m12$mer, m13$mer, m14$mer, m15$mer, mnull$mer)

# m 4 wins ???
summary(m4$gam) #all significant


# predictive model --------------------------------------------------------

# fitting mixed effects models with sometimes two interaction terms in gamm4 and glmer
# is harder than expected?!!!? (:O)
# luckily D. Schoeman knows what package can help
# ggeffects
# https://strengejacke.github.io/ggeffects/articles/practical_logisticmixedmodel.html

# for logistic mixed effects model w interaction terms
# Model contains splines or polynomial terms. Consider using terms="var_cont [all]" to get smooth plots.

SST <- ggpredict(m4, c("sst_anomaly[all]")) %>% plot() #var_contin (what you want), #varbinom (2nd var)
SST #does it work?

#clean up x - y labels and breaks
SST1 <- SST + 
  theme_minimal() +
  labs(x = "Temporal anomaly of Sea Surface Temperature (°C)",
       y = "Predicted probability of arrival",
       title = "Male arrivals from south at Flat Rock (n = 177)") +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    limits = c(0, 1))+
  geom_line(size = 1)

SST1

#save
ggsave(path = "Outputs/Graphs/Final/Models", "250225_FR_Male_Arrival_Nrth.pdf",
       plot = SST1, width = 8, height = 5) #in inches because gg weird
  
