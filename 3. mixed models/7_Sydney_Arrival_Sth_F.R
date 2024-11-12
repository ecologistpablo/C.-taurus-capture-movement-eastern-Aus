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
  filter(Location == "Sydney") %>% 
  filter(movement == "Arrival") %>% 
  filter(Sex == "F") %>% 
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
mnull <- gamm4(Presence ~ 1 + s(Tag_ID, bs = "re"), 
               data = dat1, 
               family = binomial)

#first, are all estimated degrees of freedom linear? if so move to glmms
summary(m15$gam)

# # Using the mixed model components for AIC comparison
# MuMIn::AICc(m1$mer, m2$mer, m3$mer, m4$mer, m5$mer, m6$mer,
#             m7$mer, m8$mer, m9$mer, m10$mer, m11$mer,
#             m12$mer, m13$mer, m14$mer, m15$mer, mnull$mer)


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


summary(m8)
#Using AICc, m8 is our minimum adequate model

# predict -----------------------------------------------------------------

# fitting mixed effects models with sometimes two interaction terms in gamm4 and glmer
# is harder than expected?!!!? (:O)
# luckily D. Schoeman knows what package can help
# ggeffects
# https://strengejacke.github.io/ggeffects/articles/practical_logisticmixedmodel.html

# for logistic mixed effects model w interaction terms
# Model contains splines or polynomial terms. Consider using terms="var_cont [all]" to get smooth plots.

SST <- ggpredict(m8, c("SST_anomaly[all]")) %>% plot() #var_contin (what you want), #varbinom (2nd var)
SST

GSLA <- ggpredict(m8, c("anomaly_GSLA[all]")) %>% plot() #var_contin (what you want), #varbinom (2nd var)
GSLA

#clean up x - y labels and breaks
SST1 <- SST + 
  theme_minimal() +
  labs(x = "Sea surface temperature (°C) temporal anomaly",
       y = "Predicted probability of arrival",
       title = "Female arrivals from north at Sydney (n = 51)") +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    limits = c(0, 1))+
  geom_line(size = 1) +
  theme(plot.background = element_rect(fill = "white"))
SST1

#clean up x - y labels and breaks
GSLA1 <- GSLA + 
  theme_minimal() +
  labs(x = "Temporal anomaly of gridded sea level anomaly",
       y = "Predicted probability of arrival",
       title = "Female arrivals from north at Sydney (n = 51)") +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    limits = c(0, 1))+
  geom_line(size = 1) +
  theme(plot.background = element_rect(fill = "white"))

GSLA1


p <- ggarrange(SST1, GSLA1)
p


#save
ggsave(path = "outputs/Graphs/Final/Models", "240814_Sydney_Arrival_Sth_F.pdf",
       plot = p, width = 12, height = 5) #in inches because gg weird

