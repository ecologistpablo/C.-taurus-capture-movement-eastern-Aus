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
         Tag_ID = factor(tag_id),
         Presence = factor(presence)) %>% 
  filter(Location == "Wolf Rock") %>% 
  filter(movement == "Arrival") %>% 
  filter(sex == "F") %>% 
  filter(direction == "North")

#all from southern aggregation sites in our study so all heading north when arriving

unique(dat1$tag_id)  #ensure there is more than 3 tags
table(dat1$tag_id)
str(dat1)
#8 tags   

# gamm --------------------------------------------------------------------

# Starting model with all four variables
m1 <- gamm4(Presence ~ s(sst_anomaly) + s(lunar.illumination) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),data = dat1,
            family = binomial)

# Models with combinations of three variables
m2 <- gamm4(Presence ~ s(sst_anomaly) + s(lunar.illumination) + s(anomaly_VCUR),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m3 <- gamm4(Presence ~ s(sst_anomaly) + s(lunar.illumination) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m4 <- gamm4(Presence ~ s(sst_anomaly) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m5 <- gamm4(Presence ~ s(lunar.illumination) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

# two variables
m6 <- gamm4(Presence ~ s(sst_anomaly) + s(lunar.illumination),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m7 <- gamm4(Presence ~ s(sst_anomaly) + s(anomaly_VCUR),
            random = ~(1|Tag_ID),
            data = dat1,
            family = binomial)

m8 <- gamm4(Presence ~ s(sst_anomaly) + s(anomaly_GSLA),
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
m12 <- gamm4(Presence ~ s(sst_anomaly),
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

#is edf = 1 in all models?
summary(m15$gam)


#all models linear, move to GLMMs

# # # Using the mixed model components for AIC comparison
# MuMIn::AICc(m1$mer, m2$mer, m3$mer, m4$mer, m5$mer, m6$mer,
#        m7$mer, m8$mer, m9$mer, m10$mer, m11$mer,
#        m12$mer, m13$mer, m14$mer, m15$mer, mnull$mer)


# GLMM --------------------------------------------------------------------

# Starting model with all four variables
m1 <- glmer(Presence ~ sst_anomaly + lunar.illumination + anomaly_VCUR + anomaly_GSLA + (1|Tag_ID),
            data = dat1,
            family = binomial)

# Models with combinations of three variables
m2 <- glmer(Presence ~ sst_anomaly + lunar.illumination + anomaly_VCUR + (1|Tag_ID),
            data = dat1,
            family = binomial)

m3 <- glmer(Presence ~ sst_anomaly + lunar.illumination + anomaly_GSLA + (1|Tag_ID),
            data = dat1,
            family = binomial)

m4 <- glmer(Presence ~ sst_anomaly + anomaly_VCUR + anomaly_GSLA + (1|Tag_ID),
            data = dat1,
            family = binomial)

m5 <- glmer(Presence ~ lunar.illumination + anomaly_VCUR + anomaly_GSLA + (1|Tag_ID),
            data = dat1,
            family = binomial)

# Models with two variables
m6 <- glmer(Presence ~ sst_anomaly + lunar.illumination + (1|Tag_ID),
            data = dat1,
            family = binomial)

m7 <- glmer(Presence ~ sst_anomaly + anomaly_VCUR + (1|Tag_ID),
            data = dat1,
            family = binomial)

m8 <- glmer(Presence ~ sst_anomaly + anomaly_GSLA + (1|Tag_ID),
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
m12 <- glmer(Presence ~ sst_anomaly + (1|Tag_ID),
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


summary(m15)
#Using AICc, m15 is our minimum adequate model


# predict -----------------------------------------------------------------

# fitting mixed effects models with sometimes two interaction terms in gamm4 and glmer
# is harder than expected?!!!? (:O)
# luckily D. Schoeman knows what package can help
# ggeffects
# https://strengejacke.github.io/ggeffects/articles/practical_logisticmixedmodel.html

# for logistic mixed effects model w interaction terms
# Model contains splines or polynomial terms. Consider using terms="var_cont [all]" to get smooth plots.


GSLA <- ggeffects::ggpredict(m15, c("anomaly_GSLA[all]")) %>% plot() #var_contin (what you want), #varbinom (2nd var)
GSLA

#clean up x - y labels and breaks
GSLA1 <- GSLA + 
  theme_minimal() +
  labs(x = "Temporal anomaly of gridded sea level anomaly",
       y = "Predicted probability of arrival",
       title = "Female arrivals from south at Wolf Rock (n = 42)") + 
  scale_y_continuous( breaks = c(0, 0.25, 0.5, 0.75, 1),
                      labels = c("0%", "25%", "50%", "75%", "100%"),
                      limits = c(0, 1)) +
  geom_line(size = 1)
GSLA1


#save
ggsave(path = "outputs/Graphs/Final/Models", "250226_WR_Female_Arrival_Nrth.pdf",
       plot = GSLA1, width = 5, height = 5) #in inches because gg weird

