#10.11.23
#keep on keepin on

# helpers -----------------------------------------------------------------

rm(list=ls())

source("000_helpers.R")

setwd("~/University/2023/Honours/R")
dat <- read.csv("data/Inputs/231110_cleaned_pfuenzalida_dat.csv", stringsAsFactors = TRUE)

dat1 <- dat %>% 
  mutate(Location = factor(Location),
         Sex = factor(Sex),
         Tag_ID = factor(Tag_ID),
         Presence = factor(Presence)) %>% 
  filter(Location == "Wolf Rock") %>% 
  filter(movement == "Arrival") %>% 
  filter(Sex == "F") %>% 
  mutate(Tag_ID = as.factor(Tag_ID))

dat1 <-dat1 %>% 
  filter(Direction == "North")

#all from southern aggregation sites in our study so all heading north when arriving

unique(dat1$Tag_ID)
table(dat1$Tag_ID)
str(dat1)
#8 tags   

# gamm --------------------------------------------------------------------

# Starting model with all four variables
m1 <- gamm4(Presence ~ s(SST_anomaly) + s(lunar.illumination) + s(anomaly_VCUR) + s(anomaly_GSLA),
            random = ~(1|Tag_ID),data = dat1,
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

#is edf = 1 in all models?
summary(m5$gam)


#all models linear, move to GLMMs

# # Using the mixed model components for AIC comparison
# MuMIn::AICc(m1$mer, m2$mer, m3$mer, m4$mer, m5$mer, m6$mer,
#        m7$mer, m8$mer, m9$mer, m10$mer, m11$mer,
#        m12$mer, m13$mer, m14$mer, m15$mer, mnull)
# 
# summary(m6$gam)
# 

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


summary(m11)
#Using AICc, m11 is our minimum adequate model


# predict -----------------------------------------------------------------

pdat <- expand.grid(
  anomaly_GSLA = seq(min(dat1$anomaly_GSLA, na.rm = TRUE), max(dat1$anomaly_GSLA, na.rm = TRUE), length.out = 100),
  anomaly_VCUR = mean(dat1$anomaly_VCUR), #just need it 
  Tag_ID = unique(dat1$Tag_ID)
)

preds <- pltmm(m11, dat1)

# Sort the dataframe by 'anomaly_VCUR' to ensure lines are connected in the right order
preds <- preds[order(preds$anomaly_VCUR),]

# Plotting with confidence intervals
ggplot(preds, aes(x = anomaly_VCUR, y = y)) +
  geom_line()+
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(title = "Female arrivals at Wolf Rock from south (n = 39)",
       x = "North - south current direction climatological anomaly",
       y = "Predicted Probability of Presence") +
  theme_minimal()


# VCUR --------------------------------------------------------------------

new_data <- expand.grid(
  anomaly_VCUR = seq(min(dat1$anomaly_VCUR, na.rm = TRUE), max(dat1$anomaly_VCUR, na.rm = TRUE), length.out = 100)
)

# thank Dave 
pred_data <- pltmm(m11, new_data)

#plotting w preds
p2 <- ggplot(pred_data, aes(x = anomaly_VCUR, y = y)) +
  geom_line(colour = "firebrick4", size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(title = "Predicted Presence vs. current direction",
       x = "North - south current direction anomaly",
       y = "Predicted Probability of Presence") +
  theme_minimal()


p2
