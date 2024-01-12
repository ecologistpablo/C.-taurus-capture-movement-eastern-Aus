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
  filter(Sex == "M")

dat1 <-dat1 %>% 
  filter(Direction == "South")

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
summary(m2$gam)

#we have a non linear model

# Using the mixed model components for AIC comparison
MuMIn::AICc(m1$mer, m2$mer, m3$mer, m4$mer, m5$mer, m6$mer,
            m7$mer, m8$mer, m9$mer, m10$mer, m11$mer,
            m12$mer, m13$mer, m14$mer, m15$mer, mnull$mer)

summary(m12$gam)

# predictive model --------------------------------------------------------

pdat <- expand.grid(
  SST_anomaly = seq(min(dat1$SST_anomaly, na.rm = T), max(dat1$SST_anomaly, na.rm = T), length.out = 100),
  Tag_ID = unique(dat1$Tag_ID))


preds <- predict(m12$gam, newdata = pdat, type = "response", se.fit = TRUE)

# Calculate confidence intervals
ci_lower <- preds$fit - 1.96 * preds$se.fit
ci_upper <- preds$fit + 1.96 * preds$se.fit

# Create a data frame for plotting
plot_data <- data.frame(SST_anomaly = pdat$SST_anomaly,
                        fitted_values = preds$fit,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper)

# Plotting
p1 <- ggplot(plot_data, aes(x = SST_anomaly, y = fitted_values)) +
  geom_line(size = 2, colour = "firebrick") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
  labs(title = "Extrinsic drivers of male arrivals at Coffs Harbour from the south (n = 57)",
       x = "Sea surface temperature (°C) climatological anomaly",
       y = "Predicted probability of arrivals") +
  theme_minimal()

p1


#save
ggsave(path = "Outputs/Graphs/Polishing/Models", "231222_CH_Arrivals_Sth_Males.png",
       plot = p1, width = 5, height = 5) #in inches because gg weird
