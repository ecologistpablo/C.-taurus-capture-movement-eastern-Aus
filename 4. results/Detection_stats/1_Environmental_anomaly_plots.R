#19.07.23
  #data viz for all locations

# helpers -----------------------------------------------------------------

library(tidyverse)
rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/260329_det_enviro_complete.rds")

# munging -----------------------------------------------------------------
location_levels <- c("Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                     "Gold Coast", "Ballina", "Evans Head", "Yamba", "Coffs Harbour",
                     "Port Macquarie", "Hawks Nest", "Sydney", "Illawarra")

#lvls <- c("Illawarra", "Sydney", "Hawks Nest", "Port Macquarie", "Coffs Harbour",
          "Yamba", "Evans Head", "Ballina", "Gold Coast", "North Stradbroke Island",
         "Sunshine Coast", "Wide Bay")

dat1 <- dat %>%
  filter(location %in% location_levels) %>%
  mutate(location = factor(location, levels = location_levels),
         sex = fct_recode(sex,
                     "Female" = "F",
                     "Male"   = "M"),
         location = recode(location, "Hawks Nest" = "Hunter"),
         loc_num = as.numeric(location, levels = location_levels)) # for a lm

  
str(dat1)


# sst ---------------------------------------------------------------------

# Plot
sst <- 
  ggplot(dat1, (aes(x = location, y = sst, fill = location)) +
  #geom_smooth(aes(group = sex), method = "lm", colour = "red", se = T, alpha = 0.3) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.1) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.7) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw()+ 
    labs(x = NULL, y = "Sea Surface Temperature (C°)")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  facet_wrap(~sex, ncol = 1)

sst

#save
ggsave(path = "Outputs/Graphs/final/detections", "260608_raw_sst.png",
       plot = sst, width = 12, height = 10) #in inches because gg weird


# anomalies ---------------------------------------------------------------



# Plot
a1 <- 
  ggplot(dat1, aes(x = sst_anomaly, y = location, fill = location)) +
  geom_vline(xintercept = 0, linetype = "longdash", colour = "red", alpha = 0.7) +
  geom_boxplot(width = 0.5, position = position_dodge(0.7), color = "black",
               alpha = 0.5) +
  scale_fill_viridis_d(direction = -1) +
  labs(x = "Sea Surface Temperature (°C) Anomaly", y = NULL, fill = "Focal Location")+
  theme_bw()+
  facet_wrap(~sex) +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "none")

a1


a2 <- 
  ggplot(dat1, aes(x = vcur_anomaly, y = location, fill = location)) +
  geom_vline(xintercept = 0, linetype = "longdash", colour = "red", alpha = 0.7) +
  geom_boxplot(width = 0.5, position = position_dodge(0.7), colour = "black",
               alpha = 0.5) +
  scale_fill_viridis_d(direction = -1) +
  labs(x = "Southward (negative) - Northward (positive) current anomaly", y = NULL)+
  theme_bw()+
  facet_wrap(~sex) +
  scale_y_discrete(limits = rev)+
  theme(axis.text.y = element_blank()) 
a2

a3 <- 
  ggplot(dat1, aes(x = ucur_anomaly, y = location, fill = location)) +
  geom_vline(xintercept = 0, linetype = "longdash", colour = "red", alpha = 0.7) +
  geom_boxplot(width = 0.5, position = position_dodge(0.7), colour = "black",
               alpha = 0.5) +
  scale_fill_viridis_d(direction = -1) +
  labs(x = "Eastward (negative) - Westward (positive) current anomaly", y = NULL)+
  theme_bw()+
  facet_wrap(~sex) +
  scale_y_discrete(limits = rev) +
  theme(legend.position = "none") 
a3

a4 <- 
  ggplot(dat1, aes(x = gsla_anomaly, y = location, fill = location)) +
  geom_vline(xintercept = 0, linetype = "longdash", colour = "red", alpha = 0.7) +
  geom_boxplot(width = 0.5, position = position_dodge(0.7), colour = "black",
               alpha = 0.5) +
  scale_fill_viridis_d(direction = -1) +
  labs(x = "Temporal anomaly of global sea level anomaly", y = NULL)+
  theme_bw()+
  facet_wrap(~sex) +
  scale_y_discrete(limits = rev)+
  theme(axis.text.y = element_blank()) 
a4


# combo -------------------------------------------------------------------

anomalies <- ggpubr::ggarrange(a1, a2, a3, a4,
                       nrow = 2, ncol = 2,
                       widths = c(1, 1.1, 1, 1.1))
plot(anomalies)

#save
ggsave(path = "Outputs/Graphs/final/detections", "260608_movement_anomaly.pdf",
       plot = anomalies, width = 12, height = 12) #in inches because gg weird

# Lunar Illumination ------------------------------------------------------


lunar <- 
  ggplot(data = dat1, aes(x = lunar.illumination, y = location, fill = location)) +
  
  geom_point(alpha = 1) +
  theme_bw()+
  geom_violin(width = 0.8, alpha = 0.3, scale = "width") +
  scale_fill_viridis_d(direction = -1) +
  labs(x = "Lunar Illumination", y = NULL, fill = "Focal Locations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(~ sex)+
  scale_y_discrete(limits = rev) +
  

lunar

#save
ggsave(path = "outputs/Graphs/Final/detections", "260608_lunar_facets.png",
       plot = lunar, width = 8, height = 8) # in inches because gg weird


