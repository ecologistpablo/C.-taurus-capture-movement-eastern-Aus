#19.07.23
  #data viz for all locations

# helpers -----------------------------------------------------------------

rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250827_det_enviro_complete.csv")

# munging -----------------------------------------------------------------
location_levels <- c("Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                     "Gold Coast", "Ballina", "Evans Head", "Coffs Harbour",
                     "Port Macquarie", "Hawks Nest", "Sydney", "Illawarra")

library(forcats)

dat1 <- dat %>%
  filter(presence == "1",
         location %in% location_levels) %>%
  mutate(location = factor(location, levels = location_levels),
         sex = fct_recode(sex,
                     "Female" = "F",
                     "Male"   = "M") )

  
str(dat1)
  select(-station_name)


# sst ---------------------------------------------------------------------

# Plot
sst <- 
  dat1 %>%
  ggplot(aes(x = location, y = sst, fill = location)) +
  #geom_smooth(aes(group = sex), method = "lm", color = "red", se = FALSE) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.1) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.7) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw()+ 
    labs(x = NULL, y = "Sea Surface Temperature (C°)")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 12) ) +
  facet_wrap(~sex, ncol = 1)

sst

#save
ggsave(path = "Outputs/Graphs/final/SST", "250903_raw_sst.pdf",
       plot = sst, width = 12, height = 10) #in inches because gg weird

# Plot
sstanom <- 
  dat1 %>%
  ggplot(aes(x = location, y = sst_anomaly, fill = location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.1) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.7) +
  scale_fill_viridis_d(direction = -1) +
  xlab("Location") +
  ylab("Sea Surface Temperature (°C) Anomaly ") +
  theme(legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 12) ) +
facet_wrap(~sex)


print(sstanom)

#save
ggsave(path = "Outputs/Graphs/final/SST", "250701_SST_anomaly.pdf",
       plot = sstanom, width = 10, height = 7) #in inches because gg weird

# distance vs movement ---------------------------------------------------------

#Distance v movement 
a3 <-
  dat1 %>% 
 ggplot(aes(x = location, y = distance, fill = location)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.6)+
  labs(x = "Location", y = "Distance travelled (km)", fill = "Sex") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(movement~sex) +
  scale_fill_viridis_d(direction = -1)+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 12) )

a3

#save
ggsave(path = "Outputs/Graphs/final/detection", "250225_dist-sex-movement_grid.pdf",
       plot = a3, width = 10, height = 7) #in inches because gg weird

#Distance v direction
a4 <- 
  ggplot(dat1, aes(x = location, y = distance, fill = sex, colour = sex)) +
  geom_boxplot(size = 0.5, colour = "black", alpha = 0.8) +
  labs(x = "Location", y = "Distance travelled (km)", fill = "Sex") +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~direction) +
  scale_fill_manual(values = c("firebrick2", "steelblue2"))+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 12) )

plot(a4)

final <- ggarrange(a3, a4, ncol = 1, common.legend = T, legend = "right")
final

ggsave(path = "Outputs/Graphs/Final", "240125_dist_direction_combo.pdf",plot = final, width = 10, height = 12) #in inches because gg weird

# currents ----------------------------------------------------------------

# Plot
gsla <-
  dat1 %>%
  ggplot(aes(x = location, y = anomaly_GSLA, #gsla, ucur, vcur, rs_current_velocity
             fill = location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.1) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.5) +
  scale_fill_viridis_d(direction = -1) +
  xlab("Location") +
  ylab("Temporal anomaly of gridded sea level anomaly") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 12)) +
  facet_grid(movement~sex)
  
gsla

#save
ggsave(path = "Outputs/Graphs/Final/Currents", "250225_GSLAA_facet.pdf",
       plot = gsla, width = 10, height = 7) #in inches because gg weird

# VCUR --------------------------------------------------------------------

# Plot
VCUR <-
  dat1 %>%
  ggplot(aes(x = location, y = anomaly_VCUR, #gsla, ucur, vcur, rs_current_velocity
             fill = location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.1) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.5) +
  scale_fill_viridis_d(direction = -1) +
  xlab("Location") +
  ylab("South - North current direction climatological anomaly") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 12) 
  ) +
  facet_grid(movement~sex)

print(VCUR)

ggsave(path = "Outputs/Graphs/Final/Currents",
       "250225_VCUR_facet.pdf",
       plot = VCUR, width = 10, height = 7) #in inches because gg weird


# Lunar Illumination ------------------------------------------------------

cp <- colorRampPalette(c("black", "beige"))(n = 1) #colour palette that matches the lunar cycle
str(dat1$lunar.illumination)
summary(dat1$lunar.illumination)
lunar <- 
  ggplot(data = dat1, aes(x = location, y = lunar.illumination,
  fill = location)) +
  geom_jitter(alpha = 0.7, width = 0.3) +
  geom_violin(width = 1, alpha = 0.3, scale = "width") +
  scale_fill_viridis_d(direction = -1) +
  labs(x = "Location", y = "Lunar Illumination") +
  theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          strip.text = element_text(size = 12)  
    ) +
    facet_grid(movement~sex)

lunar

#save
ggsave(path = "Outputs/Graphs/Final/Lunar", "250225_lunar_facetgrid.pdf",
       plot = lunar, width = 12, height = 7) #in inches because gg weird


