#19.07.23
  #data viz for all locations

# helpers -----------------------------------------------------------------

rm(list=ls())
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#bring and clean data environment
setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/230912a_complete_det_enviro.csv")


# munging -----------------------------------------------------------------


dat1 <- dat %>%
  filter(Presence == "1",
  Location %in% c("Wolf Rock", "Moreton Island", "Flat Rock",
                         "Coffs Harbour", "Hawks Nest", "Sydney",
                         "Jervis Bay")) %>%
  mutate(Location = factor(Location, levels = c("Wolf Rock", "Moreton Island",
                        "Flat Rock", "Coffs Harbour",
                        "Hawks Nest", "Sydney","Jervis Bay")))

# sst ---------------------------------------------------------------------

# Plot
sstanom <- 
  dat1 %>%
  ggplot(aes(x = Location, y = SST_anomaly, fill = Location)) +
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
facet_grid(movement~Sex)


print(sstanom)

#save
ggsave(path = "Outputs/Graphs/final/SST", "240125_SST_anomaly_pfuenzalida.pdf",
       plot = sstanom, width = 10, height = 7) #in inches because gg weird

# another juan ------------------------------------------------------------

#second plot
a2 <- 
  dat1 %>%
  ggplot(aes(x = Location, y = SST, fill = Location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.1) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.5) +
  scale_fill_viridis_d(direction = -1) +
  ylab("Sea Surface Temperature (°C)") +
  theme(legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 12) 
  ) + 
  facet_wrap(~Sex)


plot(a2)

ggsave(path = "Outputs/Graphs/Final/SST",
       "240125_SST_Violins.pdf", plot = a2, width = 10, height = 5) #in inches because gg weird

# distance vs movement ---------------------------------------------------------

#Distance v movement 
a3 <-
  dat1 %>% 
 ggplot(aes(x = Location, y = distance,
                 fill = Sex, colour = Sex)) +
  geom_boxplot(size = 0.5, colour = "black", alpha = 0.8) +
  labs(x = "Location", y = "Distance travelled (km)", fill = "Sex") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~movement) +
scale_fill_manual(values = c("firebrick2", "steelblue2")) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.y = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 12) )

plot(a3)

#Distance v direction
a4 <- 
  ggplot(dat1, aes(x = Location, y = distance, fill = Sex, colour = Sex)) +
  geom_boxplot(size = 0.5, colour = "black", alpha = 0.8) +
  labs(x = "Location", y = "Distance travelled (km)", fill = "Sex") +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Direction) +
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
  ggplot(aes(x = Location, y = anomaly_GSLA, #gsla, ucur, vcur, rs_current_velocity
             fill = Location)) +
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
  facet_grid(movement~Sex)
  
print(gsla)

#save
ggsave(path = "Outputs/Graphs/Final/Currents", "240125_GSLAA_facet.pdf",
       plot = gsla, width = 10, height = 7) #in inches because gg weird

# VCUR --------------------------------------------------------------------

# Plot
VCUR <-
  dat1 %>%
  ggplot(aes(x = Location, y = anomaly_VCUR, #gsla, ucur, vcur, rs_current_velocity
             fill = Location)) +
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
  facet_grid(movement~Sex)

print(VCUR)

ggsave(path = "Outputs/Graphs/Final/Currents",
       "240125_VCUR_facet.pdf",
       plot = VCUR, width = 10, height = 7) #in inches because gg weird


# Lunar Illumination ------------------------------------------------------

lunar <- 
  ggplot(data = dat1, aes(x = Location, y = lunar.illumination,
  fill = Location)) +
  geom_jitter(alpha = 0.3, width = 0.3) +
  geom_violin(width = 1, alpha = 0.3, scale = "width") +
  scale_fill_viridis_d(direction = -1) +
xlab("Location") +
  ylab("Lunar Illumination") +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          strip.text = element_text(size = 12)  
    ) +
    facet_grid(movement~Sex)

lunar

#save
ggsave(path = "Outputs/Graphs/Final/Lunar", "240125_lunar_facetgrid.pdf",
       plot = lunar, width = 12, height = 7) #in inches because gg weird

# vioplot -----------------------------------------------------------------

count <- dat %>%
  group_by(Location, Sex, movement) %>%
  summarise(Count = n(), .groups = 'drop')
print(count, n = 36)

