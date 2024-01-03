#19.07.23
  #data viz for all locations

# helpers -----------------------------------------------------------------

rm(list=ls())
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#bring and clean data environment
setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/230912a_complete_det_enviro.csv")


# munging -----------------------------------------------------------------

dat <- dat %>% 
  filter(Presence == "1")

dat1 <- dat %>% 
  filter(Location %in% c("Wolf Rock", "Moreton Island", "Flat Rock",
                         "Coffs Harbour", "Hawks Nest", "Sydney",
                         "Jervis Bay"))


# Define the location order
location_order <- c("Wolf Rock", "Moreton Island", "Flat Rock", 
                    "Coffs Harbour", "Hawks Nest", "Sydney",
                    "Jervis Bay")

# Ensure that the Location variable is set with the desired factor levels
dat1$Location <- factor(dat1$Location, levels = location_order)

# Calculate the 'num_det' count
sample_size <- dat1 %>%
  group_by(Location) %>%
  summarize(num_det = n()) %>%
  mutate(myaxis = paste0(Location, "\n", "n=", num_det))

# Set the factor levels for 'myaxis'
sample_size$myaxis <- factor(sample_size$myaxis,
                             levels = sample_size$myaxis[match(location_order,
                                                    sample_size$Location)])

# Left join the summarized data back to the original dataset
dat2 <- left_join(dat1, sample_size, by = "Location")


# sst ---------------------------------------------------------------------

# Plot
sstanom <- 
  dat2 %>%
  ggplot(aes(x = myaxis, y = SST_anomaly, fill = Location)) +
  geom_violin(width = 1.2, position = position_dodge(0.7), alpha = 0.3) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.7) +
  scale_fill_viridis_d(direction = -1) +
  ggtitle("Sea surface temperature climatological anomalies of C. taurus in East Australia [2012 - 2022]") +
  xlab("Location") +
  ylab("Sea Surface Temperature (°C) Anomaly ") +
  theme(legend.position = "none",
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)) +
facet_grid(movement~Sex)


print(sstanom)

#save
ggsave(path = "Outputs/Graphs/Polishing/SST", "231115_SST_anomaly_pfuenzalida.png",
       plot = sstanom, width = 10, height = 7) #in inches because gg weird


#second plot
a2 <- 
  dat2 %>%
  ggplot(aes(x = myaxis, y = SST, fill = Location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.3) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.7) +
  scale_fill_viridis_d(direction = -1) +
  ggtitle("Sea surface temperature of C. taurus movements in East Australia [2012 - 2022]") +
  xlab("Location") +
  ylab("Sea Surface Temperature (°C)") +
  theme(legend.position = "none",
    plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  ) + 
  facet_wrap(~Sex)


plot(a2)

ggsave(path = "Outputs/Graphs/Polishing/SST",
       "231115 SST Violins.png", plot = a2, width = 10, height = 5) #in inches because gg weird

# distance vs movement ---------------------------------------------------------

#Distance v movement 
a3 <-
  dat2 %>% 
 ggplot(aes(x = myaxis, y = distance,
                 fill = Sex, colour = Sex)) +
  geom_boxplot(size = 0.5, colour = "black", alpha = 0.8) +
  labs(x = "Location", y = "Distance (km)", fill = "Sex", title = "Travelling distance aganist movement between locations for C. taurus [2012 - 2022]") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~movement) +
scale_fill_manual(values = c("firebrick2", "steelblue2"))

plot(a3)

#Distance v direction
a4 <- 
  ggplot(dat2, aes(x = myaxis, y = distance, fill = Sex, colour = Sex)) +
  geom_boxplot(size = 0.5, colour = "black", alpha = 0.8) +
  labs(x = "Location", y = "Distance (km)", fill = "Sex", title = "Travelling distance aganist direction between locations for C. taurus [2012 - 2022]") +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Direction) +
  scale_fill_manual(values = c("firebrick2", "steelblue2"))

plot(a4)

final <- ggarrange(a3, a4, ncol = 1)

final

ggsave(path = "Outputs/Graphs/Polishing/Det", "230911 Dist v Direction for East Aus.png",plot = final, width = 10, height = 12) #in inches because gg weird

# currents ----------------------------------------------------------------

# Plot
gsla <-
  dat2 %>%
  ggplot(aes(x = myaxis, y = anomaly_GSLA, #gsla, ucur, vcur, rs_current_velocity
             fill = Location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.3) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.7) +
  scale_fill_viridis_d(direction = -1) +
  xlab("Location") +
  ylab("Climatological anomaly of global sea level (spatial) anomaly") +
  theme(legend.position = "none",
        plot.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.title.y = element_text(size = 15),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
  ) +
  facet_grid(movement~Sex)
  
print(gsla)

#save
ggsave(path = "Outputs/Graphs/Polishing/Currents/GSLA", "231115_GSLAA_facetgrid.png",
       plot = gsla, width = 10, height = 7) #in inches because gg weird

# VCUR --------------------------------------------------------------------

# Plot
VCUR <-
  dat2 %>%
  ggplot(aes(x = myaxis, y = anomaly_VCUR, #gsla, ucur, vcur, rs_current_velocity
             fill = Location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.3) +
  geom_boxplot(width = 0.15, position = position_dodge(0.7), color = "black",
               alpha = 1.0, size = 0.7) +
  scale_fill_viridis_d(direction = -1) +
  xlab("Location") +
  ylab("South - North current direction climatological anomaly") +
  theme(legend.position = "none",
        plot.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.title.y = element_text(size = 15),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
  ) +
  facet_grid(movement~Sex)

print(VCUR)

ggsave(path = "Outputs/Graphs/Polishing/Currents/VCUR",
       "231115_VCUR_anomaly.png",
       plot = VCUR, width = 10, height = 7) #in inches because gg weird


# Lunar Illumination ------------------------------------------------------

dat <- dat %>% 
  mutate(Sex = factor(Sex),
         movement = factor(movement))


ggplot(data = dat, aes(x = Location, y = lunar.illumination,
                       fill = Location)) +
  geom_point() +
  facet_grid(movement~Sex) +
  geom_violin(alpha = 0.2) +
  scale_fill_viridis_d(direction = -1)

dat2 <- dat1 %>% 
  filter(Presence == "1")
  

lunar <- 
  ggplot(data = dat2, aes(x = myaxis, y = lunar.illumination,
  fill = Location)) +
  geom_jitter(alpha = 0.3, width = 0.3) +
  geom_violin(width = 1, alpha = 0.3, scale = "width") +
  scale_fill_viridis_d(direction = -1) +
xlab("Location") +
  ylab("Lunar Illumination") +
    theme(legend.position = "none",
          plot.title = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)
    ) +
    facet_grid(movement~Sex)

lunar

#save
ggsave(path = "Outputs/Graphs/Polishing/lunar", "231115_lunar_facetgrid.png",
       plot = lunar, width = 12, height = 7) #in inches because gg weird

# vioplot -----------------------------------------------------------------

count <- dat %>%
  group_by(Location, Sex, movement) %>%
  summarise(Count = n(), .groups = 'drop')
print(count, n = 36)

