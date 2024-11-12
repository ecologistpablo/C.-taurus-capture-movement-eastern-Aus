#12
  #24.07.23
    #Lunar illumination against movement (Departures and Arrivals)


rm(list=ls())
setwd("~/University/2023/Honours/R/data")
library(tidyverse)
dat <- read_csv("Inputs/230912_complete_det_enviro.csv")

cp <- colorRampPalette(c("black", "beige"))(n = 1) #colour palette that matches the lunar cycle

# Define the location order
location_order <- c("Wolf Rock", "Moreton Island", "Flat Rock", "Port Macquarie",
                    "Coffs Harbour","Seal Rocks", "Hawks Nest", "Sydney",
                    "Jervis Bay", "Montague Island")

# Convert Location to factor with specified order
dat$Location <- factor(dat$Location, levels = location_order)

#For lunar illumination, we only want presences no psuedo absences
dat <- dat %>% 
  filter(Presence == "1")


# circular plot -----------------------------------------------------------

z1 <- ggplot(dat, aes(x = lunar.phase.deg, fill = ..x..)) +
  geom_histogram(binwidth = 25) +
  coord_polar(start = 0) +
  scale_fill_gradientn(colors = cp, name = "Lunar Illumination") +
  scale_x_continuous(breaks = c(0, 90, 180, 270, 360), 
                     labels = c("0°", "90°", "180°", "270°", "360°"), 
                     limits = c(0, 360)) +  # set the limits of the x-axis
  theme_classic() +
  labs(x = "Lunar Phase (in degrees)", y = "Frequency",
       title = "Lunar light of Arrivals across east Aus",
       legend = "Lunar Illumination") +
  theme(plot.title = element_text(size = 10)) +
  facet_grid(movement ~ Location)  # facet by Sex first, then by Location

#save
ggsave(path = "Outputs/Graphs/Polishing/All Locations/lunar", "230915 Lunar Circular Sex + movement facet.png",
       plot = z1, width = 20, height = 10) #in inches because gg weird


# lavalamp ----------------------------------------------------------------

luna <- colorRampPalette(c("black", "beige"))(n = 1) #colour palette that matches the lunar cycle


# Plot
lavalamps1 <- dat %>%
  ggplot(aes(x = Sex, y = lunar.illumination, 
             fill = Location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.3) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "Lunar light between locations across: sex, movement & direction",
    x = "Sex", y = "Lunar Illumination") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10)) +
  facet_wrap(~Location, nrow = 1)
  #facet_grid(movement ~ Location)

plot(lavalamps1)

# 2 -----------------------------------------------------------------------

# Plot
lavalamps2 <- dat %>%
  ggplot(aes(x = movement, y = lunar.illumination, #gsla, ucur, vcur, rs_current_velocity
             fill = Location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.3) +
  scale_fill_viridis_d(direction = -1) +
  #ggtitle("Lunar light against Locations across East Aus") +
  xlab("Movement") +
  ylab("Lunar Illumination") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10)) +
  facet_wrap(~Location, nrow = 1)

plot(lavalamps2)


# 3 -----------------------------------------------------------------------

# Plot
lavalamps3 <- dat %>%
  ggplot(aes(x = Direction, y = lunar.illumination, #gsla, ucur, vcur, rs_current_velocity
             fill = Location)) +
  geom_violin(width = 1, position = position_dodge(0.7), alpha = 0.3) +
  scale_fill_viridis_d(direction = -1) +
  #ggtitle("Lunar light against Locations across East Aus") +
  xlab("Direction of travel") +
  ylab("Lunar Illumination") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10)) +
  facet_wrap(~Location, nrow = 1)
plot(lavalamps3)


lavamatrix <- ggarrange(lavalamps1, lavalamps2, lavalamps3,
          nrow = 3)

plot(lavamatrix)

#save
ggsave(path = "Outputs/Graphs/Polishing/All Locations/lunar", "230915_Lavalamp_grid_lunar.png",
       plot = lavamatrix, width = 25, height = 15) #in inches because gg weird

#Lunar north - south movements -------------------------------------------------

#turn radians into degrees to plot circularly
rad2deg <- function(rad) return(180*rad/pi)

# Add a column for lunar.phase in degrees
dat1$lunar.phase.deg <- rad2deg(dat1$lunar.phase)

# Create custom color palette
cp <- colorRampPalette(c("black", "beige", "black"))(n = 360) #colour palette that matches the lunar cycle


#North
dat2_a <- dat1 %>% filter(Direction == 'North')

#a thicc ggplot
c <- ggplot(dat2_a, aes(x = lunar.phase.deg, fill = ..x..)) +
  geom_histogram(binwidth = 25) +
  coord_polar(start = 0) +
  scale_fill_gradientn(colors = cp, name = "Lunar Illumination") +
  scale_x_continuous(breaks = c(0, 90, 180, 270, 360), 
                     labels = c("0°", "90°", "180°", "270°", "360°"), 
                     limits = c(0, 360)) +  # set the limits of the x-axis
  theme_minimal() +
  labs(x = "Lunar Phase (in degrees)", y = "Frequency",
       title = "Lunar light of incoming northly movements in Flat Rock",
       legend = "Lunar Illumination") +
  theme(plot.title = element_text(size = 10)) +
  facet_wrap(~Location, ncol = 1)

plot(c)

#females
dat2_b <- dat1 %>% filter(Direction == 'South')

#a thicc ggplot
d <- ggplot(dat2_b, aes(x = lunar.phase.deg, fill = ..x..)) +
  geom_histogram(binwidth = 25) +
  coord_polar(start = 0) +
  scale_fill_gradientn(colors = cp, name = "Lunar Illumination") +
  scale_x_continuous(breaks = c(0, 90, 180, 270, 360), 
                     labels = c("0°", "90°", "180°", "270°", "360°")) +
  theme_minimal() +
  labs(x = "Lunar Phase (in degrees)", y = "Frequency",
       title = "Lunar light of incoming southerly movements in Flat Rock",
       legend = "Lunar Illumination") +
  theme(plot.title = element_text(size = 10)) +
  facet_wrap(~Location, ncol = 1)
plot(d)

z1 <- ggpubr::ggarrange(c, d, ncol = 2, nrow = 1, common.legend = T , legend = "bottom") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))
plot(z1)

#save
ggsave(path = "Outputs/Graphs/All Locations/lunar", "230727 Lunar Ill N-S Aus.png",
       plot = z1, width = 10, height = 20) #in inches because gg weird



