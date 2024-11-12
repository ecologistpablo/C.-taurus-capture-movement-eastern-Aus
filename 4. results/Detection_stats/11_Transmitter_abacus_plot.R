# 2024 02 21
  # abacus plots of tag IDs to show total detections


# libraries ---------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

rm(list=ls()) 
setwd("~/University/2023/Honours/R/data") 
greydat <- read_csv("Inputs/240114_step2.csv")
IMOS <- read_csv("Inputs/240114_step3.csv")

str(IMOS)
unique(IMOS$Location)


IMOS1 <- IMOS %>% 
  filter(Location %in% c("Wolf Rock", "Moreton Island",
                     "Flat Rock", "Coffs Harbour",
                     "Hawks Nest", "Sydney",
                     "Jervis Bay")) %>% 
  mutate(Location = fct_relevel(Location, "Wolf Rock", "Moreton Island",
                     "Flat Rock", "Coffs Harbour",
                     "Hawks Nest", "Sydney",
                     "Jervis Bay"))

# Plotting
a <- ggplot(IMOS1, aes(x = detection_datetime, y = as.factor(Tag_ID), colour = Location)) +
  geom_point(alpha = 0.8, size = 2) +  # Adjust size and transparency as needed
  theme_grey() +
  scale_colour_viridis_d(direction = -1) +
  labs(x = "Time", y = "Transmitter ID", colour = "Location") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2022-12-31")))  
a


#save
ggsave(path = "outputs/Graphs/Final/detection", "240201_abacus.pdf",
       plot = a, width = 12, height = 8) #in inches because gg weird



# Adjusting ggplot code to plot greydat first, then IMOS1
b <- ggplot() +
  geom_point(data = greydat, aes(x = detection_datetime, y = as.factor(Tag_ID)),
             colour = "grey4", alpha = 0.5, size = 2) +  # Plot greydat as grey dots
  geom_point(data = IMOS1, aes(x = detection_datetime, y = as.factor(Tag_ID),
                               colour = Location), alpha = 0.8, size = 2) +  # Then add IMOS1 data
  theme_grey() +
  scale_colour_viridis_d(direction = -1) +
  labs(x = "Time", y = "Transmitter ID", colour = "Location") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2022-12-31")))

b

#save
ggsave(path = "outputs/Graphs/Final/detection", "240219_abacus.png",
       plot = b, width = 12, height = 8) #in inches because gg weird

# location based abacus plot ----------------------------------------------

IMOS1 <- IMOS %>% 
  filter(Location %in% c("Jervis Bay", "Sydney", "Hawks Nest", "Coffs Harbour", "Flat Rock", "Moreton Island", "Wolf Rock")) %>% 
  mutate(Location = fct_relevel(Location, "Jervis Bay", "Sydney", "Hawks Nest", "Coffs Harbour", "Flat Rock", "Moreton Island", "Wolf Rock"))


c <- ggplot(IMOS1, aes(x = detection_datetime, y = Location, colour = Location)) +
  geom_point(alpha = 0.8, size = 2) +  # Adjust size and transparency as needed
  theme_grey() +
  scale_colour_viridis_d(direction = -1) +
  labs(x = "Time", y = "Location", colour = "Location") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2022-12-31"))) +
  scale_y_discrete(limits = rev(levels(IMOS1$Location)))

#save
ggsave(path = "outputs/Graphs/Final/detection", "240221_abacus_locations.png",
       plot = c, width = 12, height = 8) #in inches because gg weird

