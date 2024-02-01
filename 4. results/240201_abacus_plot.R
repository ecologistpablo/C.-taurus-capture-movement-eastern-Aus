
# libraries ---------------------------------------------------------------

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

rm(list=ls()) 
setwd("~/University/2023/Honours/R/data") 
IMOS <- read_csv("Inputs/240114_step3.csv")

str(IMOS)
unique(IMOS$Location)


IMOS <- IMOS %>% 
  filter(Location %in% c("Wolf Rock", "Moreton Island",
                     "Flat Rock", "Coffs Harbour",
                     "Hawks Nest", "Sydney",
                     "Jervis Bay")) %>% 
  mutate(Location = fct_relevel(Location, "Wolf Rock", "Moreton Island",
                     "Flat Rock", "Coffs Harbour",
                     "Hawks Nest", "Sydney",
                     "Jervis Bay"))

# Plotting
a <- ggplot(IMOS, aes(x = detection_datetime, y = as.factor(Tag_ID), colour = Location)) +
  geom_point(alpha = 0.8, size = 2) +  # Adjust size and transparency as needed
  theme_grey() +
  scale_colour_viridis_d(direction = -1) +
  labs(x = "Time", y = "Transmitter ID", colour = "Location") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2022-12-31")))  
a


#save
ggsave(path = "outputs/Graphs/Final/detection", "240201_abacus.pdf",
       plot = a, width = 12, height = 8) #in inches because gg weird

