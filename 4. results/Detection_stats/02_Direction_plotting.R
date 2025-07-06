#2023_07_21
  #north - south analysis

pacman::p_load("tidyverse", "viridis", "ggpubr", "plotly", "sf", "rnaturalearth", "ggspatial",
               "terra")

# Data --------------------------------------------------------------------

rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250701_det_enviro_complete.csv")

dat <- dat %>% 
  filter(presence == 1) #only real data shown

#dat munging -------------------------------------------------------------------


# Create a summary dataframe that counts num_det for each combination of Location, Direction, and Month
sum <- dat %>%
  group_by(location) %>%
  summarise(num_det = n()) %>%
  ungroup() %>%
  mutate(Loc_n = paste0(location, "\n", "(n =", num_det, ")"))

# Merge the summary text back into the original dataframe
dat1 <- left_join(dat, sum, by = c("location"))

dat2 <- dat1 %>% 
  filter(location %in% c("Wolf Rock", "Flat Rock", "Coffs Harbour", "Hawks Nest", "Sydney")) %>% 
  mutate(location = fct_relevel(location, "Wolf Rock", "Flat Rock", "Coffs Harbour", "Hawks Nest", "Sydney"))

#a better histogram ------------------------------------------------------------

# Calculate the count of movements for each direction and month
dat3 <- dat2 %>%
  group_by(location, direction, month, sex) %>%
  summarize(frequency = n(), .groups = "drop") %>% 
mutate(frequency = ifelse(direction == "South", -frequency, frequency))#southward movements go south

x <- 
ggplot(dat3, aes(x = month, y = 0, colour = sex, yend = frequency)) +
  geom_segment(size = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, name = "Month") +
  scale_colour_manual(values = c("indianred4", "cyan4"), name = "Sex",
                      labels = c("Female", "Male")) +
  labs(x = "Month", y = "Number of recorded movements") +
  theme_bw() +
  facet_wrap(~location, ncol = 1)
# Display the plot
x

ggsave(path = "Outputs/Graphs/Polishing/Det", "231220_dist_movement_boxplots.png",
       plot = z1, width = 15, height = 7) #in inches because gg weird

#  