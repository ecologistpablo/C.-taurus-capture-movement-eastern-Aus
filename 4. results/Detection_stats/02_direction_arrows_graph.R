#2023_07_21
  #north - south analysis

pacman::p_load("tidyverse", "ggpubr")
rm(list=ls()) 
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250730_step9.rds") %>%  #residence events
  filter(presence == 1)

location_levels <- c("Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                     "Gold Coast", "Ballina", "Evans Head", "Coffs Harbour",
                      "Pot Macquarie", "Hawks Nest", "Sydney", "Illawarra")

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
  filter(location %in% location_levels) %>%
  mutate(location = factor(location, levels = location_levels))

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

ggsave(path = "Outputs/Graphs/Final/detections", "250807_movement_arrows.pdf",
       plot = x, width = 8, height = 14) #in inches because gg weird

#  