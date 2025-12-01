#2023_07_21
  #north - south analysis

pacman::p_load("tidyverse", "ggpubr")
rm(list=ls()) 
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/251201_step9.rds") %>%  #residence events
   filter(presence == 1)

dat1 <- dat %>%
  filter(!location %in% c("Moreton Island", "Yamba", "Forster", "Central Coast", "Merimbula"),
         !str_starts(location, "deg_")) %>%   # drop these sites)
  mutate(location = fct_relevel(location,
                                "Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                                "Gold Coast", "Ballina", "Evans Head", "Coffs Harbour",
                                "Port Macquarie", "Hawks Nest", 
                                "Sydney", "Illawarra", "Naroomba")) 

#dat munging -------------------------------------------------------------------

# Create a summary dataframe that counts num_det for each combination of Location, Direction, and Month
sum <- dat1 %>%
  group_by(location) %>%
  summarise(num_det = n()) %>%
  ungroup() %>%
  mutate(Loc_n = paste0(location, "\n", "(n =", num_det, ")"))

# Merge the summary text back into the original dataframe
dat2 <- left_join(dat1, sum, by = c("location"))

#a better histogram ------------------------------------------------------------

# Calculate the count of movements for each direction and month
dat2 <- dat1 %>%
  group_by(location, direction, month, sex) %>%
  summarize(frequency = n(), .groups = "drop") %>% 
mutate(frequency = ifelse(direction == "South", -frequency, frequency))#southward movements go south

x <- 
ggplot(dat2, aes(x = month, y = 0, colour = direction, yend = frequency)) +
  geom_segment(size = 0.5, arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, name = "Month") +
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted", alpha = 0.5) +
  scale_colour_manual(values = c("indianred4", "cyan4"), name = "Direction",
                      labels = c("North", "South")) +
  labs(x = "Month", y = "Number of movements") +
  theme_bw() +
  facet_grid(location~sex)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Display the plot
x

ggsave(path = "outputs/Graphs/Final/detections", "251201_movement_arrows.png",
       plot = x, width = 8, height = 14) #in inches because gg weird

  