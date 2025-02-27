#2023_07_21
  #north - south analysis

# Libraries
library(tidyverse)
#bring and clean data environment
rm(list=ls())
setwd("~/Documents/USC/Honours/R/data")
dat <- read_csv("Inputs/250212_det_enviro_complete.csv")

unique(dat$location)

#dat munging -------------------------------------------------------------------

# Remove rows where Presence is equal to 0
dat <- dat[dat$presence != 0, ]

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

#x <- 
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

# bargraph
#a <-
  ggplot(dat3, aes(x = month, yend = frequency, fill = sex)) +
  geom_bar(stat = "identity") +
  facet_wrap(~location, ncol = 1) +
  labs(x = "Month",
       y = "Mean residency (days)",
       fill = "Sex") +
  theme_bw() +
  scale_fill_manual(values = c("indianred4", "cyan4"))
  
  # Summarise total movements per month and location for the arrow length
  arrow_data <- dat3 %>%
    group_by(location, month) %>%
    summarise(total_movement = sum(frequency), .groups = "drop")
  
  ggplot(dat3, aes(x = factor(month), y = frequency, fill = sex)) +
    geom_col(position = "stack", width = 0.6) +  # Stacked bars
    geom_segment(data = arrow_data, aes(x = factor(month), xend = factor(month),
                                        y = 0, yend = total_movement),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"), 
                 size = 1.2, colour = "black") +  # Single black arrow
    scale_x_discrete(labels = month.abb, name = "Month") +
    scale_y_continuous(name = "Number of recorded movements") +
    scale_fill_manual(values = c("indianred4", "cyan4"), name = "Sex",
                      labels = c("Female", "Male")) +
    theme_bw() +
    facet_wrap(~location, ncol = 1)  


 #save
ggsave(path = "Outputs/Graphs/Final/detection", "250226_month-movement_arrow-graph.pdf",
       plot = x, width =7, height = 9) #in inches because gg weird


# Tins little inverse data trick ------------------------------------------


#tins little trick!

# Wayne's code!
make_lat_lon_label <- function (x) {
  format(abs(x), scientific = FALSE)
}


# Reprex
df <- tibble::tribble(~hemisphere, ~season, ~prob,
                      "North", "Jan-Mar", 0.5,
                      "South", "Jan-Mar", -0.5 # negative probabilities for the southern hemisphere so they're mirroring in the bar plot
)

# Testing it out
ggplot() +
  geom_col(data = df, aes(x = season, y = prob, fill = hemisphere)) +
  scale_y_continuous(label = make_lat_lon_label) +
  theme_bw()



# bar plots ---------------------------------------------------------------

#dist <- 
  ggplot(data = dat2, aes(x = Loc_n, y = distance)) +
  geom_boxplot(aes(fill = sex), width = 0.3, position = position_dodge(0.7), colour = "black",
               alpha = 0.5, size = 0.7) +
  facet_wrap(~movement) +
  scale_fill_manual(values = c("red3", "blue3")) +
  scale_shape_manual(values = c(17, 18)) +
  labs(x = "Location", y = "Distance travelled between movements (km)",
       title = "Distance travelled between locations of C. taurus split by movement & sex") +
  theme(plot.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14))+
  guides(colour = guide_legend(override.aes = list(shape = 16)))  # Ensure shapes are represented in the legend

dist


#direct <- 
  ggplot(data = dat2, aes(x = Loc_n, y = distance)) +
  geom_boxplot(aes(fill = sex), width = 0.3, position = position_dodge(0.7), colour = "black",
               alpha = 0.5, size = 0.7) +
  facet_wrap(~direction) +
  scale_fill_manual(values = c("red3", "blue3")) +
  scale_shape_manual(values = c(17, 18)) +
  labs(x = "Location", y = "Distance travelled between movements (km)",
       title = "Distance travelled between locations of C. taurus split by movement & sex") +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14))+
  guides(colour = guide_legend(override.aes = list(shape = 16)))  # Ensure shapes are represented in the legend

direct

z1 <- ggarrange(dist, direct, ncol = 2, common.legend = T, legend = "right")
z1

ggsave(path = "Outputs/Graphs/Polishing/Det", "231220_dist_movement_boxplots.png",
       plot = z1, width = 15, height = 7) #in inches because gg weird

#  