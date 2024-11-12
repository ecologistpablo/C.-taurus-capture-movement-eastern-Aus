#2023_07_21
  #10
    #north - south analysis

# Libraries

library(tidyverse)

#bring and clean data environment
rm(list=ls())
setwd("~/University/2023/Honours/R/data")
#dat <- read_csv("Inputs/230726_step8.csv")
dat <- read_csv("Inputs/230912a_complete_det_enviro.csv")

unique(dat$Location)

#dat munging -------------------------------------------------------------------

# Remove rows where Presence is equal to 0
dat <- dat[dat$Presence != 0, ]

# Step 1: Convert detection_datetime to month (1-12)
dat1 <- dat %>%
  mutate(month = as.numeric(format(detection_datetime, "%m")))

# Create a summary dataframe that counts num_det for each combination of Location, Direction, and Month
sum <- dat1 %>%
  group_by(Location) %>%
  summarise(num_det = n()) %>%
  ungroup() %>%
  mutate(Loc_n = paste0(Location, "\n", "(n =", num_det, ")"))

# Merge the summary text back into the original dataframe
dat1 <- left_join(dat1, sum, by = c("Location"))

# Filter for specific locations in the original dataframe
dat2 <- dat1 %>% 
  filter(Location %in% c("Wolf Rock", "Moreton Island", "Flat Rock", "Coffs Harbour",
                         "Hawks Nest", "Sydney", "Jervis Bay"))

str(dat2)
unique(dat2$Loc_n)

# Convert Location to factor with specified order
dat2$Loc_n <- factor(dat2$Loc_n, levels = c("Wolf Rock\n(n =61)",
                "Moreton Island\n(n =79)", "Flat Rock\n(n =170)",
                "Coffs Harbour\n(n =114)","Hawks Nest\n(n =36)", "Sydney\n(n =78)",
                "Jervis Bay\n(n =98)"))

table(dat2$Loc_n)

#a better histogram ------------------------------------------------------------

# Calculate the count of movements for each direction and month
dat2 <- dat2 %>%
  group_by(Location, Direction, month, Sex) %>%
  summarize(frequency = n(), .groups = "drop") %>% 
mutate(frequency = ifelse(Direction == "South", -frequency, frequency))#southward movements go south


# Create a plot
x <- 
  ggplot(dat2, aes(x = month, y = 0, color = Direction,
                         xend = month, yend = frequency)) +
  geom_segment(size = 0.8, arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, name = "Month") +
    #scale_y_continuous(label = make_lat_lon_label) +
  scale_color_manual(values = c("firebrick3", "skyblue3"), name = "Travelling direction",
                     labels = c("North", "South")) +
  labs(title = "Histogram of North-South Movements by Month and Location",
       x = "Month", y = "Frequency of observations") +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_grid(Location ~ Sex)

# Display the plot
print(x)



 #save
ggsave(path = "Outputs/Graphs/Polishing/Det", "N-S_Movements_v_month_Aus_231113.pdf",
       plot = x, width = 10, height = 15) #in inches because gg weird


# Tins little inverse data trick ------------------------------------------


#tins little trick!

# Wayne's code!
make_lat_lon_label <- function (x) {
  format(abs(x), scientific = FALSE)
}


# # Reprex
# df <- tibble::tribble(~hemisphere, ~season, ~prob,
#                       "North", "Jan-Mar", 0.5,
#                       "South", "Jan-Mar", -0.5 # negative probabilities for the southern hemisphere so they're mirroring in the bar plot
# )
# 
# # Testing it out
# ggplot() +
#   geom_col(data = df, aes(x = season, y = prob, fill = hemisphere)) +
#   scale_y_continuous(label = make_lat_lon_label) +
#   theme_bw()
# 


# bar plots ---------------------------------------------------------------

dist <- ggplot(data = dat2, aes(x = Loc_n, y = distance)) +
  geom_boxplot(aes(fill = Sex), width = 0.3, position = position_dodge(0.7), colour = "black",
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


direct <- ggplot(data = dat2, aes(x = Loc_n, y = distance)) +
  geom_boxplot(aes(fill = Sex), width = 0.3, position = position_dodge(0.7), colour = "black",
               alpha = 0.5, size = 0.7) +
  facet_wrap(~Direction) +
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