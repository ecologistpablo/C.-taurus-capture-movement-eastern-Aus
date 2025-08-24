#24.01.24
# creating residence data

# we have residency, let's calculate a monthly residency index
# we're using methods outlined in:
# https://www.sciencedirect.com/science/article/am/pii/S0006320723002008
# number of days a tagged shark was detected at location
# divided by the total number of days in that month that tags were still active 
# (Clarke et al., 2022)
# tag was deemed active that month if it was detected

# load libraries and data -------------------------------------------------

pacman::p_load("tidyverse", "ggpubr")
rm(list=ls()) 
setwd("/Users/owuss/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250730_residency.rds") #residence events

location_levels <- c("Wide Bay", "Sunshine Coast", "North Stradbroke Island",
                     "Gold Coast", "Ballina", "Evans Head", "Coffs Harbour",
                     "Pot Macquarie", "Hawks Nest", "Sydney", "Illawarra")

dat1 <- dat %>%
  filter(location %in% location_levels) %>%
  mutate(location = factor(location, levels = location_levels))
unique(dat1$location)

str(dat1)
# residency index ---------------------------------------------------------

# 1. Expand all residency events into daily rows, extract month and year
dat_days <- dat1 %>%
  mutate(start_date = as.Date(start_datetime),
         end_date = as.Date(end_datetime)) %>%
  rowwise() %>% # push through function at each row 
  mutate(date = list(seq(start_date, end_date, by = "day"))) %>%
  ungroup() %>%
  dplyr::select(tag_id, sex, location, date) %>%
  unnest(date) %>%
  mutate(year = year(date),
    month = month(date, label = TRUE))

# 2. For each tag/location/month, count days present
monthly_days <- dat_days %>%
  group_by(tag_id, sex, location, month, year) %>%
  summarise(n_days_present = n_distinct(date), .groups = "drop")

# 3. For each tag/location/month, get number of years tag was active
years_per_tag_month <- monthly_days %>%
  group_by(tag_id, location, month) %>%
  summarise(n_years = n(), .groups = "drop")   # n() counts years (since year is grouped above)

# 4. Calculate mean days per year per tag/location/month
monthly_days <- monthly_days %>%
  left_join(years_per_tag_month, by = c("tag_id", "location", "month")) %>%
  mutate(days_per_year = n_days_present / n_years)

# 5. For each location/sex/month, get mean residency index
mean_residency <- monthly_days %>%
  group_by(location, sex, month) %>%
  summarise(mean_days_present = mean(days_per_year),
    sd_days_present = sd(days_per_year),          # Add SD
    se_days_present = sd(days_per_year) / sqrt(n()), # Add SE if you want
    .groups = "drop")

summary(mean_residency$mean_days_present)


# unique tags -------------------------------------------------------------

unique_tags_monthly <- dat_days %>%
  group_by(location, sex, month) %>%
  summarise(unique_tags = n_distinct(tag_id), .groups = "drop")

# plot --------------------------------------------------------------------

# residency index plot
a <- ggplot(mean_residency, aes(x = month, y = mean_days_present,
                           colour = sex, group = sex)) +
  geom_pointrange(aes(ymin = mean_days_present - sd_days_present, 
                      ymax = mean_days_present + sd_days_present),
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~location, ncol = 1) +
  scale_colour_manual(values = c("F" = "firebrick4", "M" = "deepskyblue4")) +
  labs( x = "Month", y = "Mean Days Present per Sex (± SD)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

# unique tags plot
b <- ggplot(unique_tags_monthly, aes(x = month, y = unique_tags, fill = sex)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~location, scales = "fixed", ncol = 1) +
  scale_fill_manual(values = c("F" = "firebrick4", "M" = "deepskyblue4"),
    name = "Sex") +
  labs(x = "Month", y = "Number of Unique Tags Detected") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank())

z <- ggarrange(a,b, common.legend = T, legend = "right")
z

ggsave(path = "outputs/Graphs/Final/detections", "250807_residency.pdf",
       plot = z, width = 7, height = 9) #in inches because gg weird

