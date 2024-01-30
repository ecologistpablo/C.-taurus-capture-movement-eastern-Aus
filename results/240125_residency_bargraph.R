#25.01.24
  #making a residency bargraph
    #to add to our beautiful spatial barplot

source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")
rm(list=ls())

setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/240125_res_dat.csv") 

str(dat)
head(dat)

# dplyr -------------------------------------------------------------------

dat1 <- dat %>%
  filter(Location %in% c("Wolf Rock", "Moreton Island", "Flat Rock",
                         "Coffs Harbour", "Hawks Nest", "Sydney", "Jervis Bay")) %>%
  mutate(Location = factor(Location, levels = c("Wolf Rock", "Moreton Island",
                                                "Flat Rock", "Coffs Harbour",
                                                "Hawks Nest", "Sydney", "Jervis Bay")),
         Month = factor(month(date), labels = month.abb),
         Year = factor(year(date)))  # Use abbreviated month names for readability

# take 4 ------------------------------------------------------------------

# calculate the number of days each tag is present at each location per month and year
dat2 <- dat1 %>%
  group_by(Tag_ID, Location, animal_sex, Year, Month) %>%
  summarise(
    days_present = n_distinct(date),  # Number of unique days the tag was detected
    .groups = 'drop'
  )

# aggregate across all years to get the average residency per location for each month and sex
dat3 <- dat2 %>%
  group_by(Location, animal_sex, Month) %>%
  summarise(
    avg_residency = mean(days_present),  # Average number of days present per site, per month, split by sex
    .groups = 'drop'
  )

# ggplot
ggplot(dat3, aes(x = Month, y = avg_residency, fill = animal_sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Location~animal_sex) +
  labs(title = "Average Residency by Sex at Each Location",
       x = "Month",
       y = "Average Residency (days)",
       fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

# unique tags -------------------------------------------------------------

dat2 <- dat1 %>%
  group_by(Location, Month = month(date), animal_sex) %>%
  summarise(
    unique_tags = n_distinct(Tag_ID),  # Count of unique tags
    .groups = 'drop'
  )

ggplot(dat2, aes(x = Month, y = unique_tags, fill = animal_sex)) +
  geom_bar(stat = "identity") +
  facet_grid(Location~animal_sex) +
  labs(title = "Total Unique Tags by Sex at Each Location",
       x = "Month",
       y = "Total Unique Tags",
       fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, name = "Month") 
