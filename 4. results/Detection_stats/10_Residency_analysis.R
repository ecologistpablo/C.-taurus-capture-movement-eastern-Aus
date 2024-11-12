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
a <- ggplot(dat3, aes(x = Month, y = avg_residency, fill = animal_sex)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Location, ncol = 1) +
  labs(x = "Month",
       y = "Mean residency (days)",
       fill = "Sex") +
  theme_grey() +
  scale_fill_manual(values = c("indianred4", "cyan4"))
a  

# unique tags -------------------------------------------------------------

dat2 <- dat1 %>%
  group_by(Location, Month = month(date), animal_sex) %>%
  summarise(unique_tags = n_distinct(Tag_ID),  # Count of unique tags
    .groups = 'drop')

b <- ggplot(dat2, aes(x = Month, y = unique_tags, fill = animal_sex)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Location, ncol = 1) +
  labs(x = "Month",
       y = "Total number of tagged sharks present (2012 - 2022)",
       fill = "Sex") +
  theme_grey() +
  scale_x_continuous(breaks = 1:12, labels = month.abb, name = "Month")  +
  scale_fill_manual(values = c("indianred4", "cyan4"))
b

b <- b + ylim(0, 35) #make the y limits the same in both graphs

z <- ggarrange(b, a, common.legend = T, legend = "right")
z

#for red - green colourblind people
protan(z)

#save
ggsave(path = "outputs/Graphs/Final/detection", "240517_det_residency_bars.pdf",
       plot = z, width = 12, height = 14) #in inches because gg weird

