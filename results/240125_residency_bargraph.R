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
                         "Coffs Harbour", "Hawks Nest", "Sydney",
                         "Jervis Bay")) %>%
  mutate(Location = factor(Location, levels = c("Wolf Rock", "Moreton Island",
                                                "Flat Rock", "Coffs Harbour",
                                                "Hawks Nest", "Sydney","Jervis Bay")),
         Month = factor(month(date)))

# take 2 ------------------------------------------------------------------

dat1 <- dat1 %>% 
  mutate(as.character(Tag_ID))

# Assuming your data is in a dataframe called 'dat'
dat2 <- dat1 %>%
  distinct(Tag_ID, Location, date, Month, animal_sex) %>% # Remove duplicates
  group_by(Tag_ID, Location, Month, animal_sex) %>%
  summarise(days_present = n_distinct(date), .groups = 'drop') %>%
  group_by(Location, animal_sex, Month) %>%
  summarise(avg_days_present = mean(days_present), .groups = 'drop')


summary(dat2$avg_days_present)
str(dat2)

res <- ggplot(dat2,
       aes(x = month, y = avg_days_present, fill = animal_sex)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  facet_grid(Location~animal_sex) +
  scale_fill_manual(values = c("F" = "#440154FF", "M" = "#5DC863FF")) +
  labs(title = "Monthly Residency by Sex at Each Location",
       x = "Month",
       y = "Monthly Residency",
       fill = "Sex") +
  theme_grey() +
  scale_x_continuous(breaks = 1:12, labels = month.abb, name = "Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
res  

ggsave(path = "Outputs/Graphs/Final", "240125_det_residency.png",
       plot = res, width = 10, height = 15) #in inches because gg weird
