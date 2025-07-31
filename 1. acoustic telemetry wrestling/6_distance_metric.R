#26 June 2025
  # pablo fuenzalida
    # calculate distance

library(tidyverse)
rm(list=ls())
setwd("/Documents/USC/Honours/R/data")
dat <- read_rds("Inputs/250730_step5.rds")

# distance metric --------------------------------------------------------------

# let's calculate how far each movement was in kilometers using the harvesine method
# this accounts for earth being a sphere, so it's a little better than direct dist
calculate_distance <- function(df) {
  df <- df %>%
    arrange(tag_id, movement_id) %>%
    group_by(tag_id, movement_id) %>%
    mutate(
      # Calculate the haversine distance between two rows with the same movement_id
      distance = geosphere::distHaversine(
        c(longitude[1], latitude[1]),
        c(lead(longitude)[1], lead(latitude)[1])
      ) / 1000 # Convert meters to km
    ) %>%
    mutate(    # Ensure the distance is applied to both rows and round to 2 decimal places
      distance = if_else(!is.na(distance), round(distance, 2), NA_real_)
    ) %>%
    ungroup()
  
  return(df)
}

# run the function
dat1 <- calculate_distance(dat)

# save it -----------------------------------------------------------------

write_rds(dat1, file = "Inputs/250730_step6.rds")

# plot it  ----------------------------------------------------------------

# Calculate mean and 95% CI per location and sex
dat2 <- dat1 %>%
  group_by(location, sex) %>%
  summarise(mean_distance = mean(distance, na.rm = TRUE),
    se = sd(distance, na.rm = TRUE) / sqrt(n()), 
    n = n(),
    .groups = "drop") %>%
  mutate(ci_lower = mean_distance - 1.96 * se,
    ci_upper = mean_distance + 1.96 * se)

# point-range plot
ggplot(dat2, aes(x = location, y = mean_distance, colour = sex)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
                  position = position_dodge(width = 0.5), 
                  fatten = 2,  # makes the dot larger
                  linewidth = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


