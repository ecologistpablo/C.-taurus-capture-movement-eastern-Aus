#transboundary movement calculator
  #27.12.23
    #

rm(list=ls()) 
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")
dat <- read_csv("Inputs/230907_step7.csv")

dat1 <- dat %>% 
  filter(Tag_ID == "6398")

# Step 1: Define QLD and NSW locations
QLD_locations <- c("Wolf Rock", "Moreton Island", "Flat Rock", "deg_-24", "deg_-25", "deg_-27", "deg_-28")
NSW_locations <- c("Coffs Harbour", "Hawks Nest", "Sydney", "Jervis Bay", "deg_-29", "deg_-30", "deg_-31", "deg_-32", "deg_-33", "deg_-34", "deg_-35", "deg_-36", "Montague Island", "Seal Rocks", "Port Macquarie")

# Manually set "Coolangatta SCP" to QLD
dat1 <- dat %>%
  mutate(Location = ifelse(Location == "Coolangatta SCP ", "QLD", Location)) #one receiver on the QLD / NSW Border thats

# Step 2: Sort data by Tag_ID and detection_datetime
dat2 <- dat1 %>%
  arrange(Tag_ID, detection_datetime)

# Step 3: Identify the first detection for each tag
first_detections <- dat2 %>%
  group_by(Tag_ID) %>%
  summarize(First_Detection = first(detection_datetime),
            First_Location = first(Location))

# Step 4: Track subsequent detections for each tag
transboundary_movements <- dat2 %>%
  group_by(Tag_ID) %>%
  mutate(State_Change = (Location %in% QLD_locations & lag(Location) %in% NSW_locations) | 
           (Location %in% NSW_locations & lag(Location) %in% QLD_locations)) %>%
  filter(State_Change)

# Step 5: Summarize the number of individuals and movements
summary_data <- transboundary_movements %>%
  summarize(Num_Individuals = n_distinct(Tag_ID),
            Total_Movements = n()) %>% 
  ungroup()


# Output the summary
print(summary_data)

# Sex divide --------------------------------------------------------------

# Step 3: Identify the first detection for each tag, including Sex
first_detections <- dat2 %>%
  group_by(Tag_ID) %>%
  summarize(First_Detection = first(detection_datetime),
            First_Location = first(Location),
            Sex = first(Sex))

# Step 4: Track subsequent detections for each tag
transboundary_movements <- dat2 %>%
  group_by(Tag_ID) %>%
  mutate(State_Change = (Location %in% QLD_locations & lag(Location) %in% NSW_locations) | 
           (Location %in% NSW_locations & lag(Location) %in% QLD_locations)) %>%
  filter(State_Change)

# Step 5: Summarize the number of individuals, movements and include Sex
summary_data <- transboundary_movements %>%
  group_by(Tag_ID, Sex) %>%
  summarize(Total_Movements = n()) %>%
  ungroup() %>%
  group_by(Sex) %>%
  summarize(Num_Individuals = n_distinct(Tag_ID),
            Total_Movements = sum(Total_Movements))

# Output the summary
print(summary_data)
