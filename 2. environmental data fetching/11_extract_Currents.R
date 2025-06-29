# 11 September 2023
# P. Fuenzalida
# extracting currrent data

rm(list=ls())

# Packages ----------------------------------------------------------------

library(terra)
library(sf)
library(sp)
library(viridis)
library(lubridate)
library(tidyr)
# pts ---------------------------------------------------------------------

setwd("~/University/2023/Honours/R/data")

cstack <- rast("IMOS/Currents/230912_cstack_12-22.tif") #currents stack
rcs <- read_csv("Inputs/250626_xy_coordinates.csv") #xy points
WGS84 <- crs("EPSG:4326")# Coordinate reference systems

rcs1 <- rcs %>% # fixing duplicate station_name rows 
  group_by(station_name) %>%
  mutate(station_name = if (n_distinct(latitude, longitude) > 1) 
    paste0(station_name, "_", row_number()) 
    else 
      station_name) %>%
  ungroup()

unique(rcs1$station_name) # should be 237 or numb of observations

pts.WGS <- st_as_sf(rcs1, coords = c("longitude", #convert to an SF object
                                     "latitude")) 
st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS

# plotting ----------------------------------------------------------------

plot(cstack[[3]], col = viridis(255))
pplot(pts.WGS, add = T)

# extract -----------------------------------------------------------------

cur.pts <- extract(cstack, pts.WGS, ID = F) # ID = FALSE  

# 7 day averages ----------------------------------------------------------

# reshape matrix into long format
cur.pts1 <- cur.pts %>%
  pivot_longer(cols = -station_name, names_to = "var_date", values_to = "value") %>%
  tidyr::extract(var_date, into = c("var", "date"), regex = "([A-Z]+)_(\\d{8})") %>%
  mutate(date = as.Date(date, format = "%Y%m%d"))

# make 7-day averages for data
cur.pts2 <- cur.pts1 %>%
  mutate(week_start = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(station_name, var, week_start) %>%
  mutate(value_7d = if (all(is.na(value)))
    NA_real_ else mean(value, na.rm = TRUE)) %>%
  ungroup()



# let's find out how many stations are full of NAs
# our data is coastal, so it sometimes happens that environmental data
# does not land on the coordinates close to land

# calculate number of stations with only NAs
cur.pts2 %>%
  group_by(station_name, var) %>%
  summarise(total_rows = n(),
            num_na = sum(is.na(value_7d)),
            all_na = all(is.na(value_7d)),
            prop_na = mean(is.na(value_7d)),
            .groups = "drop") %>%
  arrange(desc(all_na), desc(prop_na)) %>%
  print(n = Inf)

# around %15 NA, which is ok without bi-linear interpolation

# bi-linear interpolation -------------------------------------------------

bl <- extract(cstack, pts.WGS, method = "bilinear", ID = FALSE)
bl$station_name <- pts.WGS$station_name

bl1 <- bl %>%
  setNames(c(names(cstack), "station_name")) %>%
  pivot_longer(cols = -station_name, names_to = "var_date", values_to = "value") %>%
  tidyr::extract(var_date, into = c("var", "date"), regex = "([A-Z]+)_(\\d{8})") %>%
  mutate(date = as.Date(date, format = "%Y%m%d"))

bl2 <- bl1 %>%
  mutate(week_start = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(station_name, var, week_start) %>%
  mutate(bl_value_7d = if (all(is.na(value)))
    NA_real_ else mean(value, na.rm = TRUE)) %>%
  ungroup() 

# join both ---------------------------------------------------------------

cur.pts3 <- cur.pts2 %>%
  left_join(bl2 %>% select(station_name, date, var, bl_value_7d), 
            by = c("station_name", "date", "var")) %>%
  mutate(value_7d_filled = if_else(is.na(value_7d), bl_value_7d, value_7d))


# how many NAs are still around

cur.pts3 %>%
  group_by(station_name, var) %>%
  summarise(total_rows = n(),
            num_na = sum(is.na(value_7d_filled)),
            all_na = all(is.na(value_7d_filled)),
            prop_na = mean(is.na(value_7d_filled))) %>%
  arrange(desc(all_na), desc(prop_na)) %>% print(n = Inf)

# 18 stations out of 711 are NA

# that's 2.5% NAs
# nice

# clean - up --------------------------------------------------------------

# clean and pivot
cur.pts4 <- cur.pts3 %>% 
  distinct(station_name,var, date, value_7d_filled) %>% 
  pivot_wider(names_from = var, values_from = value_7d_filled)

# save --------------------------------------------------------------------

write_csv(cur.pts4, file = "Inputs/250627_Currents_vals_12-22.csv")
