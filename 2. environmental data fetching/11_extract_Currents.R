# 11 September 2023
  # P. Fuenzalida
    # extracting currrent data

rm(list=ls())

# Packages ----------------------------------------------------------------

pacman::p_load("ncdf4", 'purrr', 'furrr','future', 'terra', 'sf', 'sp', 'viridis', 'tidyverse')

# pts ---------------------------------------------------------------------

setwd("~/Documents/USC/Honours/R/data")

cstack <- rast("IMOS/Currents/250728_cstack_12-24.tif") #currents stack
rcs <- read_csv("Inputs/250728_step9_coordinates.csv") #xy points
WGS84 <- crs("EPSG:4326")# Coordinate reference systems

unique(rcs$station_name) # should be numb of observations in your df

pts.WGS <- st_as_sf(rcs, coords = c("longitude", "latitude")) #sf object
st_crs(pts.WGS) <- crs(WGS84) #remember to assign crs
pts.WGS

# plotting ----------------------------------------------------------------

plot(cstack[[3]], col = viridis(255))
plot(pts.WGS, add = T)

# extract -----------------------------------------------------------------

cur.pts <- terra::extract(cstack, pts.WGS, ID = F) # ID = FALSE  

# 7 day averages ----------------------------------------------------------

cur.pts$station_name <- pts.WGS$station_name

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

# NA check ----------------------------------------------------------------

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

bl <- terra::extract(cstack, pts.WGS, method = "bilinear", ID = FALSE)

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

# are ANY fully na ? 
# nice

# clean - up --------------------------------------------------------------

# clean and pivot
cur.pts4 <- cur.pts3 %>% 
  distinct(station_name,var, date, value_7d_filled) %>% 
  pivot_wider(names_from = var, values_from = value_7d_filled)

head(cur.pts4)
tail(cur.pts4)

# save --------------------------------------------------------------------

write_csv(cur.pts4, file = "Inputs/250728_Currents_vals_12-24.csv")
