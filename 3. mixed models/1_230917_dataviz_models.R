#17.09.23
  #before we model, we visualise what to model and how to model


# helpers -----------------------------------------------------------------

rm(list=ls())
source("~/University/2023/Honours/R/data/git/GNS-Movement/000_helpers.R")

#bring and clean data environment
setwd("~/University/2023/Honours/R/data")
dat <- read_csv("Inputs/230912a_complete_det_enviro.csv")

dat <- dat %>% 
  dplyr::select(-detection_datetime, -Departure_date, -Arrival_date)

summary(dat)


# visualise our relationships ---------------------------------------------

# Create a function for plotting
SmoothPlot <- function(data, xvar) {
  ggplot(data = data,
         aes(x = .data[[xvar]],
             y = Presence, colour = Sex)) +
    #if we colour by Sex it does some crazy things
    #colouring by movement doesn't really differentiate interactions starkly
    #colouring by anything means n = < 20 which isn't ideal
    geom_point() +
    geom_smooth(method = "loess", se = T) +  # You can change the smoothing method as needed
    #facet_grid(Location ~ Sex + month, scales = "free")
    facet_grid(movement~Location, scales = "free")
}

# SST ---------------------------------------------------------------------

# List of continuous variables you want to plot
continuous_vars <- c("SST", "SST_anomaly")

# Create plots for each continuous variable
plot_list <- lapply(continuous_vars, function(var) {
  SmoothPlot(dat1, var)
})

# Arrange and print the plots
sst_direction <- grid.arrange(grobs = plot_list)  # Adjust the number of columns as needed

#wolf rock has a semi-linear relationship as well as Jervis bay for SST
#the rest are non-linear, with 1 - 3 squiggles

#WR has Presence & absence at the full spectrum, relationships can't be disentangled


SST <- ggarrange(sst_direction, sst_movement, sst_Sex,
                 ncol = 3)

SST

#save
ggsave(path = "Outputs/Graphs/Polishing/SST", "SST_combined_lineplot_230925.png",
       plot = SST, width = 20, height = 15) #in inches because gg weird

# sea level anomaly -------------------------------------------------------

continuous_vars <- c("cur_GSLA", "anomaly_GSLA")

# Create plots for each continuous variable
plot_list <- lapply(continuous_vars, function(var) {
  SmoothPlot(dat1, var)
})

# Arrange and print the plots
glsa_sex <- grid.arrange(grobs = plot_list)  # Adjust the number of columns as needed

gsla <- ggarrange(glsa_direction, glsa_movement, glsa_sex,
                 ncol = 3)

gsla

#save
ggsave(path = "Outputs/Graphs/Polishing/GSLA", "GSLA_combined_lineplot_230925.png",
       plot = gsla, width = 20, height = 15) #in inches because gg weird

# south - north currents --------------------------------------------------

continuous_vars <- c("cur_VCUR", "anomaly_VCUR")

# Create plots for each continuous variable
plot_list <- lapply(continuous_vars, function(var) {
  SmoothPlot(dat1, var)
})

# Arrange and print the plots
vcur_sex <- grid.arrange(grobs = plot_list)  # Adjust the number of columns as needed

vcur <- ggarrange(vcur_direction, vcur_movement, vcur_sex,
                 ncol = 3)

vcur

#save
ggsave(path = "Outputs/Graphs/Polishing/Currents/VCUR", "VCUR_combined_lineplot_230925.png",
       plot = vcur, width = 20, height = 15) #in inches because gg weird


# dist to land + bathy ----------------------------------------------------

#Presence and absence doesn't work with this as spatially they're not different
#only temporally :o

# UCUR --------------------------------------------------------------------

continuous_vars <- c("cur_UCUR", "anomaly_UCUR")

# Create plots for each continuous variable
plot_list <- lapply(continuous_vars, function(var) {
  SmoothPlot(dat1, var)
})

# Arrange and print the plots
ucur_sex <- grid.arrange(grobs = plot_list)  # Adjust the number of columns as needed

ucur <- ggarrange(ucur_direction, ucur_movement, ucur_sex,
                 ncol = 3)

ucur

#save
ggsave(path = "Outputs/Graphs/Polishing/Currents/UCUR", "ucur_combined_lineplot_230925.png",
       plot = ucur, width = 20, height = 15) #in inches because gg weird



