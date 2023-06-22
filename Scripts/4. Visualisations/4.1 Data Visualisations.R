################################################################################
# Title: 4.1 Data Visualisations
# Author: Thomas Nibbering
# Date: June 14th, 2023
# Version: V1
################################################################################
####
# 1. Load Packages
####
# Load Packages
library(sf)
library(tidyverse)
library(tmap)
library(firatheme)

####
# 2. Load Data
####
# Load Dutch administrative borders
nld <- st_read('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/1. External/NL_Country_Boundary.gpkg')

# Load meteorological stations
wind_stns <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/2. Pre-Processed/KNMI_Station_Wind_Directions.csv')

# Load model output
nn_sen <- read.csv('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/NN_Sensitivity_Analysis.csv')
idw_sen <- read.csv('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/IDW_Sensitivity_Analysis.csv')
uk_sen <- read.csv('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/UK_Sensitivity_Analysis.csv')
rf_sen <- read.csv('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/RF_Sensitivity_Analysis.csv')

################################################################################
#                           1. Meteorological Stations
################################################################################
####
# 1. Stations
####
# Obtain coordinates for each distinct station considered
stns <- wind_stns %>% distinct(station_code, .keep_all = T)

# Transform into sf-object
stns <- st_as_sf(stns, coords = c('X', 'Y'), crs = 28992)

# Visualise stations
stations <- tm_shape(nld) + 
            tm_polygons(col = 'darkgrey') + 
            tm_shape(stns) + 
            tm_dots(size = 0.3, col = '#FFCD00') +
            tm_compass() + 
            tm_layout(frame = F) + 
            tm_scale_bar(width = 0.2)

# Store stations
tmap_save(stations, '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Thesis/Figures/KNMI_Stations.png')

################################################################################
#                               2. Forest Plot
################################################################################
####
# 1. Sensitivity Analysis
####
# Combine sensitivty analysis output
sensitivity <- rbind(nn_sen, idw_sen, uk_sen, rf_sen)

# Visualise model performance
ggplot(sensitivity, aes(x = est, y = reorder(algorithm, est), xmin = lower, xmax = upper)) + 
  geom_point(size = 3, shape = 15) + 
  geom_errorbarh(height = 0.2) + 
  xlim(c(0, 50)) + 
  labs(title = 'Sensitivity Analysis of Spatial Interpolation Models on Hourly Wind Direction Records (N = 100)',
       y = 'Spatial Interpolation Models', 
       x = 'Circular Root-Mean-Squared Error (CRMSE)') + 
  theme_fira() + 
  theme(text = element_text(family = 'Times New Roman', face = 'bold'))
