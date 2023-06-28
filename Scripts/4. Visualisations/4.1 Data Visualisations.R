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
library(data.table)
library(tmap)
library(firatheme)

####
# 2. Load Data
####
# Load Dutch administrative borders
nld <- st_read('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/1. External/NL_Country_Boundary.gpkg')

# Load meteorological stations
wind_stns <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/2. Pre-Processed/KNMI_Station_Wind_Directions.csv')

# Load sensitivity analysis
sen_nn <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/NN_Sensitivity_Analysis.csv', col.names = 'CRMSE')
sen_idw <-fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/IDW_Sensitivity_Analysis.csv', col.names = 'CRMSE')
sen_uk <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/UK_Sensitivity_Analysis.csv', col.names = 'CRMSE')
sen_rf <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/RF_Sensitivity_Analysis.csv', col.names = 'CRMSE')

################################################################################
#                               1. Wind Directions
################################################################################
####
# 1. Wind Directions
####
# Visualise wind directions
wind_direction <- ggplot(wind_stns, aes(x = DD)) + 
                  geom_histogram(binwidth = 20, fill = '#FFCD00', color = '#454545', linewidth = 0.15) + 
                  scale_x_continuous(breaks = c(0, 90, 180, 270), limits = c(0, 360)) + 
                  coord_polar(start = 0) + 
                  labs(y = 'Prevalence', 
                       x = 'Wind Direction (in Degrees)') + 
                  theme_fira() + 
                  theme(text = element_text(family = 'Times New Roman'),
                        axis.line.x = element_blank(),
                        axis.title.x = element_text(hjust = 0.5, face = 'bold'), 
                        axis.line.y = element_blank(),
                        axis.title.y = element_text(hjust = 0.5, face = 'bold'))

# Store wind directions
ggsave('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Thesis/Figures/Wind_Directions.png', 
       wind_direction, 
       height = 3, 
       width = 7)

####
# 2. Environment
####
# Remove objects from environment
rm(wind_direction)
  
################################################################################
#                           2. Meteorological Stations
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
            tm_polygons(col = '#454545') + 
            tm_shape(stns) + 
            tm_dots(size = 0.3, col = '#FFCD00') +
            tm_compass() + 
            tm_layout(frame = F) + 
            tm_scale_bar(width = 0.2)

# Store stations
tmap_save(stations, '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Thesis/Figures/KNMI_Stations.png')

####
# 2. Environment
####
# Remove objects from environment
rm(stns, stations)

################################################################################
#                           3. Sensitivity Analysis
################################################################################
####
# 1. Sensitivity Analysis
####
# Define algorithm associated with each analysis
sen_nn$algorithm <- 'Nearest Neighbour'
sen_idw$algorithm <- 'Inverse Distance Weighting'
sen_uk$algorithm <- 'Universal Kriging'
sen_rf$algorithm <- 'Random Forest'

# Combine sensitivyt analyses
sensitivity <- rbind(sen_nn, sen_idw, sen_uk, sen_rf)

# Visualise sensitivity analysis
sen_visual <- ggplot(sensitivity, aes(x = CRMSE, y = reorder(algorithm, CRMSE, median))) + 
              geom_boxplot() + 
              labs(x = 'Circular Root-Mean-Squared Error (CRMSE)', 
                   y = '') + 
              theme_fira() + 
              theme(text = element_text(family = 'Times New Roman'),
                    axis.title.x = element_text(hjust = 0.5, face = 'bold'))

# Store sensitivity analysis 
ggsave('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Thesis/Figures/Sensitivity_Analysis.png', 
       sen_visual)
