################################################################################
# Title: 3.1 Spatial Interpolation - Mode
# Author: Thomas Nibbering
# Date: June 21th, 2023
# Version: V2
################################################################################
####
# 1. Load Packages
####
# Load Packages
library(tidyverse)
library(data.table)
library(sf)
library(stars)
library(spatialsample)
library(tmap)

####
# 2. Load Data
####
# Load wind directions
wind <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/3. Final/KNMI_Station_Wind_Directions.csv')

# Load Dutch country boundary
nld <- st_read('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/3. Final/NL_Country_Boundary.gpkg')

####
# 3. Regular Grid
####
# Obtain regular grid (1x1km)
nld <- st_bbox(nld) %>%
       st_as_stars(dx = 1000, dy = 1000) %>%
       st_set_crs(28992) %>%
       st_crop(nld) %>%
       st_as_sf() %>%
       st_centroid()

####
# 4. Functions
####
# Define circular root-mean-squared error function
CRMSE <- function(actual, predicted){
         # Obtain difference 
         diff <- abs(actual - predicted)
         # Obtain circular difference
         circ <- ifelse(diff > 180, 360 - diff, diff)
         # Obtain circular root-mean-squared error
         CRMSE <- sqrt(mean(circ^2))
         return(CRMSE)
}

# Define function to obtain mode
mode <- function(x){
        # Obtain unique values
        val <- unique(x)
        # Return value most frequently occuring
        return(val[which.max(tabulate(match(x, val)))])
} 

# Define nearest neighbour k-fold cross validation function
kfcv_m <- function(data, k, cluster_function){
          # Define rownumbers
          data <- mutate(data, ID = seq.int(nrow(data)))
          # Convert to sf-object
          data <- st_as_sf(data, coords = c('X', 'Y'), crs = 28992)
          # Obtain folds
          folds <- spatial_clustering_cv(data, v = k, cluster_function = cluster_function)
          # Obtain coordinates
          data <- data %>% cbind(., st_coordinates(.)) %>% st_drop_geometry()
          # Convert to dataframe
          data <- as.data.frame(data)
          # Initialise error metric
          error <- c()
          # Iterate over folds
          for (i in 1:k){
              # Obtain Train/Validation Data
              train <- filter(data, ID %in% folds[[1]][[i]][['in_id']])
              validate <- filter(data, !ID %in% folds[[1]][[i]][['in_id']])
              # Model fit
              modus <- mode(train$DD)
              # Model predictions
              validate$pred <- modus
              # Model performance
              crmse <- CRMSE(validate$DD, validate$pred)
              # Model output
              error <- c(crmse, error)
          }
          # Return cross validated mean circular root-mean-squared error  
          return(mean(error))
}

# Define function to obtain scenarios
scenario_m <- function(data, n){
              # Define randomisation
              set.seed(123)
              # Define dates
              dates <- sample(seq(as.POSIXct('2017-01-01'), as.POSIXct('2017-12-31'), by = 'hour'), n)
              # Initialise scenario
              scenario <- c()
              # Iterate over dates
              for (i in dates){
                  # Obtain scenarios
                  samples <- list(filter(data, datetime == c(i)))
                  # Output scenarios
                  scenario <- c(samples, scenario)
              }
              return(scenario)
}

# Define function to perform kfcv_nn function across scenarios
sensitivity_m <- function(data){
                 # Initalise error metric
                 error <- c()
                 # Iterate over scenarios
                 for (j in data){
                     # Model fit
                     crmse <- kfcv_m(j, k = 10, cluster_function = 'kmeans')
                     # Model output
                     error <- c(crmse, error)
                 }
                 return(error)
}

################################################################################
#                           1. Mode Interpolation
################################################################################
####
# 1. Scenario
####
# Define scenario
scenario <- wind %>% filter(datetime == as.POSIXct('2017-04-04 12:00:00', tz = 'UTC'))

####
# 2. Spatial K-fold Cross Validation
####
# Perform spatial K-fold cross validation 
kfcv_m(scenario, k = 10, cluster_function = 'kmeans')

####
# 3. Spatial Interpolation
####
# Model fit
modus <- mode(scenario$DD)

# Model predictions
nld$values <- modus

################################################################################
#                               2. Visualisation
################################################################################
####
# 1. Visualise
####
# Visualise interpolation
m_visual <- tm_shape(nld) + 
            tm_dots('values', 
                    breaks = c(0, 40, 80, 120, 160, 200, 240, 280, 320, 360),
                    palette = c('#5287c6', '#436fac', '#345792', '#254179',
                                '#152c60', 
                                '#254179', '#345792', '#436fac', '#5287c6'),
                    legend.show = F) + 
            tm_add_legend(type = 'fill',
                          title = 'Wind Direction (in Degrees)',
                          col = c('#5287c6', '#436fac', '#345792', '#254179',
                                  '#152c60', 
                                  '#254179', '#345792', '#436fac', '#5287c6'),
                          labels = c('0 to 40', '40 to 80', '80 to 120', '120 to 160',
                                     '160 to 200',
                                     '200 to 240', '240 to 280', '280 to 320', '320 to 360')) + 
            tm_compass() + 
            tm_scale_bar(width = 0.15) + 
            tm_layout(title = '(A)',
                      frame = F, 
                      legend.title.fontfamily = 'Times New Roman',
                      legend.title.fontface = 'bold',
                      legend.title.size = 1, 
                      legend.text.fontfamily = 'Times New Roman',
                      legend.text.size = 0.8)

####
# 2. Store
####
# Store visualisation in PNG-format
tmap_save(m_visual, '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Thesis/Figures/M_Interpolation.png')

####
# 3. Environment
####
# Remove objects from environment
rm(m_visual, nld, scenario, modus)

################################################################################
#                             3. Sensitivity Analysis
################################################################################
####
# 1. Scenario
####
# Obtain scenarios (N = 383) 
scenario <- scenario_m(wind, n = 383)

####
# 2. Sensitivity Analysis
####
# Obtain circular root-mean-squared error for each scenario
crmse <- sensitivity_m(scenario)

####
# 3. Output
####
# Convert to dataframe
m_output <- data.frame(crmse_m = crmse)

####
# 4. Store Output
####
# Store sensitivity analysis output 
write.csv(m_output, 
          '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/M_Sensitivity_Analysis.csv',
          row.names = F)
