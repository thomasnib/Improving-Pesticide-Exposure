################################################################################
# Title: 3.4 Spatial Interpolation - Random Forest V2
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
library(meteo)

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

# Define random forest k-fold cross validation function
kfcv_rf <- function(data, k, cluster_function, trees, nn){
            # Obtain folds
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
              train <- dplyr::filter(data, ID %in% folds[[1]][[i]][['in_id']])
              validate <- dplyr::filter(data, !ID %in% folds[[1]][[i]][['in_id']])
              # Convert to sf-object
              st_train <- st_as_sf(train, coords = c('X', 'Y'), crs = 28992)
              st_validate <- st_as_sf(validate, coords = c('X', 'Y'), crs = 28992)
              # Model Fit
              rf <- rfsi(DD ~ 1, data = st_train, num.trees = trees, n.obs = nn)
              # Model Predictions
              pred <- pred.rfsi(rf, data = st_train, obs.col = 3, newdata = st_validate)
              # Model Performance
              crmse <- CRMSE(st_validate$DD, pred$pred)
              # Model output
              error <- c(crmse, error)
            }
            # Return cross validated mean circular root-mean-squared error  
            return(mean(error))
}

# Define function to obtain scenarios
scenario_rf <- function(data, n){
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

# Define function to perform loocv_nn function across scenarios
sensitivity_rf <- function(data){
                  # Initalise error metric
                  error <- c()
                  # Iterate over scenarios
                  for (j in data){
                    # Model fit
                    crmse <- kfcv_rf(j, k = 10, cluster_function = 'kmeans', trees = 100, nn = 1)
                    # Model output
                    error <- c(crmse, error)
                  }
                  return(error)
}

################################################################################
#                       1. Random Forest Spatial Interpolation
################################################################################
####
# 1. Scenario
####
# Define scenario
scenario <- wind %>% filter(datetime == as.POSIXct('2017-04-04 12:00:00', tz = 'UTC'))

####
# 2. Hyperparameter Tuning
####
# Define search grid
grid <- expand.grid(nn = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                    trees = seq.int(50, 400, 50))

# Initialise metrices
trees <- c()
nn <- c()
error <- c()

# Iterate over parameters
for (i in 1:nrow(grid)){
    # Model fit
    crmse <- kfcv_rf(scenario, k = 10, cluster_function = 'kmeans', trees = grid[i, 2], nn = grid[i, 1])
    # Model performance
    trees <- c(grid[i, 2], trees)
    nn <- c(grid[i, 1], nn)
    error <- c(crmse, error)
}

# Combine into dataframe
rf_hyp <- data.frame(trees = trees,
                     nn = nn,
                     error = error)

####
# 3. Spatial K-fold Cross Validation
####
# Perform spatial K-fold cross validation 
kfcv_rf(scenario, k = 10, cluster_function = 'kmeans', trees = trees[which.min(error)], nn = nn[which.min(error)]) # Trees = 100 and nn = 2

####
# 4. Spatial Interpolation
####
# Convert to sf-object
scenario <- st_as_sf(scenario, coords = c('X', 'Y'), crs = 28992)

# Model fit
rf <- rfsi(DD ~ 1, data = scenario, num.trees = 150, n.obs = 3)

# Model predictions
pred <- pred.rfsi(rf, scenario, obs.col = 3, newdata = nld)

################################################################################
#                               2. Visualisation
################################################################################
####
# 1. Datatype
####
# Convert to sf-object
pred <- st_as_sf(pred, coords = c('X', 'Y'), crs = 28992)

####
# 2. Visualise
####
# Visualise interpolation
rf_visual <- tm_shape(pred) +
             tm_dots('pred', 
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
             tm_layout(title = '(D)',
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
tmap_save(rf_visual, '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Thesis/Figures/RF_Interpolation.png')

####
# 3. Environment
####
# Remove objects from environment
rm(grid, nld, pred, rf, rf_hyp, rf_visual, scenario, crmse, error, i, nn, trees)

################################################################################
#                             3. Sensitivity Analysis
################################################################################
####
# 1. Scenario
####
# Obtain scenarios (N = 100) 
scenario <- scenario_rf(wind, n = 383)

####
# 2. Sensitivity Analysis
####
# Obtain circular root-mean-squared error for each scenario
crmse <- sensitivity_rf(scenario)

####
# 3. Output
####
# Convert to dataframe
rf_output <- data.frame(crmse_rf = crmse)

####
# 4. Store Output
####
# Store sensitivity analysis output 
write.csv(rf_output, 
          '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/RF_Sensitivity_Analysis.csv',
          row.names = F)
