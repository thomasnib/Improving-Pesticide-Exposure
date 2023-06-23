################################################################################
# Title: 3.2 Spatial Interpolation - Inverse Distance Weighting V2
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
library(spatialsample)
library(gstat)

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

# Define inverse distance weighting k-fold cross validation function
kfcv_idw <- function(data, k, cluster_function, beta){
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
                 # Convert to sf-object
                 st_train <- st_as_sf(train, coords = c('X', 'Y'), crs = 28992)
                 st_validate <- st_as_sf(validate, coords = c('X', 'Y'), crs = 28992)
                 # Model fit
                 idw <- gstat(formula = DD ~ 1, data = st_train, set=list(idp = beta))
                 # Model predictions
                 pred <- predict(idw, st_validate)$var1.pred
                 # Model performance
                 cmrse <- CRMSE(validate$DD, pred)
                 # Model output
                 error <- c(cmrse, error)
             }
             # Return cross validated mean circular root-mean-squared error  
             return(mean(error))
}

# Define function to obtain scenarios
scenario_idw <- function(data, n){
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
sensitivity_idw <- function(data){
                   # Initalise error metric
                   error <- c()
                   # Iterate over scenarios
                   for (j in data){
                       # Model fit
                       crmse <- kfcv_idw(j, k = 10, cluster_function = 'kmeans', beta = 3.441)
                       # Model output
                       error <- c(crmse, error)
                   }
                   return(error)
}

################################################################################
#             1. Inverse Distance Weighting Spatial Interpolation
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
p <- seq(0.001, 4, by = 0.01)

# Initialise metrices
beta <- c()
error <- c()

# Iterate over parameters
for (p in p){
    # Model fit
    crmse <- kfcv_idw(scenario, k = 10, cluster_function = 'kmeans', beta = p)
    # Model performance
    beta <- c(p, beta)
    error <- c(crmse, error)
}

# Combine into dataframe
idw_hyp <- data.frame(beta = beta, 
                      error = error)

####
# 3. Spatial K-fold Cross Validation
####
# Perform spatial K-fold cross validation 
kfcv_idw(scenario, k = 10, cluster_function = 'kmeans', beta = beta[which.min(error)]) # 3.441

####
# 4. Spatial Interpolation
####
# Convert to sf-object
scenario <- st_as_sf(scenario, coords = c('X', 'Y'), crs = 28992)

# Model fit
idw <- gstat(formula = DD ~ 1, data = scenario, set=list(idp = beta[which.min(error)])) # 3.441

# Model predictions
pred <- predict(idw, nld)

################################################################################
#                               2. Visualisation
################################################################################
####
# 1. Visualise
####
# Visualise interpolation
idw_visual <- tm_shape(pred) + 
              tm_dots('var1.pred', 
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
              tm_layout(frame = F, 
                        legend.title.fontfamily = 'Times New Roman',
                        legend.title.fontface = 'bold',
                        legend.title.size = 1, 
                        legend.text.fontfamily = 'Times New Roman',
                        legend.text.size = 0.8)

####
# 2. Store
####
# Store visualisation in PNG-format
tmap_save(idw_visual, '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Thesis/Figures/IDW_Interpolation.png')

####
# 3. Environment
####
# Remove objects from environment
rm(nld, idw, idw_hyp, idw_visual, nld, pred, scenario, beta, crmse, error, p)

################################################################################
#                             3. Sensitivity Analysis
################################################################################
####
# 1. Scenario
####
# Obtain scenarios (N = 100) 
scenario <- scenario_idw(wind, n = 383)

####
# 2. Sensitivity Analysis
####
# Obtain circular root-mean-squared error for each scenario
crmse <- sensitivity_idw(scenario)

####
# 3. Output
####
# Convert to dataframe
idw_output <- data.frame(crmse_idw = crmse)

####
# 4. Store Output
####
# Store sensitivity analysis output 
write.csv(idw_output, 
          '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/4. Output/IDW_Sensitivity_Analysis.csv',
          row.names = F)
