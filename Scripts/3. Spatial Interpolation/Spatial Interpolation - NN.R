################################################################################
# Title: 3.1 Spatial Interpolation - Nearest Neighbours
# Author: Thomas Nibbering
# Date: June 16th, 2023
# Version: V1
################################################################################
#                               1. Initialisation
################################################################################
####
# 1. Load Packages
####
# Load packages
library(sf)
library(terra)
library(tidyverse)

####
# 2. Load Data
####
# Load wind directions
wind <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/3. Final/KNMI_Station_Wind_Directions.csv')

# Load Dutch country boundary
nld <- st_read('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/1. External/NL_Country_Boundary.gpkg')

####
# 3. Regular Grid
####
# Obtain regular grid (1x1km)
nld <- st_bbox(nld) %>%
       st_as_stars(dx = 1000, dy = 1000) %>%
       st_set_crs(28992) %>%
       st_crop(nld)

####
# 4. Scenario
####
# Define scenario 
scenario <- wind %>% filter(datetime == as.POSIXct('2017-04-04 12:00:00', tz = 'UTC'))

####
# 5. Functions
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

# Define leave-one-out cross validation function
loocv_nn <- function(data){
            # Obtain folds
            data <- mutate(data, ID = seq.int(nrow(data)))
            # Initialise error metric
            error <- c()
            # Iterate over folds
            for (i in 1:nrow(data)){
              # Obtain train/validation set
              train <- filter(data, !ID %in% i)
              validate <- filter(data, ID %in% i)
              # Model Fit
              model <-  gstat(formula = DD ~ 1, locations = ~X+Y, data = train, nmax = 1, set=list(idp = 0))
              # Model Predictions
              pred <- predict(model, validate)$var1.pred
              # Model Performance
              cmrse <- CRMSE(validate$DD, pred)
              # Model Output
              error <- c(cmrse, error)
            }
            # Return cross validated mean circular root-mean-squared error  
            return(mean(error))
}

################################################################################
#                       2. Nearest Neighbour Interpolation
################################################################################
####
# 1. LOOCV Nearest Neighbours
####
# Perform leave-one-out cross validation on nearest neighbours algorithm
loocv_nn(scenario)

####
# 2. Spatial Interpolation
####
# Convert dataframe to sf-object
scenario <- st_as_sf(scenario, coords = c('X', 'Y'), crs = 28992)

# Define Nearest Neighbour Algorithm
nn <- gstat(formula = DD ~ 1, data = scenario, nmax = 1, set=list(idp = 0))

# Nearest Neighbour Interpolation
pred <- predict(nn, nld) 

################################################################################
#                       3. Nearest Neighbour Visualisation
################################################################################
####
# 1. Visualise
####
# Visualise Nearest Neighbour Interpolation
tm_shape(pred) + 
  tm_raster('var1.pred')



