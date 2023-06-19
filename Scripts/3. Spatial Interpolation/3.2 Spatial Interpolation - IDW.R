################################################################################
# Title: 3.2 Spatial Interpolation - Inverse Distance Weighting
# Author: Thomas Nibbering
# Date: June 19th, 2023
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
loocv_idw <- function(data, beta){
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
               model <-  gstat(formula = DD ~ 1, locations = ~X+Y, data = train, set=list(idp = beta))
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
#                   2. Inverse Distance Weighting Interpolation
################################################################################
####
# 1. Hyperparameter Tuning
####
# Define IDW p-values
powers <- seq(0.001, 4, 0.01)

# Initialise metrices
betas <- c()
crmse <- c()

# Perform IDW leave-one-out cross validation to obtain optimal value of p
for (p in powers){
    # Model Fit
    idw <- loocv_idw(scenario, p)
    # Model Performance
    betas <- c(p, betas)
    crmse <- c(idw, crmse)
}

# Convert model performance output into dataframe
idw_hyp <- data.frame(betas = betas, 
                      crmse = crmse)

# Visualise 
ggplot(idw_hyp, aes(x = betas, y = crmse)) + geom_point()

####
# 2. Spatial Interpolation
####
# Convert dataframe to sf-object
scenario <- st_as_sf(scenario, coords = c('X', 'Y'), crs = 28992)

# Define IDW Algorithm
idw <- gstat(formula = DD ~ 1, data = scenario, set = list(idp = betas[which.min(crmse)]))

# IDW Interpolation
pred <- predict(idw, nld)

################################################################################
#                               3. IDW Visualisation
################################################################################
####
# 1. Visualise
####
# Visualise IDW Interpolation
tm_shape(pred) + 
  tm_raster('var1.pred',
            title = 'Wind Direction (in Degrees)',
            breaks = c(0, 40, 80, 120, 160, 200, 240, 280, 320, 360),
            palette = c('#5287c6', '#436fac', '#345792', '#254179',
                        '#152c60', 
                        '#254179', '#345792', '#436fac', '#5287c6')) + 
  tm_compass() + 
  tm_scale_bar(width = 0.15) + 
  tm_layout(frame = F, 
            legend.title.fontfamily = 'Times New Roman',
            legend.title.fontface = 'bold',
            legend.title.size = 1, 
            legend.text.fontfamily = 'Times New Roman',
            legend.text.size = 0.7)



