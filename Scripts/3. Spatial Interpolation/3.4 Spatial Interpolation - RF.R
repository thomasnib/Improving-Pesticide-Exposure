################################################################################
# Title: 3.4 Spatial Interpolation - Spatial Random Forest
# Author: Thomas Nibbering
# Date: June 19th, 2023
# Version: V1
################################################################################
#                               1. Initialisation
################################################################################
####
# 1. Load Packages
####
library(sf)
library(meteo)
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
# Obtain regular grid
nld <- st_bbox(nld) %>%
       st_as_stars(dx = 1000, dy = 1000) %>%
       st_set_crs(28992) %>%
       st_crop(nld) %>%
       st_as_sf() %>%
       st_centroid()

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
loocv_rf <- function(data, trees, nn){
            # Obtain folds
            data <- mutate(data, ID = seq.int(nrow(data)))
            # Initialise error metric
            id <- c()
            actual <- c()
            predi <- c()
            error <- c()
            # Iterate over folds
            for (i in 1:nrow(data)){
              # Obtain Train/Validation
              train <- filter(data, !ID %in% i)
              validate <- filter(data, ID %in% i)
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
            return(mean(error))
}

################################################################################
#                         2. Random Forest Interpolation
################################################################################
####
# 1. Hyperparameter Tuning
####
# Define grid search
grid <- expand.grid(nn = 1, # Limited to 1 given the framework of this model in the context of LOOCV
                    trees = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500))

# Initialise metrices
nearest_n <- c()
n_trees <- c()
crmse <- c()

# Perform random forest leave-one-out cross validation to obtain optimal value of number of trees
for (i in 1:nrow(grid)){
    # Model Fit 
    rf <- loocv_rf(scenario, trees = grid[i, 2], nn = grid[i, 1])
    # Model Performance
    nearest_n <- c(grid[i, 1], nearest_n)
    n_trees <- c(grid[i, 2], n_trees)
    crmse <- c(rf, crmse)
}

# Convert model performance output into dataframe
rf_hyp <- data.frame(nn = nearest_n, 
                     trees = n_trees, 
                     crmse = crmse)

####
# 2. Spatial Interpolation
####
# Convert dataframe to sf-object
scenario <- st_as_sf(scenario, coords = c('X', 'Y'), crs = 28992)

# Define random forest algorithm
rf <- rfsi(DD ~ 1, data = scenario, n.obs = 1, num.trees = n_trees[which.min(crmse)])

# Random forest interpolation
pred <- pred.rfsi(rf, data = scenario, obs.col = 3, newdata = nld)

################################################################################
#                           3. Random Forest Visualisation
################################################################################
####
# 1. Sf-object
####
# Convert to sf-object
pred <- st_as_sf(pred, coords = c('X', 'Y'), crs = 28992)

####
# 1. Visualise
####
# Visualise random forest interpolation
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
tmap_save(rf_visual, '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Thesis/Figures/RF_Interpolation.png')


