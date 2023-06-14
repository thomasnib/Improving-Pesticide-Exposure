################################################################################
# Title: 2.1 Data Preparation and Enrichment
# Author: Thomas Nibbering
# Date: June 14th, 2023
# Version: V1
################################################################################
#                               1. Initialisation
################################################################################
####
# 1. Load Packages
####
library(tidyverse)
library(data.table)
library(sf)

####
# 2. Load Data
####
# Load wind directions 
wind <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/1. External/KNMI_Wind_Directions.csv')

# Load meteorological stations
stns <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/1. External/KNMI_Weather_stations.csv')

################################################################################
#                       2. Data Preparation & Enrichment
################################################################################
####
# 1. Wind Directions
####
# Convert station code variable to character data type
wind$station_code <- as.character(wind$station_code)

# Convert variables to more interpretable measurements
wind <- wind %>%
        mutate(datetime = as.POSIXct(paste0(as.Date(date), ' ', hour, ':00:00'), format = '%Y-%m-%d %H:%M:%S'),
               FH = FH * 0.1, 
               FF = FF * 0.1, 
               FX = FX * 0.1)

# Keep only variables of interest
wind <- wind %>% select(station_code, datetime, DD, FH, FF, FX)

####
# 2. Meteorological Stations
####
# Convert station code variable to character data type
stns$STN <- as.character(stns$STN)

####
# 3. Wind Directions & Meteorological Stations
####
# Combine wind directions with meteorological stations 
wind_stns <- inner_join(wind, stns, by = c('station_code' = 'STN'))

# Transform dataframe into sf-object
wind_stns <- st_as_sf(wind_stns, coords = c("LON", "LAT"), crs = 4326)

# Transform into single coordinate reference system (EPSG 28992)
wind_stns <- st_transform(wind_stns, crs = 28992)

# Obtain Coordinates 
wind_stns$X <- st_coordinates(wind_stns)[, 1]
wind_stns$Y <- st_coordinates(wind_stns)[, 2]

# Drop Geometry 
wind_stns <- wind_stns %>% st_drop_geometry()

# Resolve inconsistencies in the data
wind_stns <- wind_stns %>%
             # (1) Exclude observations with partial or complete absence of measurements
             drop_na(DD) %>%
             # (2) Exclude records representing calm or highly changeable wind directions
             filter(!DD %in% c(0, 800, 990))

################################################################################
#                             3. Data Storage
################################################################################
####
# 1. Storage
####
# Store wind directions with station coordinates in CSV-format
write.csv(wind_stns, 
         '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/2. Pre-Processed/KNMI_Station_Wind_Directions.csv', 
         row.names = F)
