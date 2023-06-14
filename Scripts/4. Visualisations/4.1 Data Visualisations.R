################################################################################
# Title: 4.1 Data Visualisations
# Author: Thomas Nibbering
# Date: June 14th, 2023
# Version: V1
################################################################################
#                               1. Initialisation
################################################################################
####
# 1. Initialise
####
# Load Packages
library(sf)
library(tidyverse)
library(tmap)

####
# 2. Load Data
####
# Load Dutch administrative borders
nld <- st_read('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/1. External/NL_Country_Boundary.gpkg')

# Load meteorological stations
wind_stns <- fread('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/2. Pre-Processed/KNMI_Station_Wind_Directions.csv')

################################################################################
#                               2. Visualisations
################################################################################
####
# 1. Study Area with Stations
####
# Obtain coordinates for each distinct station considered
stns <- wind_stns %>% distinct(station_code, .keep_all = T)

# Transform dataframe into sf-object
stns <- st_as_sf(stns, coords = c('X', 'Y'), crs = 28992)
mapview::mapview(stns)
