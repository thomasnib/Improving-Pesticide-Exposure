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
