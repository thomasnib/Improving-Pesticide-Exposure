################################################################################
# Title: 2.2 Data Preparation and Enrichment - Regular Grid
# Author: Thomas Nibbering
# Date: June 16th, 2023
# Version: V1
################################################################################
#                               1. Initialisation
################################################################################
####
# 1. Load Packages
####
library(tidyverse)
library(sf)
library(stars)

####
# 2. Load Data
####
# Load Dutch administrative borders
nld <- st_read('/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/1. External/NL_Country_Boundary.gpkg')

################################################################################
#                       2. Data Preparation & Enrichment
################################################################################
####
# 1. Regular Grids
####
# Convert Dutch administrative borders into regular grids (1x1km)
grd <- st_bbox(nld) %>%
       st_as_stars(dx = 1000, dy = 1000) %>%
       st_crop(nld)

################################################################################
#                             3. Data Storage
################################################################################
####
# 1. Store
####
# Store regular grid into Tiff-format
write_stars(grd, 
            '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/2. Pre-Processed/NL_Grid_1x1km.tiff')


