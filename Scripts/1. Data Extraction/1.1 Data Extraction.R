################################################################################
# Title: 1.1 Data Extraction
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
library(httr)
library(jsonlite)
library(tidyverse)

####
# 2. Functions
####
# Function to obtain hourly wind directions from KNMI
knmi_hourly <- function(start, end, vars, stns) {
               # Define API Call
               knmi_url <- 'https://www.daggegevens.knmi.nl/klimatologie/uurgegevens'
               knmi_params <- list(start = start,
                                   end = end, 
                                   vars = vars, 
                                   stns = stns, 
                                   fmt = 'json')
               # Obtain Data
               df <- jsonlite::fromJSON(content(GET(knmi_url, query = knmi_params), as = 'text'))
               return(df)
}

# Function to obtain meteorological stations from KNMI
knmi_stns <- function(start, end, vars, stns){
             # Define API call
             knmi_url <- 'https://www.daggegevens.knmi.nl/klimatologie/daggegevens'
             knmi_params <- list(start = start,
                                 end = end, 
                                 vars = vars, 
                                 stns = 'ALL', 
                                 fmt = 'json')
             # Obtain data
             df <- jsonlite::fromJSON(content(GET(knmi_url, query = knmi_params), as = 'text'))
             # Return unique stations
             stns <- unique(df$station_code)
             return(stns)
}

################################################################################
#                               2. Data Extraction
################################################################################
####
# 1. Initialise
####
# Define variable constants
start <- '20170101'
end <- '20171231'
vars <- 'WIND'

####
# 2. Meteoroloigcal Stations
####
# Obtain meteorological stations in the period of interest 
stns <- knmi_stns(start = start, end = end, vars = vars)

####
# 3. Wind Directions
####
# Define empty vector
df <- c()

# Obtain wind directions in the period of interest
for (i in stns){
  hourly <- knmi_hourly(start, end, vars, i)
  df <- rbind(df, hourly)
  rm(hourly)
}

################################################################################
#                               3. Data Storage
################################################################################
####
# 1. Store 
####
# Store wind directions in CSV-format
write.csv(df, 
          '/Users/thomasnibbering/Documents/Github/Improving-Pesticide-Exposure/Data/1. External/KNMI_Wind_Directions.csv',
          row.names = F)
