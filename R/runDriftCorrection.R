#
#
# Description: 
# script which uses functions in driftCorrection to correct for temperature drift
# in DJI imagery which has been converted on thermoConverter into readable .tiff files
#
#


# set up 

library(dplyr)
library(readr)
library(raster)
library(round)
library(stringr)
library(lubridate)

source("R/droneImageryProcessingFunctions.R")


# OBJECTS --------------------------------------

# path to drift csv data that is laid out in | DateTime | Real Temp | Drone Temp | columns 
# DateTime should be laid out as yyyy-mm-dd hh:mm:ss in csv for best result with processing
drift_data_csv <- "data/exampleDriftData.csv"

# path where rasters are located
raster_filepath <- "C:/Users/sotop/Documents/Drone Projects/Cedardownstream_005_Feb2025/thermal_processed_kelvin/11-04-2025 1538-19"

# path where you want corrected rasters to be output - will create a folder if it doesn't already exist

output_location <- "C:/Users/sotop/Documents/Drone Projects/Cedardownstream_005_Feb2025/Thermal_drift_corrected"



correctTemperatureDrift(drift_data_csv, raster_filepath, output_location )
  

