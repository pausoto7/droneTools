# This script allows you to apply a calibration function to stitched raster images
# By: Dave Reid
# Version 1.0
# Feb 25 2025

#INSTRUCTIONS
#-------------------------------------------------------------------
## 1. load the terra package. You may need to install it first
install.packages("terra")
library(terra)
library(tidyverse)

# load excel sheet
temp_values_sheet <- read.csv("D:/Coquitlam/Coquitlam_July18/CoquitlamRiver_20250718_Temp_withValues.csv")

full_df <- data.frame()  # initialize an empty dataframe

# sort flight locations and get m & b for each location
for (sites in unique(temp_values_sheet$Site)) {
  
  temp_value_sheet_filter <- temp_values_sheet %>%
    filter(Site == sites)
  
  # run regression (actual ~ drone for calibration)
  res <- lm(actual_temp ~ drone_temp, data = temp_value_sheet_filter)
  
  # store results
  df <- data.frame(Site = sites, m = round(coef(res)[["drone_temp"]], 2), b = round(coef(res)[["(Intercept)"]], 2))
  full_df <- rbind(full_df, df)
}

#-------------------------------------------------------------------
## 2. Set the working directory to the folder you have the raster in. 
# you can paste the directory in from windows explorer but you have to change the backslashes (\) to forward slashes (/)
#setwd("C:/Users/reidd/Documents/pix4d/CedarUpper_thermal/4_index/reflectance/tiles/")

#-------------------------------------------------------------------
## 3. add the name of the raster file to be calibrated

#input_raster_name<-"CedarUpper_thermal_noalpha_reflectance_grayscale_1_1.tif"

input_raster_name <- file.choose()   # opens a file browser

#-------------------------------------------------------------------
## 4. Create a file name for the calibrated raster to be exported
output_raster_name<- sub("\\.tif$", "_CALIBRATED.img", input_raster_name)

#-------------------------------------------------------------------
## 5. Specify the calibration parameters. 
# (this assumes a linear y = mx+b relationship)
# slope of the calibration function

# print table of calibration params for use in next section
print(full_df)

m <- 0.5
# Intercept of the function
b <- 7.30

#-------------------------------------------------------------------
## 6. load the raster file you would like to calibrate
original_raster <- rast(input_raster_name)

#-------------------------------------------------------------------
## 7. Plot the original raster to verify that it is loaded. 
# There is a chance the plot might look super strange, don't worry about it
plot(original_raster)

#-------------------------------------------------------------------
## 8. apply the calibration function to the original raster and create a new raster
calibrated_raster <- original_raster*m+b

# getting rid of NA's because google said sometimes they cause problems
calibrated_raster[!is.finite(calibrated_raster[])] <- NA

#-------------------------------------------------------------------
## 9. export the calibrated raster with the new name to the same folder as the original one
writeRaster(calibrated_raster, output_raster_name, overwrite=TRUE)
plot(calibrated_raster)
