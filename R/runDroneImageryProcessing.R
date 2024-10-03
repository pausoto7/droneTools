#
#
# Description: 
# script which uses functions in droneImageryProcessing to set up folders and naming  
# for use in thermal imagery conversion and processing 
#
#


#Set up --------------------------------------------------------------------------
library(exifr)
library(xfun)
library(fs)

# read functions in to script
source("R/droneImageryProcessingFunctions.R")

# Set the directory where you are running that drone project out of
wk_dir <- "C:/Users/sotop/Documents/Technical Projects/2024/CoquitlamThermal"


# RUN TO SEPARATE RGB AND THERMAL FILES -------------------------------------------

# Folder name where all the raw T & RGB drone images are that you want to separate - Location should be within working folder listed above
# (even if they're in multiple folders within this folder this will work)

original_imagery <- "Original"

# run function which separates the imagery into two folders: thermal_raw and RGB
separateImageryTypes(wk_dir, folder_name, original_imagery)

# RUN BELOW TO RENAME FILES ----------------------------------------------------------------------------------------------

# if you want to change name of your files, you can do that below. 
# An example is as follows: 
# file_base_name <- "Coq_Riv_Aug24" 
# would mean your file name output will be
# "Coq_Riv_Aug24_0001_T.JPG" for thermal and  "Coq_Riv_Aug24_0001_V.JPG" for RGB

# select your files base name
file_base_name <- "Coq_Riv_Aug24"


# This pattern will be used for the selection, and then renaming of your files. 
# **NOTE: If you are transferring any images that aren't taken by a DJI drone you may need to modify the "pattern" below
# Essentially you want to choose a pattern characteristics that your newly named images won't have.
# For example, a typical DJI image naming convention is as follows DJI_20240916103804_0002_T.JPG
# Hence, the raw_naming_pattern chosen below is DJI*

raw_naming_pattern <- "DJI*"

# rename thermal files
rename_files(paste0(wk_dir, "/", "Thermal_raw"),
             file_base_name, 
             raw_naming_pattern)

# rename RGB files
rename_files(paste0(wk_dir, "/", "RGB"), 
             file_base_name, 
             raw_naming_pattern)

# MAX/MIN DATE TIMES--------------------------------------------------------------

# get thermal images max/min date
get_datetime_maxmin(paste0(wk_dir, "/", "Thermal_raw"))

# get RGB images max/min date
get_datetime_maxmin(paste0(wk_dir, "/", "RGB"))

# NUMBER OF IMAGES CHECK --------------------------------------------------------- 

number_of_images(paste0(wk_dir, "/", "Thermal_raw"), paste0(wk_dir, "/", "RGB"))

