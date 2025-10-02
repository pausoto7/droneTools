#
# Purpose: script that separates and renames raw RBG and thermal images from drones
#
# Author: PS
#
# Last updated: October 3, 2024
#
# Contact for issues: paula.soto@dfo-mpo.gc.ca

library(exifr)
library(fs)


#Set up --------------------------------------------------------------------------

# Set the working directory (necessary because this script is not set as an R Project)

wk_dir <- "D:/Coquitlam/20250818_Coquitlam_SiteVisit_JRR/Drone/Raw/Thermal Photogrammetry/DJI_202508181034_035_CoquitlamSouth20250818"


# ORIGINAL_IMAGERY
# list folder where all the raw T & RGB drone images are that you want to separate 
# (even if they're in multiple folders within this folder this will work)



#Thermal separation ----------------------------------------------------------------------

separateImageryTypes <- function(wk_dir, original_imagery){
  
  #Name that thermal folder will have
  thermal_raw_folder <- paste0(wk_dir, "/Thermal_raw")
  #Name that RGB folder will have
  RGB_raw_folder <- paste0(wk_dir, "/RGB")
  
  # Create the "Thermal_raw" folder if it doesn't exist
  if (!dir_exists(thermal_raw_folder)) {
    dir_create(thermal_raw_folder)
  }
  
  # Create the "RBG" folder if it doesn't exist
  if (!dir_exists(RGB_raw_folder)) {
    dir_create(RGB_raw_folder)
  }
  
  # list file names to be copied
  T_files_to_copy <- list.files(path = file.path(wk_dir, original_imagery), 
                                pattern = "T\\.JPG$", 
                                full.names = TRUE, 
                                recursive = TRUE)
  
  # list file names to be copied
  RGB_files_to_copy <- list.files(path = file.path(wk_dir, original_imagery), 
                                  pattern = "V\\.JPG$", 
                                  full.names = TRUE, 
                                  recursive = TRUE)
  
  
  # copy T files
  file_copy(T_files_to_copy, thermal_raw_folder, overwrite = TRUE)
  
  # copy RGB files
  file_copy(RGB_files_to_copy, RGB_raw_folder, overwrite = TRUE)
  
  
}


separateImageryTypes(wk_dir, "")



# TO RENAME FILES:  --------------------------------------------------------


# if you want to change name of your files, you can do that below. 
# An example is as follows: 
# file_base_name <- "Coq_Riv_Aug24" would mean your file name output will be
# "Coq_Riv_Aug24_0001_T.JPG" for thermal and  "Coq_Riv_Aug24_0001_V.JPG" for RGB

# select your files base name
file_base_name <- "Coq_Riv_Aug18_25_S"


# Get a list of all the image files
# **NOTE: If you are transferring any images that aren't taken by a DJI drone you may need to modify the "pattern" below
  # Essentially you want to choose a pattern characteristics that your newly named images won't have.
  # That way the loop doesn't go on forever renaming the same images
  # For example, a typical DJI imager naming convention is as follows DJI_20240916103804_0002_T.JPG
  # Hence, I have chosen DJI* to be the pattern for DJI drones

raw_naming_pattern <- "DJI*"


rename_files <- function(raw_folder, raw_naming_pattern){
  
  image_files <- list.files(path = raw_folder, 
                              pattern = raw_naming_pattern, 
                              full.names = TRUE)
  
  # Loop through the image files and rename them with a sequential number
  for (i in seq_along(image_files)) {
    # Create the new file name with a padded sequence number (e.g., 0001, 0002, etc.)
    new_name <- sprintf("%s_%04d_V.jpg", file_base_name, i)
    
    # Get the full path for the new file name
    new_path <- file.path(raw_folder, new_name)
    
    # Rename the file
    file_move(image_files[i], new_path)
  }
  
  
}

# rename thermal files
rename_files(thermal_raw_folder, raw_naming_pattern)

# rename RGB files
rename_files(RGB_raw_folder, raw_naming_pattern)


# # Rename Thermal ---------------------------------------------
# T_image_files <- list.files(path = thermal_raw_folder, 
#                               pattern = raw_naming_pattern, 
#                               full.names = TRUE)
# 
# # Loop through the image files and rename them with a sequential number
# for (i in seq_along(T_image_files)) {
#   # Create the new file name with a padded sequence number (e.g., 0001, 0002, etc.)
#   new_name <- sprintf("%s_%04d_V.jpg", file_base_name, i)
#   
#   # Get the full path for the new file name
#   new_T_path <- file.path(thermal_raw_folder, new_name)
#   
#   # Rename the file
#   file_move(T_image_files[i], new_T_path)
# }


# RENAME RGB ---------------------------------------------
RBG_image_files <- list.files(path = RGB_raw_folder, 
                              pattern = raw_naming_pattern, 
                              full.names = TRUE)

# Loop through the image files and rename them with a sequential number
for (i in seq_along(RBG_image_files)) {
  # Create the new file name with a padded sequence number (e.g., 0001, 0002, etc.)
  new_name <- sprintf("%s_%04d_V.jpg", file_base_name, i)
  
  # Get the full path for the new file name
  new_V_path <- file.path(RGB_raw_folder, new_name)
  
  # Rename the file
  file_move(RBG_image_files[i], new_V_path)
}


# function which gets minimum and maximum dates and times that drone was flown. 
get_datetime_maxmin <- function(raw_folder){
  
  # GET THERMAL MAX/MIN DATETIME --------------------------------------
  all_thermal_files <- list.files(raw_folder, full.names = TRUE)
  
  # Read EXIF metadata from all the image files
  metadata <- read_exif(all_thermal_files)
  
  # Extract the 'DateTimeOriginal' field, which contains the timestamps
  date_times <- metadata$DateTimeOriginal
  
  # Convert the 'DateTimeOriginal' to a datetime format (if not already in that format)
  date_times <- as.POSIXct(date_times, format="%Y:%m:%d %H:%M:%S")
  
  # Find the minimum and maximum date-times
  min_time <- min(date_times, na.rm = TRUE)
  max_time <- max(date_times, na.rm = TRUE)
  
  return(data.frame(min = min_time, max = max_time))
  
}

# get thermal images max/min date
get_datetime_maxmin(thermal_raw_folder)

# get RGB images max/min date
get_datetime_maxmin(RGB_raw_folder)









