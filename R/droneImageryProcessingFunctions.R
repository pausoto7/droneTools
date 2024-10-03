#-------------------------------------------------------------------------------
# Purpose: set of functions to help with conversion and processing of drone 
# RBG and thermal imagery
#
# Author: PS
#
# Last updated: October 3, 2024
#
# Contact for issues: paula.soto@dfo-mpo.gc.ca
# ------------------------------------------------------------------------------



# function which separates thermal and RBG imagery and puts them into folders 
# called thermal_raw and RBG

separateImageryTypes <- function(wk_dir, folder_name, original_imagery){
  
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
  
  cat(sprintf("Seperation complete! Images can be found in \n%s and \n%s file locations", thermal_raw_folder, RGB_raw_folder))
  
}

# function that can be used to rename all files 

rename_files <- function(raw_folder, file_base_name, raw_naming_pattern){
  
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
  
  cat(sprintf("Files renamed! Check here to see: %s", raw_folder))
  
}



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


# # RENAME RGB ---------------------------------------------
# RBG_image_files <- list.files(path = RGB_raw_folder, 
#                               pattern = raw_naming_pattern, 
#                               full.names = TRUE)
# 
# # Loop through the image files and rename them with a sequential number
# for (i in seq_along(RBG_image_files)) {
#   # Create the new file name with a padded sequence number (e.g., 0001, 0002, etc.)
#   new_name <- sprintf("%s_%04d_V.jpg", file_base_name, i)
#   
#   # Get the full path for the new file name
#   new_V_path <- file.path(RGB_raw_folder, new_name)
#   
#   # Rename the file
#   file_move(RBG_image_files[i], new_V_path)
# }



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

number_of_images <- function(thermal_raw_folder, RGB_raw_folder) {
  # List all image files in the thermal folder
  thermal_images <- list.files(path = thermal_raw_folder, pattern = "\\.jpg$", full.names = TRUE)
  
  # List all image files in the RGB folder
  rgb_images <- list.files(path = RGB_raw_folder, pattern = "\\.jpg$", full.names = TRUE)
  
  # Get the number of images in each folder
  num_thermal_images <- length(thermal_images)
  num_rgb_images <- length(rgb_images)
  
  # Print the number of images in each folder
  cat("Number of images in Thermal folder:", num_thermal_images, "\n")
  cat("Number of images in RGB folder:", num_rgb_images, "\n")
  
  # Compare the numbers and print whether they are the same or not
  if (num_thermal_images == num_rgb_images) {
    cat("Both folders have the same number of images.\n")
  } else {
    cat("The folders have a different number of images.\n")
  }
}






