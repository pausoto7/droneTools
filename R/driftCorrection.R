# ---------------------------------------------
# Purpose: tool for correcting for drone temperature drift 
# 
# Author: Paula Soto
#
# Last updated: April 14, 2025
#
# ---------------------------------------------




correctTemperatureDrift <- function(drift_data_csv, raster_filepath, output_location ){
  
  # prepare  equation data ----------------------------------------------------
  
  # upload csv file, change col into Posixct file type, and rename cols to reduce issues in code
  fieldData_raw <- read_csv(drift_data_csv, show_col_types = FALSE)
  
  # Overwrite column names
  colnames(fieldData_raw) <- c("DateTime", "RealTemp", "DroneTemp")
  
  # Convert DateTime
  fieldData_raw <- fieldData_raw %>%
    mutate(DateTime = ymd_hms(DateTime, tz = "UTC"))
  
  # start time minus 1 second because log(0 seconds) is mathematically undefined
  minTime <- min(fieldData_raw$DateTime) - seconds(1)
  
  # add temp diffs and time passed in seconds
  fieldData <- fieldData_raw %>%
    mutate(departures = DroneTemp-RealTemp) %>%
    mutate(timePassed =  as.numeric(DateTime - minTime), .before = DateTime)
  
  # Log of first and second timePassed
  log_t1 <- log(fieldData$timePassed[1])
  log_t2 <- log(fieldData$timePassed[2])
  
  # Log of first and second departures
  log_y1 <- log(fieldData$departures[1])
  log_y2 <- log(fieldData$departures[2])
  
  # Calculate b
  b <- (log_y2 - log_y1) / (log_t2 - log_t1)
  
  
  # check that all time values are positive
  if (any(fieldData$timePassed <= 0)) stop("Non-positive time values found!")
  
  # Use the first observation
  y1 <- fieldData$departures[1]
  t1 <- fieldData$timePassed[1]
  
  # Calculate k
  k <- y1 / (t1^b)
  
  # modify imagery values  ----------------------------------------------------
  

  tiff_file_list <- list.files(raster_filepath,
                               full.names = TRUE)
  
  # loop through each raster
  for(files in 1:length(tiff_file_list)){
    
    # set up file input and output names
    file <- tiff_file_list[files]
    file.path <- dirname(file)
    only_filename <- basename(file)
    new_filename <- paste0(sub(".TIFF$", "_corrected.tiff", only_filename, ignore.case = TRUE))
    output_filename <- paste0(output_location, "/", new_filename)
    
    # get variables needed for calculations------------------
    tiff_metadata <- exifr::read_exif(file)
    
    raster_datetime <- ymd_hms(tiff_metadata$DateTimeOriginal)
    
    # calculate time passed
    t <- as.numeric(raster_datetime - minTime) 
    
    # calculate drift value for this raster
    drift_value <- k*t^b

    # convert tiff --------------------------------------------
    
    tiff <- raster::raster(file)
    
    thermal_values_k <- values(tiff)
    
    # convert from kelvin to celcius as ThermoConverter outputs in Kelvin
    thermal_values_c <- (thermal_values_k/100) - 273.15
    
    corrected_values <- thermal_values_c - drift_value
    
    corrected_raster <- tiff
    
    raster::values(corrected_raster) <- corrected_values
    
    # if directory doesn't exist create it
    if (!dir.exists(output_location)) {
      dir.create(output_location, recursive = TRUE)
      sprintf("Create output folder located here: %s", output_location)
    }
    
    # write raster with corrected values in Celsius
    writeRaster(corrected_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)
    
  }
  
  print("Finished correcting rasters")
  
  
}





