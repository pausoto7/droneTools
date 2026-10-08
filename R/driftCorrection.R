# ---------------------------------------------
# Purpose: tool for correcting for drone temperature drift 
# 
# Author: Paula Soto
#
# Last updated: April 14, 2025
#
# ---------------------------------------------


library(readr)
library(dplyr)
library(lubridate)
library(terra)
library(exiftoolr)   

correctTemperatureDrift <- function(drift_data_csv,
                                    raster_filepath,
                                    output_location,
                                    tz = "UTC",
                                    fit_points = c("all", "first_two")) {
  
  fit_points <- match.arg(fit_points)
  
  # Prepare drift model ------------------------------------------------------
  
  fieldData_raw <- read_csv(drift_data_csv, show_col_types = FALSE)
  colnames(fieldData_raw) <- c("DateTime", "RealTemp", "DroneTemp")
  fieldData_raw <- fieldData_raw %>%
    mutate(DateTime = ymd_hms(DateTime, tz = tz))
  
  # Start time minus 1 second because log(0) is undefined
  minTime <- min(fieldData_raw$DateTime) - seconds(1)
  
  fieldData <- fieldData_raw %>%
    mutate(
      departures = DroneTemp - RealTemp,
      # Force seconds. Plain subtraction picks units automatically (secs/mins/hours)
      timePassed = as.numeric(difftime(DateTime, minTime, units = "secs"))
    ) %>%
    arrange(timePassed)
  
  if (any(fieldData$timePassed <= 0)) stop("Non-positive time values found!")
  
  if (fit_points == "first_two") fieldData <- slice(fieldData, 1:2)
  
  # log() needs positive departures
  fit_data <- filter(fieldData, departures > 0)
  if (nrow(fit_data) < nrow(fieldData)) {
    warning(nrow(fieldData) - nrow(fit_data),
            " calibration point(s) had departures <= 0 and were dropped from the fit.")
  }
  if (nrow(fit_data) < 2) stop("Need at least 2 points with positive departures to fit the model.")
  
  # Fit departures = k * t^b as a straight line in log-log space.
  # With exactly 2 points this gives the same k and b as the original script.
  fit <- lm(log(departures) ~ log(timePassed), data = fit_data)
  k <- exp(coef(fit)[[1]])
  b <- coef(fit)[[2]]
  message(sprintf("Drift model: departure = %.4f * t^%.4f (t in seconds since %s)",
                  k, b, format(minTime)))
  
  # Process rasters ----------------------------------------------------------
  
  files <- list.files(raster_filepath, pattern = "\\.tiff?$",
                      full.names = TRUE, ignore.case = TRUE)
  # Never re-correct outputs if the input and output folders are the same
  files <- files[!grepl("_corrected\\.tiff?$", files, ignore.case = TRUE)]
  if (length(files) == 0) stop("No .tif/.tiff files found in ", raster_filepath)
  
  dir.create(output_location, showWarnings = FALSE, recursive = TRUE)
  
  results <- vector("list", length(files))
  
  for (i in seq_along(files)) {
    file <- files[i]
    out_file <- file.path(
      output_location,
      sub("\\.(tiff?)$", "_corrected.\\1", basename(file), ignore.case = TRUE)
    )
    
    # Image timestamp
    dto <- exif_read(file, tags = "DateTimeOriginal")$DateTimeOriginal
    if (is.null(dto) || is.na(dto)) {
      warning("No DateTimeOriginal in ", basename(file), ", skipping.")
      next
    }
    img_time <- ymd_hms(dto, tz = tz)
    t <- as.numeric(difftime(img_time, minTime, units = "secs"))
    
    if (t <= 0) {
      warning(basename(file), " was taken before the calibration start, skipping.")
      next
    }
    
    drift_value <- k * t^b
    
    # Apply correction (values already in Celsius)
    r <- suppressWarnings(rast(file))   # silences "unknown extent" for non-georeferenced images
    r_corr <- r - drift_value
    
    # PROFILE=BASELINE stops GDAL from writing fake GeoTIFF tags into a plain image
    writeRaster(r_corr, out_file, overwrite = TRUE,
                datatype = "FLT4S", gdal = c("PROFILE=BASELINE"))
    
    aux <- paste0(out_file, ".aux.xml")
    if (file.exists(aux)) file.remove(aux)
    
    # Copy EXIF, XMP and GPS from the original onto the corrected file.
    # Structural TIFF tags (size, bit depth, etc.) are not copied, so the new pixel data stays intact.
    exif_call(args = c("-TagsFromFile", file, "-all:all", "-overwrite_original"),
              path = out_file, quiet = TRUE)
    
    results[[i]] <- data.frame(
      file = basename(file),
      image_time = img_time,
      seconds_since_start = t,
      drift_removed_C = drift_value
    )
  }
  
  summary_df <- bind_rows(results)
  
  # Quick check that geolocation made it across
  out_files <- list.files(output_location, pattern = "_corrected\\.tiff?$",
                          full.names = TRUE, ignore.case = TRUE)
  gps <- exif_read(out_files, tags = c("GPSLatitude", "GPSLongitude"))
  if (!all(c("GPSLatitude", "GPSLongitude") %in% names(gps)) ||
      any(is.na(gps$GPSLatitude))) {
    warning("Some corrected files are missing GPS tags. Check whether the originals had them.")
  } else {
    message("GPS tags present on all corrected files.")
  }
  
  message("Finished correcting ", nrow(summary_df), " rasters.")
  invisible(summary_df)
}

# Example:
# drift_log <- correctTemperatureDrift(
#   drift_data_csv  = "bucket_calibration.csv",
#   raster_filepath = "thermal_tiffs/",
#   output_location = "thermal_tiffs_corrected/",
#   tz = "UTC"
# )








































# correctTemperatureDrift <- function(drift_data_csv, raster_filepath, output_location) {
#   
#   # Prepare equation data ----------------------------------------------------
#   
#   # Upload CSV, convert DateTime, and rename columns
#   fieldData_raw <- read_csv(drift_data_csv, show_col_types = FALSE)
#   colnames(fieldData_raw) <- c("DateTime", "RealTemp", "DroneTemp")
#   fieldData_raw <- fieldData_raw %>%
#     mutate(DateTime = ymd_hms(DateTime, tz = "UTC"))
#   
#   # Start time minus 1 second because log(0) is undefined
#   minTime <- min(fieldData_raw$DateTime) - seconds(1)
#   
#   # Calculate departures and timePassed
#   fieldData <- fieldData_raw %>%
#     mutate(departures = DroneTemp - RealTemp) %>%
#     mutate(timePassed = as.numeric(DateTime - minTime), .before = DateTime)
#   
#   if (any(fieldData$timePassed <= 0)) stop("Non-positive time values found!")
#   
#   # Log transformation of first two points
#   log_t1 <- log(fieldData$timePassed[1])
#   log_t2 <- log(fieldData$timePassed[2])
#   log_y1 <- log(fieldData$departures[1])
#   log_y2 <- log(fieldData$departures[2])
#   
#   # Fit decay model: departures = k * t^b
#   b <- (log_y2 - log_y1) / (log_t2 - log_t1)
#   y1 <- fieldData$departures[1]
#   t1 <- fieldData$timePassed[1]
#   k <- y1 / (t1^b)
#   
#   # Process rasters ----------------------------------------------------------
#   
#   tiff_file_list <- list.files(raster_filepath, full.names = TRUE)
#   
#   for (file in tiff_file_list) {
#     
#     only_filename <- basename(file)
#     new_filename <- paste0(sub(".TIFF$", "_corrected.tiff", only_filename, ignore.case = TRUE))
#     output_filename <- file.path(output_location, new_filename)
#     
#     # Read image timestamp from metadata
#     tiff_metadata <- exifr::read_exif(file)
#     raster_datetime <- ymd_hms(tiff_metadata$DateTimeOriginal)
#     
#     # Compute time and drift
#     t <- as.numeric(raster_datetime - minTime) 
#     drift_value <- k * t^b
#     
#     # Read raster and apply drift correction (already in Celsius)
#     tiff <- raster::raster(file)
#     corrected_values <- raster::values(tiff) - drift_value
#     raster::values(tiff) <- corrected_values
#     
#     # Create output folder if necessary
#     if (!dir.exists(output_location)) {
#       dir.create(output_location, recursive = TRUE)
#       message(sprintf("Created output folder: %s", output_location))
#     }
#     
#     # Save corrected raster
#     writeRaster(tiff, filename = output_filename, format = "GTiff", overwrite = TRUE)
#   }
#   
#   print("Finished correcting rasters")
# }
# 


# VERSION WITH KELVIN CORRECTIONS IN IT TO ACCOMODATE THERMOCONVERTER

# 
# correctTemperatureDrift <- function(drift_data_csv, raster_filepath, output_location ){
#   
#   # prepare  equation data ----------------------------------------------------
#   
#   # upload csv file, change col into Posixct file type, and rename cols to reduce issues in code
#   fieldData_raw <- read_csv(drift_data_csv, show_col_types = FALSE)
#   
#   # Overwrite column names
#   colnames(fieldData_raw) <- c("DateTime", "RealTemp", "DroneTemp")
#   
#   # Convert DateTime
#   fieldData_raw <- fieldData_raw %>%
#     mutate(DateTime = ymd_hms(DateTime, tz = "UTC"))
#   
#   # start time minus 1 second because log(0 seconds) is mathematically undefined
#   minTime <- min(fieldData_raw$DateTime) - seconds(1)
#   
#   # add temp diffs and time passed in seconds
#   fieldData <- fieldData_raw %>%
#     mutate(departures = DroneTemp-RealTemp) %>%
#     mutate(timePassed =  as.numeric(DateTime - minTime), .before = DateTime)
#   
#   # Log of first and second timePassed
#   log_t1 <- log(fieldData$timePassed[1])
#   log_t2 <- log(fieldData$timePassed[2])
#   
#   # Log of first and second departures
#   log_y1 <- log(fieldData$departures[1])
#   log_y2 <- log(fieldData$departures[2])
#   
#   # Calculate b
#   b <- (log_y2 - log_y1) / (log_t2 - log_t1)
#   
#   
#   # check that all time values are positive
#   if (any(fieldData$timePassed <= 0)) stop("Non-positive time values found!")
#   
#   # Use the first observation
#   y1 <- fieldData$departures[1]
#   t1 <- fieldData$timePassed[1]
#   
#   # Calculate k
#   k <- y1 / (t1^b)
#   
#   # modify imagery values  ----------------------------------------------------
#   
# 
#   tiff_file_list <- list.files(raster_filepath,
#                                full.names = TRUE)
#   
#   # loop through each raster
#   for(files in 1:length(tiff_file_list)){
#     
#     # set up file input and output names
#     file <- tiff_file_list[files]
#     file.path <- dirname(file)
#     only_filename <- basename(file)
#     new_filename <- paste0(sub(".TIFF$", "_corrected.tiff", only_filename, ignore.case = TRUE))
#     output_filename <- paste0(output_location, "/", new_filename)
#     
#     # get variables needed for calculations------------------
#     tiff_metadata <- exifr::read_exif(file)
#     
#     raster_datetime <- ymd_hms(tiff_metadata$DateTimeOriginal)
#     
#     # calculate time passed
#     t <- as.numeric(raster_datetime - minTime) 
#     
#     # calculate drift value for this raster
#     drift_value <- k*t^b
# 
#     # convert tiff --------------------------------------------
#     
#     tiff <- raster::raster(file)
#     
#     thermal_values_k <- values(tiff)
#     
#     # convert from kelvin to celcius as ThermoConverter outputs in Kelvin
#     thermal_values_c <- (thermal_values_k/100) - 273.15
#     
#     corrected_values <- thermal_values_c - drift_value
#     
#     corrected_raster <- tiff
#     
#     raster::values(corrected_raster) <- corrected_values
#     
#     # if directory doesn't exist create it
#     if (!dir.exists(output_location)) {
#       dir.create(output_location, recursive = TRUE)
#       sprintf("Create output folder located here: %s", output_location)
#     }
#     
#     # write raster with corrected values in Celsius
#     writeRaster(corrected_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)
#     
#   }
#   
#   print("Finished correcting rasters")
#   
#   
# }
# 




