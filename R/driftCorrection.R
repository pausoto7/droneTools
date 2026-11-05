library(readr)
library(dplyr)
library(lubridate)
library(terra)
library(exiftoolr)   # install.packages("exiftoolr"); exiftoolr::install_exiftool() if you don't have exiftool


#TEMPORARY TIMEZONE FIX ----------------------------------------------------
  # Older R versions don't recognise Windows' new "British Columbia Standard Time"
  # zone (BC moved to permanent UTC-7 in 2026) and throw "unknown timezone" warnings.
  # After updating R: delete this line, restart R, and run Sys.timezone().
  # No warning means the new R handles it and this fix isn't needed.
  Sys.setenv(TZ = "America/Vancouver")
# ---------------------------------------------------------------------------




correctTemperatureDrift <- function(drift_data_csv,
                                    raster_filepath,
                                    output_location,
                                    tz = "UTC",
                                    fit_points = c("all", "first_two")) {
  
  fit_points <- match.arg(fit_points)
  
  # Prepare drift model ------------------------------------------------------
  
  # Read DateTime as text so readr doesn't guess, then parse it ourselves
  fieldData_raw <- read_csv(drift_data_csv, col_types = "cdd")
  colnames(fieldData_raw) <- c("DateTime", "RealTemp", "DroneTemp")
  raw_times <- fieldData_raw$DateTime
  
  # Accepts 2024-05-03 12:05:25, and what Excel tends to save it as:
  # 5/3/2024 12:05:25, 5/3/2024 12:05, 5/3/2024 12:05:25 PM. Month/day order is assumed.
  fieldData_raw$DateTime <- suppressWarnings(parse_date_time(
    raw_times,
    orders = c("Ymd HMS", "Ymd HM", "mdY HMS", "mdY HM", "mdY IMS p", "mdY IM p"),
    tz = tz))
  
  bad <- is.na(fieldData_raw$DateTime)
  if (any(bad)) {
    stop("Couldn't read these DateTime values in the calibration CSV:\n  ",
         paste(head(raw_times[bad], 5), collapse = "\n  "),
         "\nUse the format YYYY-MM-DD HH:MM:SS, e.g. 2024-05-03 12:05:25 ",
         "(Excel often changes this when it saves).", call. = FALSE)
  }
  if (anyNA(fieldData_raw$RealTemp) || anyNA(fieldData_raw$DroneTemp)) {
    stop("Some RealTemp/DroneTemp values are blank or not numbers in the calibration CSV.",
         call. = FALSE)
  }
  message("Calibration window: ", format(min(fieldData_raw$DateTime)), " to ",
          format(max(fieldData_raw$DateTime)), " (", nrow(fieldData_raw), " readings)")
  
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
    # "-xmp" copies the whole XMP block as-is, so DJI and converter tags exiftool
    # can't write one by one (BandName, GpsStatus, UTCAtExposure...) still come across.
    exif_call(args = c("-TagsFromFile", file, "-all:all", "-xmp", "-overwrite_original"),
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



drift_log <- correctTemperatureDrift(
  drift_data_csv  = "C:/Users/sotop/Documents/thermalImagery/bucket_calibration.csv",
  raster_filepath = "C:/Users/sotop/Documents/thermalImagery/TIR",
  output_location = "C:/Users/sotop/Documents/thermalImagery/TIR_corrected",
  tz = "UTC"
) 
