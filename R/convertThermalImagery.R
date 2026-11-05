library(reticulate)

# Convert raw DJI thermal R-JPEGs (_T.JPG) into float32 Celsius TIFFs using
# dji-thermal-cli. GPS, timestamps and DJI flight/gimbal tags are kept, so the
# output goes straight into correctTemperatureDrift().
#
# The first run sets up a private Python environment ("thermal-tools") and
# installs dji-thermal-cli into it. Later runs reuse it.
#
# Settings left as NULL keep the values the camera embedded in each image.
#   emissivity      0.1 to 1 (water is roughly 0.98; the camera default is usually 0.95)
#   distance        metres to the target, or "lrf" (laser rangefinder, the default) or "embedded"
#   humidity        percent, e.g. 60
#   ambient_temp    air temperature in degrees C
#   reflected_temp  degrees C (often set equal to ambient_temp)

convertThermalImages <- function(input_dir,
                                 output_dir,
                                 emissivity = NULL,
                                 distance = NULL,
                                 humidity = NULL,
                                 ambient_temp = NULL,
                                 reflected_temp = NULL,
                                 envname = "thermal-tools") {
  
  if (!dir.exists(input_dir)) stop("input_dir doesn't exist: ", input_dir, call. = FALSE)
  
  jpgs <- list.files(input_dir, pattern = "\\.jpe?g$", ignore.case = TRUE)
  if (length(jpgs) == 0) {
    stop("No .JPG files found in ", input_dir,
         ". Point input_dir at the folder with the raw _T.JPG thermal images.", call. = FALSE)
  }
  message("Found ", length(jpgs), " JPEG(s) in ", input_dir)
  
  # One-time setup -----------------------------------------------------------
  if (!virtualenv_exists(envname)) {
    message("First run: setting up Python and dji-thermal-cli (a few minutes, one time only)...")
    starter <- virtualenv_starter()
    if (is.null(starter)) {
      message("No Python found, installing one...")
      starter <- install_python()
    }
    virtualenv_create(envname, python = starter)
    py_install("dji-thermal-cli", envname = envname, pip = TRUE)
  }
  py <- virtualenv_python(envname)
  
  # Build the command --------------------------------------------------------
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  args <- c("-m", "dji_thermal_cli", "process",
            "--input-dir",  shQuote(normalizePath(input_dir)),
            "--output-dir", shQuote(normalizePath(output_dir)),
            "--output", "temp_c:tiff")
  
  settings <- list("--emissivity" = emissivity, "--distance" = distance,
                   "--humidity" = humidity, "--ambient-temp" = ambient_temp,
                   "--reflected-temp" = reflected_temp)
  for (flag in names(settings)) {
    if (!is.null(settings[[flag]])) args <- c(args, flag, as.character(settings[[flag]]))
  }
  
  # Run ----------------------------------------------------------------------
  message("Converting to Celsius TIFFs...")
  out <- suppressWarnings(system2(py, args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  
  # Show problem files and the summary line, not the full log
  problems <- grep("^\\[(error|skip|fail)", out, value = TRUE, ignore.case = TRUE)
  if (length(problems)) message(paste(problems, collapse = "\n"))
  done_line <- grep("^Done\\.", out, value = TRUE)
  if (length(done_line)) message(done_line)
  
  if (!is.null(status) && status != 0) {
    stop("dji-thermal-cli stopped with an error:\n", paste(tail(out, 15), collapse = "\n"),
         call. = FALSE)
  }
  
  # Check the results --------------------------------------------------------
  tiffs <- list.files(output_dir, pattern = "_temp_c\\.tiff?$",
                      full.names = TRUE, ignore.case = TRUE)
  if (length(tiffs) == 0) {
    stop("No Celsius TIFFs were written. dji-thermal-cli said:\n",
         paste(tail(out, 15), collapse = "\n"), call. = FALSE)
  }
  
  tags <- tryCatch(
    suppressMessages(exiftoolr::exif_read(tiffs, tags = c("DateTimeOriginal", "GPSLatitude"))),
    error = function(e) {
      message("Couldn't check the TIFF metadata because exiftool isn't set up ",
              "(the TIFFs were still written). correctTemperatureDrift() needs it, so run:\n",
              "  install.packages(\"exiftoolr\"); exiftoolr::install_exiftool()")
      NULL
    })
  if (!is.null(tags)) {
    no_time <- !"DateTimeOriginal" %in% names(tags) || anyNA(tags$DateTimeOriginal)
    no_gps  <- !"GPSLatitude" %in% names(tags) || anyNA(tags$GPSLatitude)
    if (no_time) warning("Some TIFFs have no timestamp; drift correction will skip them.")
    if (no_gps)  warning("Some TIFFs have no GPS tags. Did the original JPEGs have GPS?")
    if (!no_time && !no_gps) message("Timestamps and GPS present on all TIFFs.")
  }
  
  message("Wrote ", length(tiffs), " Celsius TIFF(s) to ", output_dir)
  invisible(tiffs)
}

# Example, full workflow:
# source("convertThermalImages.R")
# source("correctTemperatureDrift.R")
#

convertThermalImages(
  input_dir  = "C:/Users/sotop/Documents/thermalImagery/TIR",
  output_dir = "C:/Users/sotop/Documents/thermalImagery/TIR_celsius"
  # emissivity = 0.98, distance = 25, ambient_temp = 18, reflected_temp = 18
)

# drift_log <- correctTemperatureDrift(
#   drift_data_csv  = "C:/Users/sotop/Documents/thermalImagery/bucket_calibration.csv",
#   raster_filepath = "C:/Users/sotop/Documents/thermalImagery/TIR_celsius",
#   output_location = "C:/Users/sotop/Documents/thermalImagery/TIR_corrected"
# )