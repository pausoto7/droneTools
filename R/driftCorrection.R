# tool for correcting for drone temperature drift 

library(dplyr)
library(readr)
library(raster)
library(round)
library(stringr)
library(lubridate)


# prepare power equation data ----------------------------------------------------

# upload csv file, change col into Posixct file type, and rename cols to reduce issues in code
fieldData_raw <- read_csv("data/exampleDriftData.csv", show_col_types = FALSE)

# Overwrite column names
colnames(fieldData_raw) <- c("DateTime", "RealTemp", "DroneTemp")

# Convert DateTime
fieldData_raw <- fieldData_raw %>%
  mutate(DateTime = ymd_hms(DateTime, tz = "UTC"))

driftStartEnd <- fieldData_raw$DateTime

# max time minus 1 second because log(0) gives an error
minTime <- min(driftStartEnd) - seconds(1)


# add temp diff
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

# prepare imagery 

# convert files from kelvin to celcius

tiff_file_list <- list.files("C:/Users/sotop/Documents/Drone Projects/Cedardownstream_005_Feb2025/thermal_processed_kelvin/11-04-2025 1538-19",
                             full.names = TRUE)

output_location <- "C:/Users/sotop/Documents/Drone Projects/Cedardownstream_005_Feb2025/Thermal_drift_corrected/"

for(files in 1:length(tiff_file_list)){
  
  
  # get drift value -----------------------------------------
  file <- tiff_file_list[files]
  
  file.path <- dirname(file)
  
  only_filename <- basename(file)
  new_filename <- paste0(sub(".TIFF$", "_corrected.tiff", only_filename, ignore.case = TRUE))
  output_filename <- paste0(output_location, new_filename)

  
  tiff_metadata <- exifr::read_exif(file)
  
  raster_datetime <- ymd_hms(tiff_metadata$DateTimeOriginal)
  
  t <- as.numeric(raster_datetime - minTime) 
  
  drift_value <- k*t^b
  
  
  # convert tiff --------------------------------------------
  
  tiff <- raster::raster(file)
  
  thermal_values_k <- values(tiff)
  
  thermal_values_c <- (thermal_values_k/100) - 273.15
  
  corrected_values <- thermal_values_c - drift_value
  
  corrected_raster <- tiff
  
  raster::values(corrected_raster) <- corrected_values
  
  if (!dir.exists(output_location)) {
    dir.create(output_location, recursive = TRUE)
    sprintf("Create output folder located here: %s", output_location)
  }
  
  writeRaster(corrected_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
}

# can't do exponents with negative values ... could pose a problem in artic conditions?



