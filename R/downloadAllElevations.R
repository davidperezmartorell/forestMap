#' Download elevations for each country
#' @param NULL Input values
#' @return NULL No values output
#' @export
#' @examples
#' downloadAllElevations()
# Funtion downloadAllElevations ------------------------------------------------------
# Download info from https://github.com/rspatial/geodata/issues/
# It's available in geodata library
downloadAllElevations <- function() {
  library("readr")
  library("geodata")
  # Load World elevations
   elevation_tiff <- "inst/wc2.1_2.5m_elev.tif"
   # Read the elevation data from the TIFF file
   elevation_data <- raster(elevation_tiff)

  
  #Load shape files
   # Set the folder path
    folder_path <- "inst/gadm"
   # Get a list of all files in the folder
    files <- list.files(folder_path, full.names = TRUE)
   # Create an empty list to store the processed maps
    processed_maps <- list()
  

    
  #For each file, download their elevations map  
  # Loop through each file
  for (file in files) {
    #Get country code from file
    country_code <- sub(".*/gadm41_(\\w+)_\\d+_pk\\.rds", "\\1", file)
    # Read the map file (replace this with your actual map reading function)
    map <- readRDS(file)
    #convert map in 
    country_sf <- st_as_sf(map)
    #Take limits altitud y longitud
    bbox <- st_bbox(country_sf)
    x1 = bbox[1]
    y1 = bbox[4]
    x2 = bbox[3]
    y2 = bbox[2]
    
    
    #Cut elevationswith map borders
    elevation <- elevation_30s(country = country_code,lat = y1, lon = x1, path = "inst/elevations", clip = "none")
    
    # Store the processed map in the list
     processed_maps[[file]] <- elevation
    
    # Create a filename based on the desired pattern
     filename <- paste0("elevations_", country_code, "_30.tif")
    
    # Save the elevation map with the desired filename
     writeRaster(elevation, filename, overwrite = TRUE)
     

  }
cat("Downloading elevations maps for all countries\n")

}
