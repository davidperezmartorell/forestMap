#' Plot the map
#' @param input Input values
#' @param data Data to plot
#' @return NULL No values output
#' @export
#' @examples
#' plotMapForCountry(input,data)
# Funtion plotMapForCountry ------------------------------------------------------
# Render the map
plotMapForCountry <- function(input,data) {
library("dplyr")
library("ggplot2")
library("leaflet")
library("terra")
library("sf")
library("raster")
library("sp")

  summary_data <- data %>%
    group_by(country, exact_lat, exact_long, .groups = 'drop') %>%
    summarise(count_comm = n_distinct(id_comm),
              count_study = n_distinct(id_study)) %>%
    ungroup()

  # Create file name according country
  iso3_values <- data$iso3 %>% unique()
  file <- paste0("inst/gadm/gadm41_", iso3_values, "_1_pk.rds")
  # Load country shapefile
  countryShape <- readRDS(file)
  # Unpack the PackedSpatVector
  countryShape <- terra::unwrap(countryShape)
  # Convert to sf object
  country_sf <- st_as_sf(countryShape)
  # Convert to data frame
  country_df <- as.data.frame(country_sf)

  # Create a leaflet map
  # Extract the Proj4string from the terra CRS
  terra_crs <- terra::crs(countryShape)
  

  # Create file name according country
  file <- paste0("inst/elevations/", iso3_values, "_elv_msk.tif")
  # Read the elevation data from the TIFF file
  elevation_data <- raster(file)
  # Convert elevation data to a data frame
  elevation_df <- rasterToPoints(elevation_data)
  # Convert matrix to data frame
  elevation_df <- as.data.frame(elevation_df)
  # Now, check the structure and class
  browser()
  # Get the min and max elevation values
  min_elevation <- min(elevation_df[[3]], na.rm = TRUE)
  max_elevation <- max(elevation_df[[3]], na.rm = TRUE)
  
  
  # Create a ggplot
  ggplot() +
    geom_raster(data = as.data.frame(elevation_df), aes(x = x, y = y, fill = elevation_df[[3]])) +
    geom_sf(data = country_df, aes(geometry = geometry), fill = "transparent", color = "black", size = 2) +
    geom_point(data = summary_data, aes(x = exact_long, y = exact_lat), color = "red", size = 3) +
    theme_minimal() +
    labs(title = "Elevations", fill = "Elevations") +
    scale_fill_viridis_c(option = "viridis", limits = c(min_elevation, max_elevation))
  
  
}
