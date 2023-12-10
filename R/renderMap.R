#' Plot the map
#' @param input Input values
#' @param option Type of map. General(for mapamundi), country(view coutry borders, elevations, rivers and studies points)
#' @param selectGeneral Calues to plot
#' @return man Return the map we want to plot
#' @export
#' @examples
#' plotAll(input,option,inputData)
# Funtion plotMap ------------------------------------------------------
# Render the map
renderMap <- function(input,option,inputData) {
  library("dplyr")
  library("ggplot2")
  library("leaflet")
  if (option=="general")
    {
     summary_data <- inputData %>%
      group_by(country, CapitalLatitude, CapitalLongitude) %>%
      summarise(count_comm = n_distinct(id_comm),
                count_study = n_distinct(id_study),
                .groups = 'drop') %>%
      ungroup()
  
    # Create a leaflet map
    cat("Plot general map into the function\n")
    map <- leaflet(data = summary_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~CapitalLatitude,
        lng = ~CapitalLongitude,
        popup = ~paste("Country: ", summary_data$country, "<br>",
                       "Count Comm: ", summary_data$count_comm, "<br>",
                       "Count Study: ", summary_data$count_study),
        label = ~count_comm,  # You can change this to any variable you want to display as the label
        color = "blue",
        fillOpacity = 0.7,
        radius = 3 + (summary_data$count_comm / max(summary_data$count_comm)) * 10
      )
    
    return(map)
  }
 else if (option=="country")
    {
    cat("Plot country map into the function\n")
    library("dplyr")
    library("ggplot2")
    library("leaflet")
    library("terra")
    library("sf")
    library("raster")
    library("sp")
    myData <- selectCountry(inputData,input$country)
    summary_data <- myData %>%
      group_by(country, exact_lat, exact_long) %>%
      summarise(count_comm = n_distinct(id_comm),
                count_study = n_distinct(id_study),
                .groups = 'drop') %>%
      ungroup()
    
    # Create file name according country
    iso3_values <- myData$iso3 %>% unique()
    file <- paste0("inst/gadm/gadm41_", iso3_values, "_1_pk.rds")
    # Load country shapefile
    countryShape <- readRDS(file)
    # Unpack the PackedSpatVector
    countryShape <- terra::unwrap(countryShape)
    # Convert to sf object
    country_sf <- st_as_sf(countryShape)
    # Convert to data frame
    country_df <- as.data.frame(country_sf)
    # Convert country_df to sf
    country_sf <- st_as_sf(country_df)
    
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
    
    # Get the min and max elevation values
    min_elevation <- 0
    max_elevation <- max(elevation_df[[3]], na.rm = TRUE)
    
    # Define your custom color palette
    custom_palette <- c( "green","burlywood1","darkgoldenrod1","darkgoldenrod4" ,"brown", "darkgray", "azure4")
    # Create a ggplot
    map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = country_sf, fill = "transparent", color = "black", weight = 2) %>%
      addCircleMarkers(
        data = summary_data,
        lat = ~exact_lat,
        lng = ~exact_long,
        popup = ~paste("Country: ", summary_data$country, "<br>",
                       "Count Comm: ", summary_data$count_comm, "<br>",
                       "Count Study: ", summary_data$count_study),
        label = ~count_comm,
        color = "red",
        fillOpacity = 0.7,
        radius = 3
      )
    
    
    return(map)
 }
  
    


} #End of renderMap
