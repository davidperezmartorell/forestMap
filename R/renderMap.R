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
  library("leaflet.extras")
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
   library("leaflet")
   library("sf")
   library("raster")
   
   myData <- selectCountry(inputData, input$country)
   summary_data <- myData %>%
     group_by(country, exact_lat, exact_long) %>%
     summarise(count_comm = n_distinct(id_comm),
               count_study = n_distinct(id_study),
               .groups = 'drop') %>%
     ungroup()
   
   # Create file name according to country
   iso3_values <- myData$iso3 %>% unique()
   
   # Load country shapefile
   file_country <- paste0("inst/gadm/gadm41_", iso3_values, "_1_pk.rds")
   countryShape <- readRDS(file_country)
   
   # Unpack the PackedSpatVector and convert to sf object
   country_sf <- st_as_sf(unwrap(countryShape))
   
   # Create file name according to country
   file_elevation <- paste0("inst/elevations/", iso3_values, "_elv_msk.tif")
   
   # Read the elevation data from the TIFF file
   elevation_data <- raster(file_elevation)
   
   # Check if the raster has values
   if (length(elevation_data[]) == 0) {
     stop("Elevation raster has no values.")
   }
   
   # Get the min and max elevation values
   min_elevation <- max(0, min(elevation_data[], na.rm = TRUE))
   max_elevation <- max(elevation_data[], na.rm = TRUE)
   
   # Define your custom color palette
   custom_palette <- c("#008000", "#DEB887", "#B8860B", "#8B4513", "#A52A2A", "#A9A9A9", "#EEEEEE")
   

   # Create a leaflet map
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
     ) %>%
     addRasterImage(elevation_data, colors = custom_palette, opacity = 0.7)
   

   #Create the legend
     # Calculate the interval values
     legend_values <- seq(min_elevation, max_elevation, length.out = 7)
     
     # Define a custom legend
     legend_html <- paste0(
       "<div id='legend' class='leaflet-control leaflet-control-custom legend'>",
       "<p><strong>Elevation (m)</strong></p>"
     )
     
     # Add legend entries based on the interval values
     for (i in seq_along(legend_values)) {
       legend_html <- paste0(
         legend_html,
         "<p><i style='background:", custom_palette[i], "'></i> ", round(legend_values[i]), "</p>"
       )
     }
     legend_html <- paste0(legend_html, "</div>")
   
   # Add the legend
   map <- map %>% addControl(html = legend_html,position = "bottomright")
   
   return(map)
   
   
   
   
   
   
   
   
 }
  
    


} #End of renderMap
