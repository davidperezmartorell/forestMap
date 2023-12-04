#' Plot the map
#' @param input Input values
#' @param data Data to plot
#' @return NULL No values output
#' @export
#' @examples
#' plotMap(input,data)
# Funtion Loaddata ------------------------------------------------------
# Render the map
plotMap <- function(input,data) {
library("dplyr")
library("ggplot2")
library("leaflet")
  
  # Filter out rows with NA in the country column
  data <- data %>% filter(!is.na(country))
  data <- data %>% filter(!is.na(exact_lat) & !is.na(exact_long))
  browser()


  # Create a leaflet map
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      data = data,
      lat = ~exact_lat,
      lng = ~exact_long,
      popup = ~id_study,
      label = ~study_year
    )
}




