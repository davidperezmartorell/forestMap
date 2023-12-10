#' Plot the map
#' @param input Input values
#' @param data Data to plot
#' @return NULL No values output
#' @export
#' @examples
#' plotMap(input,data)
# Funtion plotMap ------------------------------------------------------
# Render the map
plotMap <- function(input,data) {
library("dplyr")
library("ggplot2")
library("leaflet")

  summary_data <- data %>%
    group_by(country, CapitalLatitude, CapitalLongitude) %>%
    summarise(count_comm = n_distinct(id_comm),
              count_study = n_distinct(id_study),
              .groups = 'drop') %>%
    ungroup()

  # Create a leaflet map
  leaflet(data = summary_data) %>%
    addTiles() %>%
    addCircleMarkers(
      lat = ~CapitalLatitude,
      lng = ~CapitalLongitude,
      popup = ~paste("Country: ", country, "<br>",
                     "Count Comm: ", count_comm, "<br>",
                     "Count Study: ", count_study),
      label = ~count_comm,  # You can change this to any variable you want to display as the label
      color = "blue",
      fillOpacity = 0.7,
      radius = 3 + (summary_data$count_comm / max(summary_data$count_comm)) * 10
    )
}