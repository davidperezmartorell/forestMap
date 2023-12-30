#' Plot the map
#' @param input Input values
#' @param country Type of map. General(for mapamundi), country(view coutry borders, elevations, rivers and studies points)
#' @param selectGeneral Calues to plot
#' @return man Return the map we want to plot
#' @export
#' @examples
#' plotAll(input,inputData)
# Funtion plotMap ------------------------------------------------------
# Render the map
renderMap <- function(input, inputData) {
  country <- input$country
  library("dplyr")
  library("ggplot2")
  library("leaflet")
  library("sf")
  library("raster")
  source("renderTittleMap.R"); #Function to plot the tittle of each map

 browser()
  summary_data <- inputData %>%
    group_by(Country, exact_lat, exact_long, study_year, id_study, id_comm) %>%
    summarise(
      count_study = n_distinct(id_study),
      count_comm = n_distinct(id_comm),
      stage = first(stage),  # Assuming it's the same for each group
      study_common_taxon_clean = first(study_common_taxon_clean),  # Assuming it's the same for each group
      .groups = 'drop'
    ) %>%
    ungroup()
  
  # Calculate centroid for each country
  centroid_data <- summary_data %>%
    group_by(Country) %>%
    summarise(
      centroid_lat = mean(exact_lat),
      centroid_long = mean(exact_long),
      total_count_study = sum(count_study),
      total_count_comm = sum(count_comm),
      .groups = 'drop'
    ) %>%
    ungroup()
  
  # Create a leaflet map with marker clusters
  map <- leaflet(data = summary_data) %>%
    addTiles() %>%
    addMarkers(
      lat = ~exact_lat,
      lng = ~exact_long,
      popup = ~paste(
        "Country: ", Country, "<br>",
        "Study Year: ", study_year, "<br>",
        "ID Study: ", id_study, "<br>",
        "ID Comm: ", id_comm, "<br>",
        "Count Study: ", count_study, "<br>",
        "Count Comm: ", count_comm, "<br>",
        "Stage: ", stage, "<br>",
        "Study Common Taxon Clean: ", study_common_taxon_clean
      ),
      clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)
    ) %>%
    addMarkers(
      data = centroid_data,
      lat = ~centroid_lat,
      lng = ~centroid_long,
      popup = ~paste(
        "Country: ", Country, "<br>",
        "Total Count Study: ", total_count_study, "<br>",
        "Total Count Comm: ", total_count_comm
      )
    )
   return(map)
 }



 