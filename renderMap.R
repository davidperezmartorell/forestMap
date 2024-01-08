#' Plot the map
#' @param input Input values
#' @param selectGeneral Calues to plot
#' @return man Return the map we want to plot
#' @export
#' @examples
#' plotAll(input,inputData)
# Funtion plotMap ------------------------------------------------------
# Render the map
  renderMap <- function(inputData) {
  
  
    library("dplyr")
    library("ggplot2")
    library("leaflet")
    library("sf")
    library("raster")
    source("renderTittleMap.R"); #Function to plot the tittle of each map
    # Create a reactive expression for popup information
    popupInfo <- reactiveVal(NULL)
  
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
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(
        lat = ~exact_lat,
        lng = ~exact_long,
        layerId = ~id_comm,  # Set layerId to a unique identifier, e.g., id_comm
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
      )
 
    return(list(map = map, data = map_data))
   }
  
  
  
   