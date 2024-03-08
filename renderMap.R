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
  library("leaflet")
  library("sf")
  library("rnaturalearth")
  
  # Code to test function
  # assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
  # assembleages_filtered <- dplyr::select(assembleages, "id", "id_comm", "id_study", "study_year", "stage", "study_common_taxon_clean", 
  #                                        "taxon_level", "exact_lat", "exact_long", "country", "disturbance1_age_clean", "age", "n_comm_available",
  #                                        "metric", "citation", "database")
  # assembleages_filtered <- assembleages_filtered %>% unique()
  # inputData <- assembleages_filtered

  inputData2 <- inputData %>%
    group_by(id_study) %>%
    mutate(
      possible_alternatives = {
        stages <- unique(stage[order(age, decreasing = TRUE)])
        if ("reference" %in% stages) {
          stages <- c("reference", stages[stages != "reference"])
        }
        paste(stages, collapse = ", ")
      }
    ) %>%
    ungroup() %>%
    select(-id, -age, -disturbance1_age_clean, -stage)
  
  
  # Convert 'exact_lat' and 'exact_long' columns to numeric, handling non-numeric values
  inputData2$exact_lat <- as.numeric(inputData2$exact_lat)
  inputData2$exact_long <- as.numeric(inputData2$exact_long)
  
  # Load world countries data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  # Set CRS to WGS84
  world <- st_set_crs(world, 4326)  # 4326 is the EPSG code for WGS84
  
  
  summary_data <- inputData2 %>%
    group_by(id_study) %>%
    summarise(
      count_study = n_distinct(id_study),
      possible_alternatives = first(possible_alternatives),
      study_common_taxon_clean = first(study_common_taxon_clean),
      metric = first(metric),
      citation = first(citation),
      database = first(database),
      n_comm_available = n_distinct(n_comm_available),
      exact_lat = first(exact_lat),  # Include exact_lat column
      exact_long = first(exact_long),  # Include exact_long column
      study_year = first(study_year),
      .groups = 'drop'
    )
  
  summary_data$exact_lat <- as.numeric(summary_data$exact_lat)
  summary_data$exact_long <- as.numeric(summary_data$exact_long)
  

  # Create a leaflet map with marker clusters
  map <- leaflet(data = summary_data) %>%
    addProviderTiles("Esri.WorldImagery") %>%
    addCircleMarkers(
      lat = ~exact_lat,
      lng = ~exact_long,
      layerId = ~id_study,
      radius = 10,
      fillColor = ~case_when(
        possible_alternatives == "recovering" ~ "green",
        possible_alternatives == "reference, recovering" ~ "orange",
        possible_alternatives == "recovering, disturbed" ~ "purple",
        possible_alternatives == "reference, recovering, disturbed" ~ "brown"
      ),
      fillOpacity = 1,
      color = "black",
      stroke = TRUE,
      popup = ~paste(
        "Year: ", study_year, "<br>",
        "Number of assemblages: ", n_comm_available, "<br>",
        "Study: ", id_study, "<br>",
        "Stage through the time: ", possible_alternatives, "<br>",
        "Common taxon: ", study_common_taxon_clean, "<br>",
        "Metric: ", metric, "<br>",
        "Source: ", citation, "<br>",
        "Origin database: ", database, "<br>"
      ),
      clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("green", "orange", "purple", "brown"),
      labels = c("Recovering", "Reference, Recovering", "Recovering, Disturbed", "Reference, Recovering, Disturbed"),
      title = "Stages",
      opacity = 1
    )
  
  
  
  
  
  map <- map %>%
    setView(lng = mean(summary_data$exact_long, na.rm = TRUE), 
            lat = mean(summary_data$exact_lat, na.rm = TRUE), 
            zoom = 3) 

  return(list(map = map, data = summary_data))
}








  
  
  
   