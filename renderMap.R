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
  #Code to test function
  # assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
  # assembleages_filtered <- dplyr::select(assembleages,"id","id_comm", "id_study", "study_year", "stage", "study_common_taxon_clean", "taxon_level", "exact_lat", "exact_long", "country", "disturbance1_age_clean")
  # assembleages_filtered <- assembleages_filtered %>% unique()
  # inputData<-assembleages_filtered
  # 
  # library("dplyr")
  # library("leaflet")
  # library("sf")
  # library("rnaturalearth")
  
  
  summary_data <- inputData %>%
    group_by(id, country, exact_lat, exact_long, study_year, id_study) %>%
    summarise(
      count_study = n_distinct(id_study),
      stage = first(stage),
      disturbance1_age_clean = first(disturbance1_age_clean),
      study_common_taxon_clean = first(study_common_taxon_clean),
      .groups = 'drop'
    ) %>%
    ungroup()
  
  # Convert 'exact_lat' and 'exact_long' columns to numeric, handling non-numeric values
  summary_data$exact_lat <- as.numeric(summary_data$exact_lat)
  summary_data$exact_long <- as.numeric(summary_data$exact_long)
  
  # Handle any NAs or non-numeric values
  summary_data$exact_lat[is.na(summary_data$exact_lat) | !is.numeric(summary_data$exact_lat)] <- NA
  summary_data$exact_long[is.na(summary_data$exact_long) | !is.numeric(summary_data$exact_long)] <- NA
  summary_data <- filter(summary_data, stage != "0")
  # Load world countries data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  # Set CRS to WGS84
  world <- st_set_crs(world, 4326)  # 4326 is the EPSG code for WGS84
  
  # Mutate inputData based on disturbance1_age_clean
  summary_data <- summary_data %>%
    mutate(disturbance_category = case_when(
      disturbance1_age_clean %in% c("burning", "burning/logging/farming", "burning/farming") ~ "burning",
      disturbance1_age_clean == "mining" ~ "mining",
      disturbance1_age_clean %in% c("logging", "logging/farming", "forest uses", "forest uses/logging") ~ "forest",
      disturbance1_age_clean %in% c("cultivation", "animal farming","farming", "plantation", "plantation/logging") ~ "cultivation",
      TRUE ~ "none"
    ))


  # Create a leaflet map with marker clusters
  # Create a leaflet map with marker clusters
  map <- leaflet() %>%
    addProviderTiles("Esri.WorldImagery") %>%
    addPolygons(
      data = world,
      fillColor = NA,
      color = "black",
      weight = 1,
      group = "Base Map"
    ) %>%
    addCircleMarkers(
      data = summary_data,  # Use summary_data directly
      lat = ~exact_lat,
      lng = ~exact_long,
      layerId = ~id_study,
      radius = 20,
      fillOpacity = 1,
      color = "black",
      fillColor = ~case_when(
        stage == "recovering" ~ "green",
        stage == "reference" ~ "blue",
        stage == "disturbed" ~ "darkred",
        stage == "protection" ~ "yellow"
      ),
      popup = ~paste(
        "Country: ", country, "<br>",
        "Year of study: ", study_year, "<br>",
        "Name of Study group: ", id_study, "<br>",
        "Number of studies: ", count_study, "<br>",
        "Stage: ", stage, "<br>",
        "Study Common Taxon Clean: ", study_common_taxon_clean,
        "Disturbance: ", disturbance1_age_clean
      ),
      group = "Stage Markers",
      clusterOptions = markerClusterOptions()
    ) %>%
    addCircleMarkers(
      data = summary_data,  # Use summary_data directly
      lat = ~exact_lat,
      lng = ~exact_long,
      layerId = ~id_study,
      radius = 20,
      fillOpacity = 1,
      color = "black",
      fillColor = ~case_when(
        disturbance1_age_clean %in% c("burning", "burning/logging/farming", "burning/farming") ~ "pink",
        disturbance1_age_clean == "mining" ~ "darkblue",
        disturbance1_age_clean %in% c("logging", "logging/farming", "forest uses", "forest uses/logging") ~ "brown",
        disturbance1_age_clean %in% c("cultivation", "animal farming","farming", "plantation", "plantation/logging") ~ "white",
        TRUE ~ "darkred"
      ),
      popup = ~paste(
        "Country: ", country, "<br>",
        "Year of study: ", study_year, "<br>",
        "Name of Study group: ", id_study, "<br>",
        "Number of studies: ", count_study, "<br>",
        "Stage: ", stage, "<br>",
        "Study Common Taxon Clean: ", study_common_taxon_clean
      ),
      group = "Disturbance Age Clean Markers",
      clusterOptions = markerClusterOptions()
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("green", "blue", "darkred", "yellow"),
      labels = c("Recovering", "Reference", "Disturbed", "Protection"),
      title = "Stage",
      opacity = 1
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("pink", "darkblue", "brown", "white", "darkred"),
      labels = c("Burning", "Mining", "Logging", "Cultivation", "Others"),
      title = "Disturbance",
      opacity = 1
    )
  
  
  # Add layer control
  map <- map %>%
    addLayersControl(
      baseGroups = c("Base Map"),
      overlayGroups = c("Stage Markers", "Disturbance Age Clean Markers"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  map <- map %>%
    setView(lng = mean(summary_data$exact_long, na.rm = TRUE), 
            lat = mean(summary_data$exact_lat, na.rm = TRUE), 
            zoom = 3)
  
  map
  return(list(map = map, data = summary_data))
}




  
  
  
   