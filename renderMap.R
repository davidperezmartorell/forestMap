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
renderMap <- function(input, country, inputData) {
  country <- input$country
  library("dplyr")
  library("ggplot2")
  library("leaflet")
  library("sf")
  library("raster")
  source("renderTittleMap.R"); #Function to plot the tittle of each map

  cat("renderMap.R: Do you want ", country," map\n")
  
    if (country =="All the World")
    {
     cat("renderMap.R: Plot general map into the function\n")
     map<-getGeneralMap(input,inputData)
    }else
    {
      # Create the base map (countryMap)
       cat("renderMap.R: Plot country map into the function\n") 
       countryMap <- getCountryMap(input, inputData)
       return(map)
    }
} #End of renderMap


#Function to construct General map (Mapamundi)
 getGeneralMap<-function(input,inputData){
   browser()
  summary_data <- inputData %>%
    group_by(Country, exact_lat, exact_long) %>%
    summarise(count_comm = n_distinct(id_comm),
              count_study = n_distinct(id_study),
              .groups = 'drop') %>%
    ungroup()
  
  # Create a leaflet map
  cat("renderMap.R: Plot general map into the function\n")
  browser()
  map <- leaflet(data = summary_data) %>%
    addTiles() %>%
    addCircleMarkers(
      lat = ~exact_lat,
      lng = ~exact_long,
      popup = ~paste("Country: ", summary_data$Country, "<br>",
                     "Count Comm: ", summary_data$count_comm, "<br>",
                     "Count Study: ", summary_data$count_study),
      label = ~count_comm,  # You can change this to any variable you want to display as the label
      color = "blue",
      fillOpacity = 0.7,
      radius = 3 + (summary_data$count_comm / max(summary_data$count_comm)) * 10
    )
  return(map)
}

#Function to construct country map (Only country)
 getCountryMap<-function(input,inputData){
  myData <- selectCountry(inputData, input$country)
  summary_data <- myData %>%
    group_by(country, exact_lat, exact_long) %>%
    summarise(count_comm = n_distinct(id_comm),
              count_study = n_distinct(id_study),
              .groups = 'drop') %>%
    ungroup()
  
  # Create a leaflet map
  cat("renderMap.R: Plot general map into the function\n")
  map <- leaflet() %>%
    addTiles() %>%
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
 