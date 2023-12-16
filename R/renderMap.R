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
renderMap <- function(input, option, inputData, showElevation = FALSE, showRivers = FALSE, showBorders = FALSE) {
  library("dplyr")
  library("ggplot2")
  library("leaflet")
  library("sf")
  library("raster")
  
  cat("renderMap.R: Do you want ", option," map\n")
  cat("renderMap.R: Buttons pressed are elevations=",showElevation, " rivers= ",showRivers, " and borders= ",showBorders,"\n")
    if (option=="general")
    {
     cat("renderMap.R: Plot general map into the function\n")
     map<-getGeneralMap(input,option,inputData)
    }

    if ((option=="country") & (input$showElevation == FALSE) & (input$showBorders == FALSE) & (input$showRivers == FALSE))
    {
       cat("renderMap.R: Plot country map into the function\n")
       map<-getCountryMap(input,option,inputData)
    }
  
    if ((option!="general") & (input$showElevation == TRUE) & (input$showBorders == FALSE) & (input$showRivers == FALSE))
    {
        cat("renderMap.R: Plot country map into the function\n")
        map<-getElevationMap(input,option,inputData)
    }
  
    if ((option!="general") & (input$showElevation == FALSE) & (input$showBorders == TRUE) & (input$showRivers == FALSE))
    {
        cat("renderMap.R: Plot country map into the function\n")
        map<-getBorderMap(input,option,inputData) 
    }
    
    if ((option!="general") & (input$showElevation == FALSE) & (input$showBorders == FALSE) & (input$showRivers == TRUE))
    {
        cat("renderMap.R: Plot country map into the function\n")
        cat("renderMap.R: NOTE:IT'S PENDING TO CREATE RIVERS MAP, ONLY OPTION AVAILABLE COPY OF BORDERS\n")
        map<-getRiverMap(input,option,inputData) 
    } 

    if ((option!="general") & (input$showElevation == TRUE) & (input$showBorders == TRUE) & (input$showRivers == FALSE))
    {
      cat("renderMap.R: Plot elevations and rivers map into the function\n")
      # Call the other functions to get individual maps
      countryMap <- getCountryMap(input, option, inputData)
      elevationMap <- getElevationMap(input, option, inputData)
      borderMap <- getBorderMap(input, option, inputData)
      browser()
      # Combine the layers into a single map
      map <- addRasterImage(countryMap, elevationMap)
      map <- addRasterImage(map, borderMap)
      browser()
    }  
  
  
  

  return(map)
  
} #End of renderMap


#Function to construct General map (Mapamundi)
getGeneralMap<-function(input,option,inputData){
  summary_data <- inputData %>%
    group_by(country, CapitalLatitude, CapitalLongitude) %>%
    summarise(count_comm = n_distinct(id_comm),
              count_study = n_distinct(id_study),
              .groups = 'drop') %>%
    ungroup()
  
  # Create a leaflet map
  cat("renderMap.R: Plot general map into the function\n")
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

#Function to construct country map (Only country)
  getCountryMap<-function(input,option,inputData){
  myData <- selectCountry(inputData, input$country)
  summary_data <- myData %>%
    group_by(country, exact_lat, exact_long) %>%
    summarise(count_comm = n_distinct(id_comm),
              count_study = n_distinct(id_study),
              .groups = 'drop') %>%
    ungroup()
  
  # Create file name according to country
  iso3_values <- myData$iso3 %>% unique()
  
  
  # Create a leaflet map
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
  
  #Function to construct elevation map (Only elevations)
  getElevationMap<-function(input,option,inputData){
    myData <- selectCountry(inputData, input$country)
    summary_data <- myData %>%
      group_by(country, exact_lat, exact_long) %>%
      summarise(count_comm = n_distinct(id_comm),
                count_study = n_distinct(id_study),
                .groups = 'drop') %>%
      ungroup()
    
    # Create file name according to country
    iso3_values <- myData$iso3 %>% unique()
    
    
    # Create a leaflet map
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
  
    # Code to render the map with elevation
    cat("renderMap.R: Let's plot elevations layer\n")
    # Load country shapefile
    file_country <- paste0("inst/gadm/gadm41_", iso3_values, "_1_pk.rds")
    countryShape <- readRDS(file_country)
    
    # Unpack the PackedSpatVector and convert to sf object
    country_sf <- st_as_sf(terra::unwrap(countryShape))
    
    # Create file name according to country
    file_elevation <- paste0("inst/elevations/", iso3_values, "_elv_msk.tif")
    
    # Read the elevation data from the TIFF file
    elevation_data <- raster(file_elevation)
    
    
    # Get the min and max elevation values
    min_elevation <- max(0, min(elevation_data[], na.rm = TRUE))
    max_elevation <- max(elevation_data[], na.rm = TRUE)
    
    # Define your custom color palette
    custom_palette <- c("#221133","#00A000", "#aaff00","#ddff00", "#DEB887", "#B8860B", "#8B4513", "#A52A2A", "#A9A9A9", "#EEEEEE")
    
    #This is to reduce detail in case of warning "Raster image too large" only for Brazil map. If more countries, try with catch error or add more files in this exception
    
    if(file_country=="inst/gadm/gadm41_BRA_1_pk.rds"){
      cat("renderMap.R: Catched Brazil error. Map so large- Let's reduce map\n")
      elevation_data <- aggregate(elevation_data, fact = 10, fun = mean)
    }
    
    map <- addRasterImage(map, elevation_data, colors = custom_palette, opacity = 0.7)
    
    #Create the legend
    # Calculate the interval values
    legend_values <- seq(min_elevation, max_elevation, length.out = 10)
    
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
  
  #Function to construct border map (Only borders)
  getBorderMap<-function(input,option,inputData){
    myData <- selectCountry(inputData, input$country)
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
    
    # Create file name according to country
    iso3_values <- myData$iso3 %>% unique()
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
      ) 
    return(map)
  }

#Function to construct river map (Only river)
getRiverMap<-function(input,option,inputData){
  myData <- selectCountry(inputData, input$country)
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
  
  # Create file name according to country
  iso3_values <- myData$iso3 %>% unique()
  # Create a leaflet map
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = country_sf, fill = "transparent", color = "blue", weight = 2) %>%
    addCircleMarkers(
      data = summary_data,
      lat = ~exact_lat,
      lng = ~exact_long,
      popup = ~paste("Country: ", summary_data$country, "<br>",
                     "Count Comm: ", summary_data$count_comm, "<br>",
                     "Count Study: ", summary_data$count_study),
      label = ~count_comm,
      color = "blue",
      fillOpacity = 0.7,
      radius = 3
    ) 
  return(map)
}