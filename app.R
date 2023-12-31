# Install the 'here' package if you haven't already
# install.packages("here")

#roxygen2::roxygenise()
#runApp()


cat("app.R:  Loading libraries\n")
source("loadLibraries.R");loadLibraries() # Load all libraries
source("loadData.R"); #Load all data
source("fillCountryAccordingLatLon.R"); #Join more info about each country
source("footer.R"); #Foot of page with information about creators, contact and versions
source("renderMap.R"); #Function to plot the maps
source("generateTableText.R"); #To write contents when we navigate the map

#Load data
 cat("app.R:  Loading all data\n")
 data <- loadData()
 
 taxon <- data$taxon
 cat("app.R: Loaded ", nrow(taxon), "values from taxon\n")
 assembleages <- data$assembleages
 cat("app.R: Loaded ", nrow(assembleages), "values from assembleages\n")
 index <- data$index
 cat("app.R: Indexed from both studies ", nrow(index), " values with id_study as main index\n")
 index_filtered <- index %>% unique()
 cat("app.R: Indexed from both studies ", nrow(index_filtered), " after filter\n")
 
 #Fill empties values for countries and receive other interesting info from each one.
 cat("app.R: Check error in countries and receive ISO3 code\n")
 assembleages <- fillCountryAccordingLatLon(assembleages)
 cat("app.R: There are", length(unique(assembleages$Country)), "different countries.\n")

 
 #Select interesting values from both dataframes  
 # Select values from assembleages based on index
 cat("loadData.R: Filterting assembleages \n")
 assembleages_filtered <- assembleages %>% filter(id_study %in% index$id_study)
 assembleages_filtered <- dplyr::select(assembleages_filtered, "id_study", "study_year", "stage", "study_common_taxon_clean", "taxon_level", "exact_lat", "exact_long", "Country", "ISO3")
 assembleages_filtered <- assembleages_filtered %>% unique()
 
 # Select values from taxon based on index
 cat("loadData.R: Filterting taxon \n")
 taxon_filtered <- taxon %>% filter(id_study %in% index$id_study)
 taxon_filtered <- taxon_filtered %>% dplyr::select(id_comm, id_study,taxon_level , rank)
 taxon_filtered <- taxon_filtered %>% unique()
 
 
 #Main information after filters
 cat("app.R: Filtering the data we need to reduce dataframe size \n")
 cat("app.R: After filter, there are loaded ", nrow(taxon_filtered), "values from taxon\n")
 head(taxon_filtered)

 cat("app.R: After filter, there are loaded ", nrow(assembleages_filtered), "values from assembleages\n")
 head(assembleages_filtered)
 
 #Select info to show in World map
   # Merge taxon_filtered and assemblages_filtered on id_study
    dataWorldMap <- merge(taxon_filtered, assembleages_filtered, by = "id_study")
   # Merge merged_df and index on id_study
    dataWorldMap <- merge(dataWorldMap, index_filtered, by = "id_study")

 # Define a reactive value to store the map
  mapToPlot <- reactiveVal()
  map <- reactiveVal()
 
  # Menu --------------------------------------------------------------------
 # UI
  ui <- fluidPage(
    useShinyjs(), 
    
    tags$head(
      tags$style(HTML("body, html {height: 100%;margin: 0;} #map {height: 100%;} ")),
      tags$script(src = "https://unpkg.com/leaflet-providers@1.12.0/leaflet-providers.js"),
      tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet@1.7.1/dist/leaflet.css")
    ),
    
    titlePanel(HTML("<h3>Plot maps from particular inventory with studies and inventories</h3>")),
    
    tabsetPanel(
      tabPanel("Map", 
               fluidRow(
                 column(12, htmlOutput("TittleMap")),
                 column(12, leafletOutput("map")),
                 column(12, div(id = "belowTitle", HTML("<h3>Below Title</h3>")))
               )
      ),
      
      tabPanel("Footer",
               mainPanel(
                 footer()
               )
      )
    )
  )
  
  
 
 

  
# Server
server <- function(input, output,session) {
  browser()
  # Observe click events on the map
  observeEvent(input$map_marker_click, {
    # Extract information from the clicked marker
    click <- input$map_marker_click
    if (!is.null(click)) {
      # Generate and update the table text
      tableText <- generateTableText(dataWorldMap)
      
      # Update the content of the belowTitle div with the table text
      shinyjs::html("belowTitle", tableText)
      
    }
  })
  
  output$clickedInfo <- renderPrint({
    # Print the clicked popup information
    print(clickedIconData$data)
  })
  
  # Call function to create my map and plot it
  output$map <- renderLeaflet({
    map <- renderMap(dataWorldMap)  
    if (!is.null(map)) {
      map
    }
  })
}#End of server function

shinyApp(ui, server)
  