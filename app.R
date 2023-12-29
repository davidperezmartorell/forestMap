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

 
   # Print the first few rows of the final merged dataframe
   head(final_merged_df)


 # Define a reactive value to store the map
  mapToPlot <- reactiveVal()
 
  # Menu --------------------------------------------------------------------
 # UI
 ui <- fluidPage(
   useShinyjs(), # Agregar esta función para usar shinyjs
   # Encabezado del código con un título
   titlePanel(HTML("<h3>Plot maps from particular inventory with studies and inventories</h3>")),
   sidebarLayout(
     sidebarPanel(
       width = 4, 
       # Aquí escogemos el país según datos de archivo y tabla países
       fluidRow(
         column(width = 12,
                selectInput("country", label = "Select country", 
                            choices = c("All the World", as.character(unique(assembleages$ADMIN))),
                            selected = "All the World"),
                textOutput("Select country")
         )
       ),
       # Botón para resetear contenido
       actionButton("reset", "reset")
     ), # Fin de sidebarPanel
     mainPanel(
       htmlOutput("TittleMap"),
       leafletOutput("map"),
       footer() 
       # plotAll(input, option, selectGeneral)
     )
   ) # Fin de UI
 )
 
 

  
# Server
server <- function(input, output,session) {

  
  #Call function to create my map and plot it
  output$map <- renderLeaflet({
    map <- renderMap(input, country, dataWorldMap)  
    if (!is.null(map)) {
      map
    }
  })
}#End of server function

shinyApp(ui, server)
  