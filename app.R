# Install the 'here' package if you haven't already
# install.packages("here")

#roxygen2::roxygenise()
#runApp()

# Load the 'here' package
library("here") #Load files
library("dplyr")  #Work with dataframes
library("ggplot2") #Plot maps
library("leaflet") #Plot maps
library("shiny") #To shiny menus and frames
library("shinyjs") #Special options from shiny
library("raster")
library("sf")  #To add buttons in menu


# Load all libraries
# Load libraries
library_path <- here::here("R", "loadLibraries.R")
source(library_path)

# Select main information
library_path <- here::here("R", "selectGeneral.R")
source(library_path)

# Source the LoadData.R file
library_path <- here::here("R", "loadData.R")
source(library_path)

# Source the plotMap.R file
library_path <- here::here("R", "plotMap.R")
source(library_path)

#Load data
 cat("Loading all data\n")
 data <- loadData()
 taxon <- data$taxon
 assemblages <- data$assemblages
 index <- data$index

#Load shapes
 if (length(list.files("inst/gadm")) == 0) { #Check if map folder is empty or not. If it's empty, load shapes
   cat("No map loaded. Let's download shapes in inst/gadm")
   downloadAllShapes()
 } else{cat("All maps seems downloaded\n")}
  
 #Main information
 selectGeneral <- selectGeneral(index,assemblages,taxon)  # Index main values  to plot general map
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
                            choices = c("All the World", unique(selectGeneral$country)),
                            selected = "All the World"),
                textOutput("Select country")
         )
       ),
       fluidRow(
         column(width = 12,
                checkboxInput("showElevation", "Elevation", value = FALSE),
                checkboxInput("showRivers", "Rivers", value = FALSE),
                checkboxInput("showBorders", "Borders", value = FALSE)
                ),
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
    # Enable shinyjs
    shinyjs::hide("showElevation")
    shinyjs::hide("showRivers")
    shinyjs::hide("showBorders")
    # Observe changes in showElevation and showRivers
    observe({
      if (input$country != "All the World") {
          shinyjs::show("showRivers")
          shinyjs::show("showElevation")
          shinyjs::show("showBorders")
        }
    })
      
      

    # Create a reactive value to track changes in the input$country
    observeEvent(c(input$country, input$showElevation, input$showRivers, input$showBorders), {
      cat("something has changed in country option\n")
      # Check if the selected country is "All the World"
      if (input$country == "All the World") {
        cat("Plotting World map\n")
        myMapToPlot <- renderMap(input, "general", selectGeneral)
      } else {
        cat("Plotting ", input$country, " map\n")
        dataSelectedCountry <- selectCountry(selectGeneral, input$country)
        # Check if we want a clean map, add rivers, or add elevations layers
        if (input$showElevation) {
          # Handle when showElevation is TRUE
          myMapToPlot <- renderMap(input, "elevation", dataSelectedCountry)
        } else if (input$showRivers) {
          # Handle when showRivers is TRUE
          myMapToPlot <- renderMap(input, "rivers", dataSelectedCountry)
        } else if (input$showBorders) {
          # Handle when showRivers is TRUE
          myMapToPlot <- renderMap(input, "borders", dataSelectedCountry)
        } else {
          # Handle the default case when neither showElevation nor showRivers is TRUE
          myMapToPlot <- renderMap(input, "country", dataSelectedCountry)
        }
      }
      mapToPlot(myMapToPlot)  # Store the map in the reactive value
    })

    # Call the function with the required arguments
    observeResetButton(session, input)
    
    # aquí va el código para que acctualice
    run_code <- function() {
      flush.console()
    }
    
    # Función para renderizar el título del mapa
    output$TittleMap <- renderText({
      run_code()
    })
    

    # Use this function in your server.R
    output$map <- renderLeaflet({
      map <- mapToPlot()  # Retrieve the stored map
      if (!is.null(map)) {
        map
      }
    })
}#End of server function

shinyApp(ui, server)
  