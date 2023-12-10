# Install the 'here' package if you haven't already
# install.packages("here")

#roxygen2::roxygenise()
#runApp()

# Load the 'here' package
library("here")
library("dplyr")
library("ggplot2")
library("leaflet")
library("shiny")
library("shinyjs")


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
   titlePanel(HTML("<h3>Mapas de todo el mundo patrocinado por https://ecosistemaglobal.org/ y Asociacion Focazul</h3>")),
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

   # Create a reactive value to track changes in the input$country
     observeEvent(input$country, {
       cat("something has changed in country option\n")
       # Check if the selected country is "All the World"
       if (input$country == "All the World") {
         cat("Plotting World map\n")
         myMapToPlot <- renderMap(input,"general",selectGeneral)
       } else {
         cat("Plotting ", input$country, " map\n")
         dataSelectedCountry <- selectCountry(selectGeneral, input$country)
         myMapToPlot <- renderMap(input,"country",dataSelectedCountry)
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
  