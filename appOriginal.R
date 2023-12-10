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
 
  # Menu --------------------------------------------------------------------
  ui <- fluidPage(
    
    useShinyjs(), # Agregar esta función para usar shinyjs
    tags$head( #Encabezado del navegador
      tags$title("Mapas de todo el mundo")
    ),
    
    tags$style(
      HTML(" 
    h3 {
      color: red;
      font-size: 24px;
      font-weight: bold;
    }
    h6 {
      color: blue;
      font-size: 16px;
      font-weight: bold;
    }
    select, .selectize-input, .selectize-dropdown {
      border: 1px solid gray;
      padding: 2px;
      font-size: 12px;
      font-family: Arial;
      border-radius: 2px;
      #width: 70%;
      margin-top: 1px;margin-left: 1px;margin-right: 1px;margin-bottom: 1px;
    }
    .sidebar {
       text-align: left;
       max-width: 100px;
    }
    .well {
     width: 80%;
    }
  ")
    )
    
    ,
    
    #Encabezado del codigo con un titulo
    titlePanel(HTML("<h3>Forest map based in studies around the world</h3>")),
    sidebarLayout(
      # Panel de herramientas lateral
      sidebarPanel(
        width = 4,
        fluidRow(
          column(12,selectInput("country", "Select country", choices = c("All the World", unique(selectGeneral$country)), selected = "All the World"),# "All the World"),
            textOutput("Select country")
          ),
        ),
        fluidRow(
          radioButtons(
            inputId = "options",
            label = "Options",
            choices = c("biomap", "richness", "abund"),
            selected = "biomap"
          )
        ),
        # Boton para resetear contenido
        actionButton("reset", "reset")
      ), # Fin de sidebarPanel
      
      # Carga cuerpo ------------------------------------------------------------
      mainPanel(
        htmlOutput("TittleMap"),
        plotMap(input, selectGeneral),
        footer() 
      ) #Fin de sidebarLayout
    )# Fin del main pannel
  ) #Fin de fluidPage  

  
  # Server
  server <- function(input, output, session) {

   # Create a reactive value to track changes in the input$country
   observeEvent(input$country, {
     cat("something has changed in country option\n")
     # Check if the selected country is "All the World"
     if (input$country == "All the World") {
       cat("Plotting World map\n")
       plotMap(input, selectGeneral)
     } else {
       cat("Plotting ", input$country, " map\n")
       dataSelectedCountry <- selectCountry(selectGeneral, input$country)
       plotMapForCountry(input, dataSelectedCountry)
     }
   })
    
    # Call the function with the required arguments
    observeResetButton(session, input)
}


  shinyApp(ui, server)
  