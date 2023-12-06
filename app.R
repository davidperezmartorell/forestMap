# Install the 'here' package if you haven't already
# install.packages("here")

# Load the 'here' package
library("here")
library("dplyr")
library("ggplot2")
library("leaflet")

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
 data <- loadData()
 taxon <- data$taxon
 assemblages <- data$assemblages
 index <- data$index
 
 #Main information
 selectGeneral <- selectGeneral(index,assemblages,taxon)  # Index main values  to plot general map
 selectGeneral <- getCapital(selectGeneral)               # Add capital of each contry, latitude and longitude

 # UI
  ui <- fluidPage(
    titlePanel("Species Map"),
    
    sidebarLayout(
      sidebarPanel(
        # Add UI components for taxon level, species, and study year selection
        selectInput("id_comm", "Select Comm", choices = unique(selectGeneral$id_comm)),
        selectInput("id_study", "Select study", choices = unique(selectGeneral$id_study.x)),
        actionButton("updateMap", "Update Map")
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  )
  
  # Server
  server <- function(input, output, session) {
    
    
    # # Render the map
    # output$map <- renderLeaflet({
    #   leaflet() %>%
    #     addTiles() %>%
    #     addCircleMarkers(
    #       data = filtered_data(),
    #       lat = ~exact_lat,
    #       lng = ~exact_long,
    #       popup = ~id_study.x,
    #       label = ~study_year
    #     )
    # })
    
  }

  plotMap(input,selectGeneral)
  shinyApp(ui, server)
  