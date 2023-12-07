# Install the 'here' package if you haven't already
# install.packages("here")

#roxygen2::roxygenise()
#runApp()

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
 

 # UI
  ui <- fluidPage(
    titlePanel("Species Map"),
    
    sidebarLayout(
      sidebarPanel(
        # Add UI components for taxon level, species, and study year selection
        selectInput("country", "Select country", choices = c("All the World",unique(selectGeneral$country)), selected="All the World")
      ),
      mainPanel(
        plotMap(input, selectGeneral)
      )
    )
  )
  
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
  
}


  shinyApp(ui, server)
  