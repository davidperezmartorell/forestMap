#Load libraries
 library("readr")
 library("dplyr")
 library("leaflet")
 library("shiny")

#Load sources
 taxon_origin <- read_delim("../inst/tax_cleaned.csv", delim = ";", show_col_types = FALSE)
 sources <- read_delim("../inst/comm_nodist_plants.csv", delim = ";", show_col_types = FALSE)
 

#Common columns
 # Assuming taxon and sources are your data frames
 common_columns <- intersect(names(taxon), names(sources))

 # Print the common columns
 print(common_columns)

#Join by id_comm
 # Assuming taxon and sources are your data frames
 merged_data <- merge(taxon, sources, by = "id_comm")

 # Display the first 10 rows of the merged data frame
 head(merged_data, 10)

#Filter by id_comm==Addo-Fordjour 2019_Liana_Sit1_NA RU_Addo-Fordjour
 filtered_data <- merged_data# %>% filter(id_comm == "Addo-Fordjour 2019_Liana_Sit1_NA") #Sheil et al. 2002_CIFOR_Indonesia_herbs_100_NA
  # Display the filtered data
 print(filtered_data)
 
 #PLOT data
  data <- filtered_data[, c("id_comm","id_study.x","taxon_level.x", "rank", "study_year", "taxon_clean", "exact_lat", "exact_long")]
  data <- data %>% sample_n(1000)  
  
  
  
  
  
  
  
  
  
  # UI
  # UI
  ui <- fluidPage(
    titlePanel("Species Map"),
    
    sidebarLayout(
      sidebarPanel(
        # Add UI components for taxon level, species, and study year selection
        selectInput("id_comm", "Select Comm", choices = unique(data$id_comm)),
        selectInput("id_study", "Select study", choices = unique(data$id_study.x)),
        selectInput("id_taxon_level", "Select taxon_level", choices = unique(data$taxon_level.x)),
        selectInput("taxon_level", "Select Taxon Level", choices = unique(data$rank)),
        selectInput("study_year", "Select Study Year", choices = unique(data$study_year)),
        selectInput("species", "Select Species", choices = unique(data$taxon_clean)),
        actionButton("updateMap", "Update Map")
      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  )
  
  # Server
  server <- function(input, output, session) {
    
    # Reactive expression for filtered data
    filtered_data <- reactive({
      data %>%
        filter(
          id_comm == input$id_comm,
          taxon_level.x == input$id_taxon_level,
          rank == input$taxon_level
        )
    })
    
    # Update choices for species based on selected taxon_level
    observe({
      species_choices <- NULL
      
      if (input$taxon_level %in% unique(data$rank)) {
        species_choices <- data %>%
          filter(rank == input$taxon_level, id_comm == input$id_comm) %>%
          select(taxon_clean) %>%
          distinct() %>%
          pull()
      }
      
      updateSelectInput(session, "species", choices = species_choices)
    })
    
    # Update choices for study year based on selected taxon_level and species
    observe({
      study_year_choices <- NULL
      
      if (!is.null(input$taxon_level) && !is.null(input$species)) {
        study_year_choices <- data %>%
          filter(rank == input$taxon_level, taxon_clean == input$species, id_comm == input$id_comm) %>%
          select(study_year) %>%
          distinct() %>%
          pull()
      }
      
      updateSelectInput(session, "study_year", choices = study_year_choices)
    })
    
    # Update choices for id_study based on selected id_comm
    observe({
      id_study_choices <- NULL
      
      if (!is.null(input$id_comm)) {
        id_study_choices <- data %>%
          filter(id_comm == input$id_comm) %>%
          select(id_study.x) %>%
          distinct() %>%
          pull()
      }
      
      updateSelectInput(session, "id_study.x", choices = id_study_choices)
    })
    
    # Additional logic to update map on button click
    observeEvent(input$updateMap, {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data = filtered_data(),
          lat = ~exact_lat,
          lng = ~exact_long,
          popup = ~id_study.x,
          label = ~study_year
        )
    })
    
    # Render the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          data = filtered_data(),
          lat = ~exact_lat,
          lng = ~exact_long,
          popup = ~id_study.x,
          label = ~study_year
        )
    })
    
  }
  
  shinyApp(ui, server)
  