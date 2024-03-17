  # Install the 'here' package if you haven't already
  # install.packages("here")
  
  #roxygen2::roxygenise()
  #runApp()
  
  
  cat("app.R:  Loading libraries\n")
  source("loadLibraries.R")
  loadLibraries() # Load all libraries
  source("loadData.R"); #Load all data
  source("versions.R"); #Foot of page with information about creators, contact and versions
  source("renderMap.R"); #Function to plot the maps
  source("studyClicked.R"); #Search info about study clicked in map
  source("getRichnessPlot.R"); #Plot graphics abund and richness by age
  source("speciesStudyMatrix.R"); #Create table with all species related to clicked study
  source("givemeGbifInfo.R"); #Search info in GBIB about each specie
  source("filterDataByIdstudy.R"); #Returns the data that pertains to the study clicked
  source("getInventoryPlot.R"); #Returns plot with inventory in that study and abundancy by family
  source("getInventoryPlotByClass.R"); #Returns plot with inventory in that study and abundancy by class
  source("getInventoryPlotByOrder.R"); #Returns plot with inventory in that study and abundancy by Order
  source("mergeAssembleagesTaxon.R"); #Returns taxon and assembleages merged. it's needed to create general plots- Furthermore, it's filtered 5% values in the top
  source("getplotInventoryByOrderGeneral.R"); #Returns plot with inventory all the database by Order
  source("getplotInventoryByClassGeneral.R"); #Returns plot with inventory all the database by Class
  source("getplotInventoryByStageGeneral.R"); #Returns plot with inventory all the database by Stage
  source("getplotRichnessCloudAgeGeneral.R"); #Returns plot with all the database by age nd with boxplots
  source("getplotRichnessCloudAgeGeneralPoints.R"); #Returns plot with all the database by age nd with points and
  source("getplotInventoryByStudyCommonTaxonGeneral.R"); #Returns plot with inventory all the database by study common taxon
  source("getplotRichnessByDisturbanceAgeGeneral.R"); #Returns taxon and assembleages merged
  source("getplotRecoveringConditionGeneral.R"); #Returns Plot with relation recovering condition versus age for all species
 
  
  
  # Remove all objects from the global environment
  rm(list = ls())
  #Garbage memory
  gc()
  
  #Load data
   cat("app.R:  Loading all data\n")
   data <- loadData()
   
   taxon <- data$taxon
   
   assembleages <- data$assembleages
   cat("app.R: Loaded ", nrow(assembleages), "values from assembleages\n")
  
   cat("app.R: Check error in countries and receive ISO3 code\n")
   #assembleages <- fillCountryAccordingLatLon(assembleages)
   cat("app.R: There are", length(unique(assembleages$country)), "different countries.\n")
  
   
   #Select interesting values from both dataframes  
   cat("app.R: Filterting assembleages \n")
    assembleages_filtered <- dplyr::select(assembleages,"id","id_comm", "id_study", "study_year", "stage", "study_common_taxon_clean", "taxon_level", "exact_lat", "exact_long", "country", "disturbance1_age_clean",
                                          "country", "disturbance1_age_clean", "age", "n_comm_available", "metric", "citation", "database")
   assembleages_filtered <- assembleages_filtered %>% unique()
   
   cat("app.R: After filter, there are loaded ", nrow(assembleages_filtered), "values from assembleages\n")
   
   #Load mix of taxon and assembleages by d_comm to create general graphics
   cat("app.R: Creating a register combining taxon and assembleages \n")
   mergedAssembleagesTaxonByTime <- data$mergedAssembleagesTaxon
   
   head(assembleages_filtered)
   
   # Define a reactive value to store the map
    mapToPlot <- reactiveVal()
    map <- reactiveVal()
   
    # Menu --------------------------------------------------------------------
   # UI
        ui <- fluidPage(
          useShinyjs(), 
          
          tags$head(
            tags$style(
              HTML(
                "body, html {height: 100%; margin: 0; background-color: #f2f2f2;} 
        #map {height: 100%;} 
        .title-panel {background-color: #f2f2f2; color: #000000; font-family: 'Tahoma'; font-size: 18px; font-weight: bold; text-align: center; padding: 50px; height: 200px;}"
              )
            ),
            tags$script(src = "https://unpkg.com/leaflet-providers@1.12.0/leaflet-providers.js"),
            tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet@1.7.1/dist/leaflet.css")
          ),
          
          titlePanel(HTML("<h2>FORESTMAP Plot maps from particular inventory with studies and inventories</h2>")),
  
          
          tabsetPanel(
              tabPanel("Map", 
                 fluidRow(
                   column(12, leafletOutput("map", width = "100%", height = "90vh"))  # Adjust the height value for the map
                 )
              ),
            tabPanel("Study",
             fluidRow(
                   titlePanel(HTML("<h3>Information from study</h3>")),
                   # column(12, div(id = "contentCommRelated", HTML("<h3>Information about ID_COMM clicked</h3>"))),
                   div(DT::dataTableOutput("studyClicked"), style = "margin-bottom: 20px; ,margin-right: 20px;"),  # Output for the first table with common values
                   titlePanel(HTML("<h3>Information from communities</h3>")),
                   div(DT::dataTableOutput("studiesRelated"), style = "margin-top: 20px;")   # Output for the second table with different values
                )
             ),
  
            tabPanel("Graphics",
               mainPanel(
                 fluidRow(
                   column(6, 
                          div(style = "margin-right 20px; margin-bottom: 20px;", plotOutput("plotRichness", height = "400px"))
                   ),
                   column(6, 
                          div(style = "margin-bottom: 20px;", plotlyOutput("plotInventoryByClass", height = "400px"))
                   ),
                   column(6, 
                          div(style = "margin-right: 20px; margin-bottom: 20px;", plotlyOutput("plotInventoryByOrder", height = "400px"))
                   ),
                   column(6, 
                          div(style = "margin-bottom: 20px;", plotlyOutput("plotInventory", height = "400px"))
                   ),
                 )
               )
            ),
  
           
            tabPanel("Taxon",
               fluidRow(
                 column(12, uiOutput("matrixTittle")), 
                 column(12, dataTableOutput("speciesStudyMatrixOutput", height = "400px"))
               )
            ),
            
  
            tabPanel("Species",
             fluidRow(
               column(12, dataTableOutput("givemeGbifInfo", height = "400px"))
             )
            ),
  
              tabPanel("General",
                 sliderInput("timeRange", "Time Range:", min = 0, max = 500, value = c(0, 500), step = 5, width = '100%'),
                 mainPanel(
                   fluidRow(
                     column(12, img(src = "plotInventoryByStageGeneral.png", style = "width: auto; height: auto; display: block; margin-left: auto; margin-right: auto;")),
                   ),
                   fluidRow(
                     column(12, plotlyOutput("plotInventoryByStageGeneral"))
                   ),
                   fluidRow(
                     column(12, img(src = "plotRecoveringConditionGeneral.png", style = "width: auto; height: auto; display: block; margin-left: auto; margin-right: auto;")),
                   ),
                   fluidRow(
                     column(12, plotlyOutput("plotRecoveringCondition"))
                   ),
                   fluidRow(
                     column(12, img(src = "plotRichnessByDisturbanceAgeGeneral.png", style = "width: auto; height: auto; display: block; margin-left: auto; margin-right: auto;")),
                   ),
                   fluidRow(
                     column(12, plotlyOutput("plotRichnessByDisturbanceAgeGeneral"))
                   ),
                   fluidRow(
                     column(12, img(src = "plotRichnessByCloudAgeGeneral.png", style = "max-width: auto; height: auto; display: block; margin-left: auto; margin-right: auto;")),
                   ),                     
                   fluidRow(                     
                    column(12, plotlyOutput("plotRichnessByCloudAgeGeneral"))
                   ),
                   fluidRow(
                     column(12, img(src = "plotRichnessByCloudAgeGeneralPoints.png", style = "max-width: auto; height: auto; display: block; margin-left: auto; margin-right: auto;")),
                   ),                     
                   fluidRow(                     
                     column(12, plotlyOutput("plotRichnessByCloudAgeGeneralPoints"))
                   ),
                   fluidRow(
                     column(12, img(src = "plotInventoryByStudyCommonTaxonGeneral.png", style = "max-width: auto; height: auto; display: block; margin-left: auto; margin-right: auto;")),
                   ),                     
                   fluidRow(                     
                     column(12, plotlyOutput("plotInventoryByStudyCommonTaxonGeneral"))
                   ),
                   fluidRow(
                     column(12, img(src = "plotInventoryByClassGeneral.png", style = "max-width: auto; height: auto; display: block; margin-left: auto; margin-right: auto;")),
                   ),                     
                   fluidRow(                     
                     column(12, plotlyOutput("plotInventoryByClassGeneral"))
                   ),
               )),
            tabPanel("Versions",
               mainPanel(
                 div(titlePanel(HTML("<h3>Versions of this program</h3>")), style = "margin-bottom: 20px; margin-right: 20px;"),
                 column(12, versions(), height = "400px")
               )
             )
          )  #End tabsetPanel
        ) #End UI-fluidPage
    
   
  # Server
  server <- function(input, output,session) {
    
    
    # Create reactiveValues to store map and clicked marker information
    map_info <- reactiveVal(NULL)
    mergedAssembleagesTaxon <- reactiveVal(NULL)
    
    

    # Observe click events on the map and execute these actions
    observeEvent(input$map_marker_click, {
      
      #Extract info from map with clicked tear
        click <- input$map_marker_click
        infoFromIDStudy <- filter(assembleages_filtered, id_study == click$id)
       
      #React when is clicked some tear with some study
        #Extract result
          
           result <- studyClicked(assembleages, click$id)
           studyClickedData <- result$assemblagesCommon
           studiesRelatedData <- result$assemblagesUnique
  
      #Create table according study clicked with general and particular contents
           output$studyClicked <- renderDT({
             datatable(studyClickedData, escape = FALSE)
             
           })
         
         output$studiesRelated <- renderDT({
           DT::datatable(studiesRelatedData)
         })
     
     #Request info about each specie according study clicked
        output$givemeGbifInfo <- DT::renderDataTable({
              speciesList<-givemeGbifInfo(taxon ,assembleages,infoFromIDStudy$id_study)
            datatable(speciesList, options = list(dom = 't', pageLength = 100, scrollX = TRUE), caption = tags$caption(tags$h1("SPECIES INFO ")))
          })
  
          
        # Request inventory of taxon
              output$speciesStudyMatrixOutput <- renderDataTable({
                # Render the DataTable
                  dataframeMatrix <- speciesStudyMatrix(assembleages, taxon, click$id)
                matrixDataframe <- dataframeMatrix$data
                matrixTittle <- dataframeMatrix$tittle  # Corrected variable name
                
                # Send the title to the UI
                output$matrixTittle <- renderUI({
                  HTML(matrixTittle)
                })
                
                # Return the DataTable
                datatable(matrixDataframe, options = list(dom = 't', pageLength = 100, scrollX = TRUE))
              })
  
          
  
          #Extract data for study ploats for specific id_study   
          dataTaxonAsembleagesByStudy <- filterDataByIdstudy(taxon,assembleages,infoFromIDStudy$id_study)                 
          
          #React when is clicked Graphics with plot
          output$plotRichness <- renderPlot({
              cat("getRichnessPlot.R: Creating graphics by Richness\n")
              richnessPlot <- getRichnessPlot(dataTaxonAsembleagesByStudy)
              richnessPlot
          })
   
          output$plotInventory <- renderPlotly({
              cat("getRichnessPlot.R: Creating graphics by Richness\n")
              inventoryPlot <- getInventoryPlot(dataTaxonAsembleagesByStudy)
              inventoryPlot
            })
      
         output$plotInventoryByClass <- renderPlotly({
              cat("getInventoryPlotByClass.R: Creating graphics by Class\n")
              inventoryPlotByClass <- getInventoryPlotByClass(dataTaxonAsembleagesByStudy)
              inventoryPlotByClass
           })
  
         output$plotInventoryByOrder <- renderPlotly({
              cat("getInventoryPlotByOrder.R: Creating graphics by Order\n")
              inventoryPlotByOrder <- getInventoryPlotByOrder(dataTaxonAsembleagesByStudy)
              inventoryPlotByOrder
          })
         
         
  
      
        
         output$popupInfo <- renderText({
          if (!is.null(marker_info$info)) {
            # You can customize how you want to display the information
            # For example, you may want to format it as a table or list
            paste("Clicked Marker Info:", toString(marker_info$info))
          } else {
            "Click a marker to view information"
          }
        })
      
        
        output$clickedInfo <- renderPrint({
          # Print the clicked popup information
          print(clickedIconData$data)
        })

    })#End of Observe event function
    
    # Define a reactive expression that updates with input$timeRange
    filterDataByAge <- reactive({
      req(input$timeRange) # Ensure input$timeRange is available
      filtered <- mergedAssembleagesTaxonByTime %>% 
        filter(age >= input$timeRange[1] & age <= input$timeRange[2])
      cat("app.R: Has been selected age range from ",input$timeRange[1] , " to " , input$timeRange[2], "\n")
      return(filtered)
    })
    
    ######################################################
    #    Call function to create my map and plot it      #
    ######################################################
      output$map <- renderLeaflet({
          map_info  <- renderMap(assembleages_filtered) 
          map <- map_info$map
          clickedInfo <- map_info$data
            if (!is.null(map)) {
              map
            }
      })
      
    ######################################################
    #    Plot general graphics                           #
    ######################################################  
      output$plotInventoryByStageGeneral <- renderPlotly({
        cat("getplotInventoryByStageGeneral.R: Creating graphics by Stage\n")
        mergedAssembleagesTaxon<-filterDataByAge()          
        plotInventoryByStageGeneral <- getplotInventoryByStageGeneral(mergedAssembleagesTaxon)
        plotInventoryByStageGeneral
      })
      
      output$plotRecoveringCondition <- renderPlotly({
        cat("getplotInventoryByStageGeneral.R: Creating graphics by Stage\n")
        mergedAssembleagesTaxon<-filterDataByAge()
        plotRecoveringCondition <- getplotRecoveringConditionGeneral(mergedAssembleagesTaxon)
        plotRecoveringCondition
      })
      
      
      output$plotRichnessByDisturbanceAgeGeneral <- renderPlotly({
        cat("getplotRichnessByDisturbanceAgeGeneral: Creating graphics Disturbance by General\n")
        mergedAssembleagesTaxon<-filterDataByAge()
        plotRichnessByDisturbanceAgeGeneral <- getplotRichnessByDisturbanceAgeGeneral(mergedAssembleagesTaxon)
        plotRichnessByDisturbanceAgeGeneral
      })
      
      
      output$plotInventoryByStudyCommonTaxonGeneral <- renderPlotly({
        cat("plotInventoryByStudyCommonTaxonGeneral: Creating graphics by General\n")
        mergedAssembleagesTaxon<-filterDataByAge()
        plotInventoryByStudyCommonTaxonGeneral <- getplotInventoryByStudyCommonTaxonGeneral(mergedAssembleagesTaxon)
        plotInventoryByStudyCommonTaxonGeneral
      })
      
      output$plotInventoryByClassGeneral <- renderPlotly({
        cat("plotInventoryByClassGeneral: Creating graphics by General\n")
        mergedAssembleagesTaxon<-filterDataByAge()
        plotInventoryByClassGeneral <- getplotInventoryByClassGeneral(mergedAssembleagesTaxon)
        plotInventoryByClassGeneral
      })
      
      output$plotInventoryByOrderGeneral <- renderPlotly({
        cat("getplotInventoryByOrderGeneral.R: Creating graphics by General\n")
        mergedAssembleagesTaxon<-filterDataByAge()
        plotInventoryByOrderGeneral <- getplotInventoryByOrderGeneral(mergedAssembleagesTaxon)
        plotInventoryByOrderGeneral
      })
      
      output$plotRichnessByCloudAgeGeneral <- renderPlotly({
        cat("getplotRichnessCloudAgeGeneral: Creating graphics by General\n")
        mergedAssembleagesTaxon<-filterDataByAge()
        plotRichnessByCloudAgeGeneral <- getplotRichnessCloudAgeGeneral(mergedAssembleagesTaxon)
        ggplotly(plotRichnessByCloudAgeGeneral)
      })
      
      output$plotRichnessByCloudAgeGeneralPoints <- renderPlotly({
        cat("getplotRichnessCloudAgeGeneralPoints: Creating graphics by General\n")
        mergedAssembleagesTaxon<-filterDataByAge()
        plotRichnessByCloudAgeGeneralPoints <- getplotRichnessCloudAgeGeneralPoints(mergedAssembleagesTaxon)
        ggplotly(plotRichnessByCloudAgeGeneralPoints)
      })
  
  }#End of server function
  
  shinyApp(ui, server)
          