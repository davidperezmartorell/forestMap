# Install the 'here' package if you haven't already
# install.packages("here")

#roxygen2::roxygenise()
#runApp()


cat("app.R:  Loading libraries\n")
source("loadLibraries.R");loadLibraries() # Load all libraries
source("loadData.R"); #Load all data
#source("fillCountryAccordingLatLon.R"); #Join more info about each country
source("versions.R"); #Foot of page with information about creators, contact and versions
source("renderMap.R"); #Function to plot the maps
source("createTittleForMainTable.R"); #To write contents when we navigate the map
source("commRelated.R"); #Search info about id_comm Communities related to the id_comm choosen in map
source("studyClicked.R"); #Search info about study clicked in map
#source("studiesRelated.R"); #Search info about study related to the id_study choosen in map
source("getRichnessPlot.R"); #Plot graphics abund and richness by age
source("speciesStudyMatrix.R"); #Create table with all species related to clicked study
source("givemeGbifInfo.R"); #Search info in GBIB about each specie
source("givemeAbundance.R"); #Returns abundance from assembleages ad in_comm for each taxon_clean
source("filterDataByIdstudy.R"); #Returns data from taxon and assembleages filtered by id_study to plot values
source("getInventoryPlot.R"); #Returns plot with inventory in that study and abundancy by family
source("getInventoryPlotByClass.R"); #Returns plot with inventory in that study and abundancy by class
source("getInventoryPlotByOrder.R"); #Returns plot with inventory in that study and abundancy by Order
source("mergeAssembleagesTaxon.R"); #Returns taxon and assembleages merged. it's needed to create general plots- Furthermore, it's filtered 5% values in the top
source("getplotInventoryByOrderGeneral.R"); #Returns plot with inventory all the database by Order
source("getplotInventoryByClassGeneral.R"); #Returns plot with inventory all the database by Class
source("getplotInventoryByStageGeneral.R"); #Returns plot with inventory all the database by Stage
source("getplotRichnessByFamilyGeneral.R"); #Returns plot with inventory all the database by Family
source("getplotRichnessByDisturbanceAgeGeneral.R"); #Returns taxon and assembleages merged and filtered 5% values in the top
source("getplotRecoveringConditionGeneral.R"); #Returns Plot with relation recovering condition versus age for all species

#Load data
 cat("app.R:  Loading all data\n")
 data <- loadData()
 
 taxon <- data$taxon
 
 cat("app.R: Loaded ", nrow(taxon), "values from taxon\n")
 assembleages <- data$assembleages
 cat("app.R: Loaded ", nrow(assembleages), "values from assembleages\n")

 cat("app.R: Check error in countries and receive ISO3 code\n")
 #assembleages <- fillCountryAccordingLatLon(assembleages)
 cat("app.R: There are", length(unique(assembleages$country)), "different countries.\n")

 
 #Select interesting values from both dataframes  
 cat("loadData.R: Filterting assembleages \n")
 
 assembleages_filtered <- dplyr::select(assembleages,"id","id_comm", "id_study", "study_year", "stage", "study_common_taxon_clean", "taxon_level", "exact_lat", "exact_long", "country", "disturbance1_age_clean",
                                        "country", "disturbance1_age_clean", "age", "n_comm_available", "metric", "citation", "database")
 assembleages_filtered <- assembleages_filtered %>% unique()
 
 cat("app.R: After filter, there are loaded ", nrow(assembleages_filtered), "values from assembleages\n")
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
        
        titlePanel(HTML("<h3>FORESTMAP Plot maps from particular inventory with studies and inventories</h3>")),
        
        tabsetPanel(
          tabPanel("Map", 
           fluidRow(
             column(12, leafletOutput("map", height = "800px"))  # Adjust the height value for the map
           )
          ),
          tabPanel("Study",
           fluidRow(
             titlePanel(HTML("<h3>FORESTMAP Information from study</h3>")),
             # column(12, div(id = "contentCommRelated", HTML("<h3>Information about ID_COMM clicked</h3>"))),
             div(DT::dataTableOutput("studyClicked"), style = "margin-bottom: 20px; ,margin-right: 20px;"),  # Output for the first table with common values
             titlePanel(HTML("<h3>FORESTMAP Information from communities</h3>")),
             div(DT::dataTableOutput("studiesRelated"), style = "margin-top: 20px;")   # Output for the second table with different values
           )
          ),

          tabPanel("Graphics",
                   mainPanel(
                     fluidRow(
                       column(6, 
                              div(titlePanel(HTML("<h3>FORESTMAP Richness plot</h3>")), style = "margin-bottom: 20px; ,margin-right: 20px;"),
                              div(style = "margin-right: 5%;", plotOutput("plotRichness", height = "400px"))
                       ),
                       column(6, 
                              div(titlePanel(HTML("<h3>FORESTMAP Inventory species by class</h3>")), style = "margin-bottom: 20px; ,margin-right: 20px;"),
                              div(style = "margin-right: 5%;", plotlyOutput("plotInventoryByClass", height = "400px"))
                       ),
                       column(6, 
                              div(titlePanel(HTML("<h3>FORESTMAP Inventory species by order</h3>")), style = "margin-bottom: 20px; ,margin-right: 20px;"),
                              div(style = "margin-left: 5%;", plotlyOutput("plotInventoryByOrder", height = "400px"))
                       ),
                       column(6, 
                              div(titlePanel(HTML("<h3>FORESTMAP Inventory species by family</h3>")), style = "margin-bottom: 20px; ,margin-right: 20px;"),
                              div(style = "margin-left: 5%;", plotlyOutput("plotInventory", height = "400px"))
                       ),
                     )
                   )
          ),

         
          tabPanel("Taxon",
                   fluidRow(
                     column(12, uiOutput("matrixTittle")),  #HERE DRAW TITTLE AND DATA ABOUT THOW THE INFO IS EXTRACTED IN EACH COMMUNITY
                     column(12, dataTableOutput("speciesStudyMatrixOutput", height = "400px"))
                   )
          ),
          

          tabPanel("Species",
           fluidRow(
             div(titlePanel(HTML("<h3>FORESTMAP Info from GBIF</h3>")), style = "margin-bottom: 20px; ,margin-right: 20px;"),
             column(12, dataTableOutput("givemeGbifInfo", height = "400px"))
           )
          ),

          tabPanel("General",
                   mainPanel(
                     fluidRow(
                       tags$style(type='text/css', ".tab-content { height: 100% }"),  # Adjust the height as needed
                       column(12, 
                              div(titlePanel(HTML("<h3>FORESTMAP General Stage plot</h3>")), style = "margin-right: 10px;"),
                              div(plotlyOutput("plotInventoryByStageGeneral"))
                       ),
                       column(12, 
                              div(titlePanel(HTML("<h3>FORESTMAP General Abundance respect recovering contition over the Years</h3>")), style = "margin-right: 10px;"),
                              div(plotlyOutput("plotRecoveringCondition"))
                       ),
                       column(12, 
                              div(titlePanel(HTML("<h3>FORESTMAP General Disturbances plot</h3>")), style = "margin-right: 10px;"),
                              div(plotlyOutput("plotRichnessByDisturbanceAgeGeneral"))
                       ),
                       column(12, 
                              div(titlePanel(HTML("<h3>FORESTMAP General Inventory species by <p>class</p></h3>")), style = "margin-right: 10px;"),
                              div(plotlyOutput("plotInventoryByClassGeneral"))
                       ),
                       column(12, 
                              div(titlePanel(HTML("<h3>FORESTMAP General Inventory species by order</h3>")), style = "margin-right: 10px;"),
                              div(plotlyOutput("plotInventoryByOrderGeneral"))
                       ),
                       column(12, 
                              div(titlePanel(HTML("<h3>FORESTMAP General Inventory species by family</h3>")), style = "margin-right: 10px;"),
                              div(plotlyOutput("plotRichnessByFamilyGeneral"))
                       )
                     )
                   )
          ),          
          
          tabPanel("Versions",
           mainPanel(
             div(titlePanel(HTML("<h3>FORESTMAP Versions of this program</h3>")), style = "margin-bottom: 20px; ,margin-right: 20px;"),
             column(12, versions(), height = "400px")
           )
          )
        )
      )
  
 
# Server
server <- function(input, output,session) {
  
  # Create reactiveValues to store map and clicked marker information
  map_info <- reactiveVal(NULL)
  
  # Observe click events on the map and execute these actions
  observeEvent(input$map_marker_click, {
    #Extract info from map with clicked tear
    
      click <- input$map_marker_click
      infoFromIDStudy <- filter(assembleages_filtered, id_study == click$id)
     

    #Tittle page
      cat("APP.R: Has been clicked id_study ", click$id, "\n")
        tableText <- createTittleForMainTable(infoFromIDStudy)

    #React when is clicked some tear with some study
      #Extract result
        
         result <- studyClicked(assembleages, click$id)
         studyClickedData <- result$assemblagesCommon
         studiesRelatedData <- result$assemblagesUnique
        
      #Extract data for study ploats   
         dataTaxonAsembleagesByStudy <- filterDataByIdstudy(taxon,assembleages,infoFromIDStudy$id_study)

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

        #React when is clicked Graphics with plot
          output$plotRichness <- renderPlot({
          # Call your function to get the richness plot
          cat("getRichnessPlot.R: Creating graphics by Richness\n")
          richnessPlot <- getRichnessPlot(dataTaxonAsembleagesByStudy)
          # Use the ggplot object directly
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
       
       
       #Merge assembleages and taxon to send general graphics
       #This function create the filter and record in file
         # source("loadData.R")
         # cat("app.R:Loading Taxon to create mergedData\n"); data <- loadData();taxon <- data$taxon
         # cat("app.R:Loading Taxon to create mergedData\n");assembleages <- data$assembleages
         # mergedAssembleagesTaxon <- mergeAssembleagesTaxon(taxon ,assembleages)
         # write.csv2(merged_taxon, file = "inst/filtered_data.csv", row.names = FALSE)
       
       #Load combination from taxon and assembleages
       mergedAssembleagesTaxon <- read.csv("inst/filtered_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
       
       #Create general graphics showed in General tag
       output$plotRichnessByFamilyGeneral <- renderPlotly({
         cat("getplotRichnessByFamilyGeneral.R: Creating graphics by General\n")
         plotRichnessByFamilyGeneral <- getplotRichnessByFamilyGeneral(mergedAssembleagesTaxon)
         plotRichnessByFamilyGeneral
       })
       
       output$plotRichnessByDisturbanceAgeGeneral <- renderPlotly({
         cat("getplotRichnessByDisturbanceAgeGeneral: Creating graphics by General\n")
         plotRichnessByDisturbanceAgeGeneral <- getplotRichnessByDisturbanceAgeGeneral(mergedAssembleagesTaxon)
         plotRichnessByDisturbanceAgeGeneral
       })
       
       output$plotInventoryByStageGeneral <- renderPlotly({
         cat("getplotInventoryByStageGeneral.R: Creating graphics by Stage\n")
         plotInventoryByStageGeneral <- getplotInventoryByStageGeneral(mergedAssembleagesTaxon)
         plotInventoryByStageGeneral
       })
       
       output$plotRecoveringCondition <- renderPlotly({
         cat("getplotInventoryByStageGeneral.R: Creating graphics by Stage\n")
         plotRecoveringCondition <- getplotRecoveringConditionGeneral(mergedAssembleagesTaxon)
         plotRecoveringCondition
       })
       
       output$plotInventoryByClassGeneral <- renderPlotly({
         cat("getplotInventoryByClassGeneral.R: Creating graphics by Class\n")
         plotInventoryByClassGeneral <- getplotInventoryByClassGeneral(mergedAssembleagesTaxon)
         plotInventoryByClassGeneral
       })
       
       output$plotInventoryByOrderGeneral <- renderPlotly({
         cat("getplotInventoryByOrderGeneral.R: Creating graphics by General\n")
         plotInventoryByOrderGeneral <- getplotInventoryByOrderGeneral(mergedAssembleagesTaxon)
         plotInventoryByOrderGeneral
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
  
  # Call function to create my map and plot it
    output$map <- renderLeaflet({
        map_info  <- renderMap(assembleages_filtered) 
        map <- map_info$map
        clickedInfo <- map_info$data
          if (!is.null(map)) {
            map
          }
    })

 

}#End of server function

shinyApp(ui, server)
        