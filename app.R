
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
  source("getInventoryPlotByOrderPresence.R"); #Returns Plot with data grouped by presence|ausence
  source("getInventoryPlotByClassPresence.R"); #Returns Plot with  data grouped by presence|ausence
  source("getInventoryPlotByFamilyPresence.R"); #Returns Plot with  data grouped by presence|ausence
  source("getPieGraphs.R"); #Returns 3 PIE GRAPHS WITH DISTRIBUCION IN taxonomy in that study
  


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
   
   
   #Load data filtering values to plot study graph by presence|absence
    mergedByFamily <- data$mergedByFamily
    mergedByClass <- data$mergedByClass
    mergedByOrder <- data$mergedByOrder

   
   # head(assembleages_filtered)
    
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
                "
                  body, html {height: 100%; margin: 0; background-color: #f2f2f2;} 
                  #map {height: 100%;} 
                  .title-panel {background-color: #f2f2f2; color: #000000; font-family: 'Tahoma'; font-size: 18px; font-weight: bold; text-align: center; padding: 50px; height: 200px;}
                  
                  /* New Styles */
                  .custom-title {
                    background-color: #4CAF50; /* Green background */
                    color: white; /* White text */
                    padding: 14px 20px; /* Some padding */
                    margin: 10px 0px; /* Some margin */
                    border: none; /* No border */
                    cursor: pointer; /* Pointer/hand icon */
                    width: 100%; /* Full width */
                    font-family: Arial, sans-serif; /* Font family */
                    text-align: center; /* Centered text */
                  }
                  .custom-download-button .btn {
                    background-color: #4CAF50; /* Green background */
                    color: white; /* White text */
                    padding: 10px 24px; /* Some padding */
                    border-radius: 5px; /* Rounded corners */
                    font-size: 16px; /* Larger font size */
                    font-weight: bold; /* Bold font weight */
                  }
                  "
              )
            ),
            tags$script(src = "https://unpkg.com/leaflet-providers@1.12.0/leaflet-providers.js"),
            tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet@1.7.1/dist/leaflet.css"),
            
            tags$head(
              tags$script(HTML("
            $(document).on('shiny:connected', function() {
                // Add title attributes when the Shiny session is connected
                //$('#downloadHTML_DISABLED').attr('title', 'Please, navigate all the data before download');
                //$('#downloadPDF_DISABLED').attr('title', 'Please, navigate all the data before download');
                //$('#downloadCSV_DISABLED').attr('title', 'Please, navigate all the data before download');
                $('#downloadHTML_DISABLED').attr('disabled', true).addClass('disabled-button').attr('title', 'Disabled for security reasons');
                $('#downloadPDF_DISABLED').attr('disabled', true).addClass('disabled-button').attr('title', 'Disabled for security reasons');
                $('#downloadCSV_DISABLED').attr('disabled', true).addClass('disabled-button').attr('title', 'Disabled for security reasons');
            });"))),
          ),  #End of tags$head

          
          fluidRow(
            column(width = 8, offset = 1, 
                   HTML("<h3 class='custom-title'>FORESTMAP Forest disturbances inventories</h3>")
            ),
            column(width = 1, # Adjust based on the size of your button or desired alignment
                   downloadButton("downloadHTML_DISABLED", label = "HTML"),
                   align = "center"
            ),
            column(width = 1, # Adjust based on the size of your button or desired alignment
                   downloadButton("downloadPDF_DISABLED", label = "PDF"),
                   align = "center"
            ),
            column(width = 1, # Adjust based on the size of your button or desired alignment
                   downloadButton("downloadCSV_DISABLED", label = "CSV"),
                   align = "center"
            )
          ),
          
          
          tabsetPanel(
              tabPanel("Map", 
                 fluidRow(
                   column(12, leafletOutput("map", width = "100%", height = "90vh"))  # Adjust the height value for the map
                 )
              ),
            tabPanel("Study",
             fluidRow(
                   titlePanel(HTML("<h3>Information from study</h3>")),
                   div(DT::dataTableOutput("studyClicked"), style = "margin-bottom: 20px; ,margin-right: 20px;"),  # Output for the first table with common values
                   titlePanel(HTML("<h3>Information from communities</h3>")),
                   div(DT::dataTableOutput("studiesRelated"), style = "margin-top: 20px;")   # Output for the second table with different values
                )
             ),
  
            tabPanel("Graphics",
               mainPanel(
                 fluidRow(
                   column(12, 
                          div(plotlyOutput("plotRichness", width = "900px", height = "556px"))
                   )),
                 # fluidRow(
                 #   column(12, 
                 #          div(style = "margin-top: 20px;", plotlyOutput("plotInventoryByClass", width = "900px", height = "556px"))
                 #   )),
                 fluidRow(
                   column(12, 
                          div(style = "margin-top: 20px;", plotlyOutput("inventoryPlotByClassPresence", width = "900px", height = "556px"))
                   )),
                 fluidRow(
                   column(12, 
                          div(style = "margin-top: 20px;", plotlyOutput("inventoryPlotByOrderPresence", width = "900px", height = "556px"))
                   )),
                 fluidRow(
                   column(12, 
                          div(style = "margin-top: 20px;", plotlyOutput("inventoryPlotByFamilyPresence", width = "900px", height = "556px"))
                   ))
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
                 column(4, div(plotOutput("pieSpecieClass", height = "400px"))),
                 column(4, div(plotOutput("pieSpecieOrder", height = "400px"))),
                 column(4, div(plotOutput("pieSpecieFamily", height = "400px")))
               ),
               
               fluidRow(
                 column(12, dataTableOutput("givemeGbifInfo", height = "400px"))
               )
              ),
  
            tabPanel("General",
               sliderInput("timeRange", "Time Range:", min = 0, max = 500, value = c(0, 500), step = 5, width = '100%'),
               mainPanel(
                 fluidRow(
                   column(12, plotlyOutput("plotInventoryByStageGeneral", width = "1200px", height = "450px"))
                 ),
                 tags$div(style = "height: 30px;"), # Add margin between plots
                 fluidRow(
                   column(12, plotlyOutput("plotRichnessByDisturbanceAgeGeneral", width = "1200px", height = "450px"))
                 ),
                 tags$div(style = "height: 30px;"), # Add margin between plots
                 fluidRow(                     
                   column(12, plotlyOutput("plotInventoryByStudyCommonTaxonGeneral", width = "1200px", height = "450px"))
                 )
             )),
            
            tabPanel("Versions",
               mainPanel(
                 div(titlePanel(HTML("<h3>Versions of this program</h3>")), style = "margin-bottom: 20px; margin-right: 20px;"),
                 column(12, versions(), height = "400px")
               )
             )
          )  #End tabsetPanel
          
        ) #End UI-fluidPage
    cat(" Memoria  Memoria usada tras cargar datos y menus ")
    
    cat (" \n")
   
  # Server

  server <- function(input, output,session) {
    
    # Initialize reactive values
    studyClickedDataReact <- reactiveVal()
    studiesRelatedDataReact <- reactiveVal()
    richnessPlotReact <- reactiveVal()
    inventoryPlotByClassReact<- reactiveVal()
    inventoryPlotByOrderPresenceReact <- reactiveVal()
    inventoryPlotByClassPresenceReact <- reactiveVal()
    inventoryPlotByFamilyPresenceReact <- reactiveVal()
    matrixDataframeReact <- reactiveVal()
    speciesListReact <- reactiveVal()
    plotInventoryByStageGeneralReact <- reactiveVal()
    plotRichnessByDisturbanceAgeGeneralReact <- reactiveVal()
    plotInventoryByStudyCommonTaxonGeneralReact <- reactiveVal()
    pieSpecieClassReact <- reactiveVal()
    pieSpecieOrderReact <- reactiveVal()
    pieSpecieFamilyReact <- reactiveVal()
    
    cat(" Memoria  Memoria usada tras de crear variables globales "); print(mem_used()); cat("\n"); cat (" \n")
  
    
    # Observe click events on the map and execute these actions
    observeEvent(input$map_marker_click, {
      
      #Extract info from map with clicked tear
        click <- input$map_marker_click
        infoFromIDStudy <- filter(assembleages_filtered, id_study == click$id)
       
      #React when is clicked some item with some study
        #Extract result
          
           result <- studyClicked(assembleages, click$id)
           studyClickedData <- result$assemblagesCommon
           studiesRelatedData <- result$assemblagesUnique

      #Filter values to plot study graph by presence|absence
           mergedByFamily <- filter(mergedByFamily, id_study == click$id)
           mergedByClass <- filter(mergedByClass, id_study == click$id)
           mergedByOrder <- filter(mergedByOrder, id_study == click$id)
           
     
           
###########################################################################
#    Call function to create info about study clicked and info related    #
###########################################################################
      #Create table according study clicked with general and particular contents
        output$studyClicked <- renderDT({
           datatable(studyClickedData, escape = FALSE)
         })
         
         output$studiesRelated <- renderDT({
           DT::datatable(studiesRelatedData)
         })
         
         
###########################################################################
#    Call function to add taxonomy info in taxon.csv file                 #
###########################################################################          
 #Request info about each specie according study clicked
    output$givemeGbifInfo <- DT::renderDataTable({
      
      speciesList <- taxon %>% 
        filter(id_study == infoFromIDStudy$id_study) %>% 
        dplyr::select(taxon_clean, kingdom, phylum, class, order, family, genus, IsGymnosperm) %>%
        rename(Taxon = taxon_clean, Kingdom = kingdom, Phylum = phylum, Class = class,
               Order = order, Family = family, Genus = genus, "Is Gymnosperm" = IsGymnosperm)
      speciesList <- speciesList %>% unique()
      

          ###########################################
          #         PLOT PIE TABLE WITH TAXONOMY    #
          ###########################################
          
          pieGraphs <- getPieGraphs(speciesList)
          
          pieSpecieClass<-pieGraphs$pieSpecieClass
          pieSpecieFamily<-pieGraphs$pieSpecieFamily
          pieSpecieOrder<-pieGraphs$pieSpecieOrder

          
          #React when is clicked Graphics with plot
          output$pieSpecieClass <- renderPlot({
            cat("app.R: Creating pie graphics by class\n")
            pieSpecieClassReact(pieSpecieClass)
            pieSpecieClass
          })
          
          output$pieSpecieOrder <- renderPlot({
            cat("app.R: Creating pie graphics by class\n")
            pieSpecieOrderReact(pieSpecieOrder)
            pieSpecieOrder
          })
          
          output$pieSpecieFamily <- renderPlot({
            cat("app.R: Creating pie graphics by family\n")
            pieSpecieFamilyReact(pieSpecieFamily)
            pieSpecieFamily
          })
          

          #This function only filters but can trigger info from GBIF. givemeGbifInfoLONG.R have more contents about how to get info from GBIF
          #speciesList<-givemeGbifInfo(taxon ,assembleages,infoFromIDStudy$id_study)
          #speciesListReact
          
          speciesListReact(speciesList)
          #Returns table values
          datatable(speciesList, options = list(dom = 't', pageLength = 100, scrollX = TRUE), caption = tags$caption(tags$h1("SPECIES INFO ")))
      })
  
###########################################################################
#    Call function to create inventory matrix                             #
###########################################################################          
         
        # Request inventory of taxon
                output$speciesStudyMatrixOutput <- renderDataTable({
                
                # Render the DataTable
                 dataframeMatrix <- speciesStudyMatrix(assembleages, taxon, click$id)
                 matrixDataframe <- dataframeMatrix$data
                 matrixTittle <- dataframeMatrix$tittle  # Corrected variable name
                 matrixDataframeReact(matrixDataframe)

                # Send the title to the UI
                output$matrixTittle <- renderUI({
                  HTML(matrixTittle)
                })
                
                cat(" Memoria  Memoria usada tras crear matriz de datos speciesStudyMatrixOutput "); print(mem_used()); cat("\n"); cat (" \n")
                # Return the DataTable
                #datatable(matrixDataframe, options = list(dom = 't', pageLength = 100, scrollX = TRUE))
                datatable(matrixDataframe, escape = FALSE, options = list(dom = 't', pageLength = 100, scrollX = TRUE))
                
              })
  
          
          #Extract data for study ploats for specific id_study   
              dataTaxonAsembleagesByStudy <- filterDataByIdstudy(taxon,assembleages,infoFromIDStudy$id_study)                 
          
          #React when is clicked Graphics with plot
          output$plotRichness <- renderPlotly({
              cat("getRichnessPlot.R: Creating graphics by Richness\n")
              richnessPlot <- getRichnessPlot(dataTaxonAsembleagesByStudy)
              richnessPlotReact(richnessPlot)
              richnessPlot
          })
          
          
          # output$plotInventoryByClass <- renderPlotly({
          #   cat("getInventoryPlotByClass.R: Creating graphics by Class\n")
          #   inventoryPlotByClass <- getInventoryPlotByClass(dataTaxonAsembleagesByStudy)
          #   inventoryPlotByClassReact(inventoryPlotByClass)
          #   inventoryPlotByClass
          # })
          
          output$inventoryPlotByClassPresence <- renderPlotly({
            
            cat("getInventoryPlotByClassPresence.R: Creating graphics by Class presence\n")
            inventoryPlotByClassPresence <- getInventoryPlotByClassPresence(mergedByClass)
            inventoryPlotByClassPresenceReact(inventoryPlotByClassPresence)
            cat(" Memoria  Memoria usada tras crear gráficos de plot inventoryPlotByClassPresence "); print(mem_used()); cat("\n"); cat (" \n")
            
            inventoryPlotByClassPresence
          })          
          
          output$inventoryPlotByOrderPresence <- renderPlotly({
            cat("getInventoryPlotByOrderPresence.R: Creating graphics by Order presence\n")
            inventoryPlotByOrderPresence <- getInventoryPlotByOrderPresence(mergedByOrder)
            inventoryPlotByOrderPresenceReact(inventoryPlotByOrderPresence)
            cat(" Memoria  Memoria usada tras crear gráficos de plot inventoryPlotByOrderPresence "); print(mem_used()); cat("\n"); cat (" \n")
            inventoryPlotByOrderPresence
          }) 
      
          
          output$inventoryPlotByFamilyPresence <- renderPlotly({
            
            cat("getInventoryPlotByFamilyPresence.R: Creating graphics by Family presence\n")
            inventoryPlotByFamilyPresence <- getInventoryPlotByFamilyPresence(mergedByFamily)
            inventoryPlotByFamilyPresenceReact(inventoryPlotByFamilyPresence)
            cat(" Memoria  Memoria usada tras crear gráficos de plot inventoryPlotByFamilyPresence "); print(mem_used()); cat("\n"); cat (" \n")
            inventoryPlotByFamilyPresence
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


        


###########################################################################
#    Update the reactive values to create html/pdf to export   #
###########################################################################
        # Update the reactive values
         studyClickedDataReact(result$assemblagesCommon)  # Set value using ()
         studiesRelatedDataReact(result$assemblagesUnique)  # Set value using ()
         cat(" Memoria  Memoria usada tras analizar item clickado\n"); print(mem_used()); cat("\n"); cat (" \n")
         

        
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
          cat(" Memoria  Memoria usada tras crear plot principal "); print(mem_used()); cat("\n"); cat (" \n")
            if (!is.null(map)) {
              map
            }
      })
      

        
    ######################################################
    #    Plot general graphics                           #
    ######################################################  
      output$plotInventoryByStageGeneral <- renderPlotly({
        cat(" Memoria  Memoria usada antes de crear gráficos general "); print(mem_used()); cat("\n"); cat (" \n")
        cat("getplotInventoryByStageGeneral.R: Creating graphics by Stage\n")
        mergedAssembleagesTaxon<-filterDataByAge()          
        plotInventoryByStageGeneral <- getplotInventoryByStageGeneral(mergedAssembleagesTaxon)
        rm(mergedAssembleagesTaxon)
        plotInventoryByStageGeneralReact(plotInventoryByStageGeneral)

        # Convert ggplot to plotly
        plotLYInventoryByStageGeneral <- ggplotly(plotInventoryByStageGeneral)
        rm(plotInventoryByStageGeneral)
        # Modify layout to add legend at the bottom
        plotLYInventoryByStageGeneral <- plotLYInventoryByStageGeneral %>%
          layout(legend = list(orientation = "h", x = 0, y = -0.2))  # Add legend at the bottom
        
        plotLYInventoryByStageGeneral<- ggplotly(plotLYInventoryByStageGeneral) %>% layout(boxmode = 'group', boxgap = 0.1)
        cat(" Memoria  Memoria usada tras de crear gráfico plotLYInventoryByStageGeneral "); print(mem_used()); cat("\n"); cat (" \n")
        plotLYInventoryByStageGeneral
      })

      output$plotRichnessByDisturbanceAgeGeneral <- renderPlotly({
        
        cat("getplotRichnessByDisturbanceAgeGeneral: Creating graphics Disturbance by General\n")
        mergedAssembleagesTaxon<-filterDataByAge()
        plotRichnessByDisturbanceAgeGeneral <- getplotRichnessByDisturbanceAgeGeneral(mergedAssembleagesTaxon)
        rm(mergedAssembleagesTaxon)
        plotRichnessByDisturbanceAgeGeneralReact(plotRichnessByDisturbanceAgeGeneral)
        
        
        # Convert ggplot to plotly
        plotLYRichnessByDisturbanceAgeGeneral <- ggplotly(plotRichnessByDisturbanceAgeGeneral)
        rm(plotRichnessByDisturbanceAgeGeneral)
        # Modify layout to add legend at the bottom
        plotLYRichnessByDisturbanceAgeGeneral <- plotLYRichnessByDisturbanceAgeGeneral %>%
          layout(legend = list(orientation = "h", x = 0, y = -0.2))  # Add legend at the bottom
        
        plotLYRichnessByDisturbanceAgeGeneral<- ggplotly(plotLYRichnessByDisturbanceAgeGeneral) %>% layout(boxmode = 'group', boxgap = 0.1)
        cat(" Memoria  Memoria usada tras de crear gráfico plotLYRichnessByDisturbanceAgeGeneral "); print(mem_used()); cat("\n"); cat (" \n")
        plotLYRichnessByDisturbanceAgeGeneral
        
      })
      
      
      output$plotInventoryByStudyCommonTaxonGeneral <- renderPlotly({
        
        cat("plotInventoryByStudyCommonTaxonGeneral: Creating graphics by General\n")
        mergedAssembleagesTaxon<-filterDataByAge()
        plotInventoryByStudyCommonTaxonGeneral <- getplotInventoryByStudyCommonTaxonGeneral(mergedAssembleagesTaxon)
        rm(mergedAssembleagesTaxon)
        plotInventoryByStudyCommonTaxonGeneralReact(plotInventoryByStudyCommonTaxonGeneral)
        
        # Convert ggplot to plotly
        plotLYInventoryByStudyCommonTaxonGeneral <- ggplotly(plotInventoryByStudyCommonTaxonGeneral)
        rm(plotInventoryByStudyCommonTaxonGeneral)
        # Modify layout to add legend at the bottom
        plotLYInventoryByStudyCommonTaxonGeneral <- plotLYInventoryByStudyCommonTaxonGeneral %>%
          layout(legend = list(orientation = "h", x = 0, y = -0.2))  # Add legend at the bottom
        
        
        plotLYInventoryByStudyCommonTaxonGeneral<- ggplotly(plotLYInventoryByStudyCommonTaxonGeneral) %>% layout(boxmode = 'group', boxgap = 0.1)
        cat(" Memoria  Memoria usada tras de crear gráfico plotLYInventoryByStudyCommonTaxonGeneral "); print(mem_used()); cat("\n"); cat (" \n")
        plotLYInventoryByStudyCommonTaxonGeneral
      })
      
      


######################################################
#    Create HTML                                     #
######################################################   

      # Direct assignment of downloadHandler to output$downloadHTML
      output$downloadHTML <- downloadHandler(
        #DISABLED UNTIL AUTHORS PERMISSIONS
        # filename = function() {
        #   paste0("report-", Sys.Date(), ".html")
        # },
        # content = function(file) {
        # 
        #   #Request react information
        #     # Access the current value of reactive variables
        #     studyClickedData <- studyClickedDataReact()  
        #     studiesRelatedData <- studiesRelatedDataReact()  
        #     richnessPlotData <- richnessPlotReact()
        #     inventoryPlotByOrderPresenceData <- inventoryPlotByOrderPresenceReact()
        #     inventoryPlotByClassPresenceData <- inventoryPlotByClassPresenceReact()
        #     inventoryPlotByFamilyPresenceData <- inventoryPlotByFamilyPresenceReact()
        #     matrixDataframeData <- matrixDataframeReact()
        #     speciesListData <- speciesListReact()
        #     pieSpecieClass <- pieSpecieClassReact()
        #     pieSpecieOrder <- pieSpecieOrderReact()   
        #     pieSpecieFamily <- pieSpecieFamilyReact()
        # 
        #   #Convert images
        #       # Convert to png for the richness plot
        #       plotFilePathRichness <- tempfile(fileext = ".png")
        #       ggsave(plotFilePathRichness, plot = richnessPlotData, width = 6, height = 4, dpi = 300)
        # 
        #       # Save to png for the richness plot
        #       plotFilePathClassPresence <- tempfile(fileext = ".png")
        #       ggsave(plotFilePathClassPresence, plot = inventoryPlotByClassPresenceData, width = 6, height = 4, dpi = 300)
        #       # Save the inventory plot by class to a file
        #       plotFilePathOrderPresence <- tempfile(fileext = ".png")
        #       ggsave(plotFilePathOrderPresence, plot = inventoryPlotByOrderPresenceData, width = 6, height = 4, dpi = 300)
        #       
        #       # Save the inventory plot by class to a file
        #       plotFilePathFamilyPresence <- tempfile(fileext = ".png")
        #       ggsave(plotFilePathFamilyPresence, plot = inventoryPlotByFamilyPresenceData, width = 6, height = 4, dpi = 300)
        #       
        # 
        #       #Create pie plots
        #       plotpieSpecieClass <- tempfile(fileext = ".png")
        #       ggsave(plotpieSpecieClass, plot = pieSpecieClass, width = 6, height = 4, dpi = 300)
        #       plotpieSpecieOrder <- tempfile(fileext = ".png")
        #       ggsave(plotpieSpecieOrder, plot = pieSpecieOrder, width = 6, height = 4, dpi = 300)
        #       plotpieSpecieFamily <- tempfile(fileext = ".png")
        #       ggsave(plotpieSpecieFamily, plot = pieSpecieFamily, width = 6, height = 4, dpi = 300)              
        # 
        #    #Generate images as html
        #     # Generate HTML content for each data table
        #     studyClickedDataTableHtml <- htmlTable(studyClickedData)
        #     studiesRelatedDataTableHtml <- htmlTable(studiesRelatedData)
        #     
        #     matrixDataframeHtml<-htmlTable(matrixDataframeData)
        # 
        #     speciesListHtml<-htmlTable(speciesListData)
        # 
        # 
        #     
        #                 
        #   # Combine HTML parts into one HTML document
        #   htmlContent <- paste(
        #     "<html><body><h1>Report Title</h1>",
        #     "<p>Here is the dynamic content from our Shiny app.</p>",
        #     "<h2>Study Clicked Data</h2>",
        #     studyClickedDataTableHtml,
        #     "<h2>Studies Related Data</h2>",
        #     studiesRelatedDataTableHtml,
        #     "<h2>Species matrix</h2>",
        #     matrixDataframeHtml,
        #   
        #     sprintf('<img src="%s" alt="Specie list by Class">', plotpieSpecieClass),
        # 
        #     sprintf('<img src="%s" alt="Specie list by Order">', plotpieSpecieOrder),
        # 
        #     sprintf('<img src="%s" alt="Specie list by Family">', plotpieSpecieFamily),
        #     "<h2>Taxon table</h2>",          
        #     speciesListHtml,
        # 
        #     sprintf('<img src="%s" alt="Richness Plot">', plotFilePathRichness),
        #     
        #     sprintf('<img src="%s" alt="Class presence Plot">', plotFilePathClassPresence),
        #     
        #     sprintf('<img src="%s" alt="Order presence Plot">', plotFilePathOrderPresence),
        #     
        #     sprintf('<img src="%s" alt="Family presence Plot">', plotFilePathFamilyPresence),
        #     "</body></html>",
        #     sep = "\n"
        #   )
        #   
        #   # Write the HTML content to the file specified by downloadHandler
        #   writeLines(htmlContent, file)
        # }
      )

      ######################################################
      #    Create PDF                                      #
      ######################################################       
      output$downloadPDF <- downloadHandler(
       #DISABLED UNTIL AUTHORS PERMISSIONS
       #  filename = function() {
       #    paste0("report-", Sys.Date(), ".pdf")
       #  },
       #  content = function(file) {
       #                  
       #                  
       #                  #HERE THE SAME CODE COPIED FROM HTML CODE EXPORT
       #                  #Request react information
       #                  # Access the current value of reactive variables
       #                  studyClickedData <- studyClickedDataReact()  
       #                  studiesRelatedData <- studiesRelatedDataReact()  
       #                  richnessPlotData <- richnessPlotReact()
       #                  inventoryPlotByOrderPresenceData <- inventoryPlotByOrderPresenceReact()
       #                  inventoryPlotByClassPresenceData <- inventoryPlotByClassPresenceReact()
       #                  inventoryPlotByFamilyPresenceData <- inventoryPlotByFamilyPresenceReact()
       #                  matrixDataframeData <- matrixDataframeReact()
       #                  speciesListData <- speciesListReact()
       #                  pieSpecieClass <- pieSpecieClassReact()
       #                  pieSpecieOrder <- pieSpecieOrderReact()   
       #                  pieSpecieFamily <- pieSpecieFamilyReact()
       #                  
       #                  #Convert images
       #                  # Convert to png for the richness plot
       #                  plotFilePathRichness <- tempfile(fileext = ".png")
       #                  ggsave(plotFilePathRichness, plot = richnessPlotData, width = 6, height = 4, dpi = 300)
       # 
       #                  
       #                  # Save to png for the richness plot
       #                  plotFilePathClassPresence <- tempfile(fileext = ".png")
       #                  ggsave(plotFilePathClassPresence, plot = inventoryPlotByClassPresenceData, width = 6, height = 4, dpi = 300)
       #                  # Save the inventory plot by class to a file
       #                  plotFilePathOrderPresence <- tempfile(fileext = ".png")
       #                  ggsave(plotFilePathOrderPresence, plot = inventoryPlotByOrderPresenceData, width = 6, height = 4, dpi = 300)
       #                  
       #                  # Save the inventory plot by class to a file
       #                  plotFilePathFamilyPresence <- tempfile(fileext = ".png")
       #                  ggsave(plotFilePathFamilyPresence, plot = inventoryPlotByFamilyPresenceData, width = 6, height = 4, dpi = 300)
       #                  
       # 
       #                  #Generate images as html
       #                  # Generate HTML content for each data table
       #                  studyClickedDataTableHtml <- htmlTable(studyClickedData)
       #                  studiesRelatedDataTableHtml <- htmlTable(studiesRelatedData)
       #                  
       #                  matrixDataframeHtml<-htmlTable(matrixDataframeData)
       #                  
       #                  speciesListHtml<-htmlTable(speciesListData)
       #                  
       #                  
       #                  # Render the ggplot object
       #                  plotpieSpecieClass <- ggplot2::ggplot_build(pieSpecieClass)$plot
       #                  plotpieSpecieOrder <- ggplot2::ggplot_build(pieSpecieOrder)$plot
       #                  plotpieSpecieFamily <- ggplot2::ggplot_build(pieSpecieFamily)$plot
       #                  
       #                  # Save the rendered plot as a PNG file
       #                  plotpieSpecieClass_path <- tempfile(fileext = ".png")
       #                  ggsave(plotpieSpecieClass_path, plot = plotpieSpecieClass, width = 6, height = 4, dpi = 300)
       # 
       #                  plotpieSpecieOrder_path <- tempfile(fileext = ".png")
       #                  ggsave(plotpieSpecieOrder_path, plot = plotpieSpecieOrder, width = 6, height = 4, dpi = 300)
       # 
       #                  plotpieSpecieFamily_path <- tempfile(fileext = ".png")
       #                  ggsave(plotpieSpecieFamily_path, plot = plotpieSpecieFamily, width = 6, height = 4, dpi = 300)
       #                  
       #  # Define the path for the temporary .Rmd file
       #  tempRmdPath <- tempfile(fileext = ".Rmd")
       # 
       #  # Create and write content to the .Rmd file
       #  rmdContent <- c(
       #    "---",
       #    "title: 'FORESTMAP Plot maps from particular inventory with studies and inventories'",
       #    "output: pdf_document",
       #    "---",
       #    "",
       #    "This report is generated based on user input.",
       #    
       #    "## Study Clicked Data Table",
       #    "```{r study-clicked-data-table, echo=FALSE}",
       #    "knitr::kable(studyClickedData)",
       #    "```",
       #    "## Studies Related Data Table",
       #    "```{r studies-related-data-table, echo=FALSE}",
       #      "knitr::kable(studiesRelatedData)",
       #    "```",
       #    
       # 
       #    "```{r inventory-plot-pie-by-class, echo=FALSE}",
       #    "knitr::include_graphics(plotpieSpecieClass_path)",
       # 
       #    "```{r inventory-plot-pie-by-family, echo=FALSE}",
       #    "knitr::include_graphics(plotpieSpecieFamily_path)",
       # 
       #    "```{r inventory-plot-pie-by-order, echo=FALSE}",
       #    "knitr::include_graphics(plotpieSpecieOrder_path)",
       #    
       # 
       #    "## Matrix Dataframe",
       #    "```{r matrix-dataframe, echo=FALSE}",
       #    "knitr::kable(matrixDataframeData)",
       #    "```",
       #    "## Species List Data",
       #    "```{r species-list-data, echo=FALSE}",
       #    "knitr::kable(speciesListData)",
       #    "```",
       # 
       #    "```{r inventory-plot-by-order, echo=FALSE}",
       #    "inventoryPlotByOrderPresenceData",
       #    "```",
       # 
       #    "```{r inventory-plot-by-class, echo=FALSE}",
       #    "inventoryPlotByClassPresenceData",
       #    "```",
       # 
       #    "```{r inventory-plot-by-family, echo=FALSE}",
       #    "inventoryPlotByFamilyPresenceData",
       #    "```",
       # 
       #    "```{r richness-plot, echo=FALSE}",
       #    "knitr::include_graphics(plotFilePathRichness)",
       #    "```",
       # 
       #    "```{r class-presence-plot, echo=FALSE}",
       #    "knitr::include_graphics(plotFilePathClassPresence)",
       #    "```"
       #  )#End of contents to add in pdf
       #  # R Markdown content for plots
       #  
       #  # Write the Rmd content to the temp Rmd file
       #  writeLines(rmdContent, con = tempRmdPath)
       #  
       #  
       #  # Prepare parameters to pass to the R Markdown document
       #  params <- list(
       #    studyClickedDataTableHtml = studyClickedDataTableHtml,
       #    studiesRelatedDataTableHtml = studiesRelatedDataTableHtml,
       #    matrixDataframeData = matrixDataframeData,
       #    speciesListData = speciesListData,
       #      inventoryPlotByOrderPresenceData = inventoryPlotByOrderPresenceData,
       #    inventoryPlotByClassPresenceData = inventoryPlotByClassPresenceData,
       #    inventoryPlotByFamilyPresenceData = inventoryPlotByFamilyPresenceData,
       #    plotFilePathRichness = plotFilePathRichness,
       #    plotFilePathClassPresence = plotFilePathClassPresence
       #  )
       #  
       #  
       #  # Render the R Markdown document to PDF
       #  # Correctly use tempRmdPath in the render call
       #    rmarkdown::render(tempRmdPath, output_file = file, params = params, envir = new.env())
       # }
      )   

      
      ######################################################
      #    Create CSV                                      #
      ######################################################     
      # Direct assignment of downloadHandler to output$downloadCSV
      output$downloadCSV <- downloadHandler(
        #DISABLED UNTIL AUTHORS PERMISSIONS
        # filename = function() {
        #   paste0("report-", Sys.Date(), ".zip")
        # },
        # content = function(file) {
        #   # Request react information
        #   # Access the current value of reactive variables
        #   studyClickedData <- studyClickedDataReact()  
        #   studiesRelatedData <- studiesRelatedDataReact()  
        #   matrixDataframeData <- matrixDataframeReact()
        #   speciesListData <- speciesListReact()
        #   
        #   # Write each dataset to a separate CSV file
        #   write.csv(studyClickedData, file = "studyClickedData.csv", row.names = FALSE)
        #   write.csv(studiesRelatedData, file = "studiesRelatedData.csv", row.names = FALSE)
        #   write.csv(matrixDataframeData, file = "matrixDataframeData.csv", row.names = FALSE)
        #   write.csv(speciesListData, file = "speciesListData.csv", row.names = FALSE)
        #   
        #   # Create a zip file containing all CSV files
        #   files_to_zip <- c("studyClickedData.csv", "studiesRelatedData.csv", "matrixDataframeData.csv", "speciesListData.csv")
        #   zip(file, files_to_zip)
        # }
      )
      
  }#End of server function  
  
  
  
  shinyApp(ui, server)
