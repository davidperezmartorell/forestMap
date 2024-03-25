taxon <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
taxon$measurement <- na.omit(taxon$measurement)
taxon$measurement <- round(taxon$measurement, digits = 2)
assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
id_study<-"CU_Liebsch et al. 2008, Liebsch et al. 2007_Guapyassu 1994"
source("loadLibraries.R")
library("dplyr")
library("dplyr")
source("filterDataByIdstudy.R"); #Returns data from taxon and assembleages filtered by id_study to plot values
source("getInventoryPlotByClass.R"); #Returns plot with inventory in that study and abundancy by family

#Para ejecutar plots de ese estudio y ver si funciona
dataTaxonAsembleagesByStudy <- filterDataByIdstudy(taxon,assembleages,id_study)
RichnessPlot<-getInventoryPlotByClass(dataTaxonAsembleagesByStudy)
RichnessPlot


#PAra comparar id_study de ambas fuentes
taxon[grepl("CU_Liebsch et al. 2008, Liebsch et al. 2007_Guapyassu 1994", taxon$id_comm), ] %>% select(id_study, id_comm) %>% unique()
assembleages[grepl("CU_Liebsch et al. 2008, Liebsch et al. 2007_Guapyassu 1994", assembleages$id_study), ] %>% select(id_study, id_comm,site) %>% unique()


#Volver a cargar datos de CU_Liebsch et al. 2008
taxon <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
taxon %>% filter(grepl("Liebsch et al", id_comm)) %>% select(id_comm) %>% unique()
assembleages %>% filter(grepl("Liebsch et al", id_comm)) %>% select(id_comm) %>% unique()



#Revisar los DOIS que existen para todos los assembleages por si falta alguno
  # Load citation data
  citation_data <- read.csv("inst/citation_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
  assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
  # Assuming 'assemblages' is an sf object
  assemblages_ori <- st_drop_geometry(assembleages)
  
  # Merge citation data with assemblages based on the citation column
  assemblages_merged <- merge(assemblages_ori, citation_data, by = "citation", all = TRUE)
  assemblages_merged2 <- assemblages_merged %>% select (id_study,citation,Title, DOI) 
  
  #Valores sin DOI
  assemblages_merged2 %>% filter(DOI == "") %>% select(citation, DOI) %>% unique()
  
  
  assemblages_ori %>% filter (citation=="Eilu and Obua 2005") %>% select(citation) %>% unique()
  citation_data %>% filter (citation=="Eilu and Obua 2005") %>% select(citation,DOI) %>% unique()
  
  
  ####################################################################################
  #                                                                                  #
  #      Aqui importo datos de taxon y de datos detaxonomia y los combino            #
  #                                                                                  #
  ####################################################################################
  source("loadLibraries.R")
  library("dplyr")
  #Load values
  taxon <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
  taxonWithTaxonomy <- read.csv("inst/tax_with_taxonomy.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
  
  #taxon2 <- taxon %>% slice(1:1000)
  # Merge taxon dataframe with taxonWithTaxonomy dataframe
   merged_taxon <- merge(taxon, taxonWithTaxonomy[c("taxon_ori", "kingdom", "phylum", "class", "order", "family", "genus", "IsGymnosperm")], by = "taxon_ori", all.x = TRUE)
  
   merged_taxon$measurement <- as.numeric(merged_taxon$measurement)
   
   # Set scipen option to a higher value
   options(scipen = 999)
   
   # Round the measurement column to two decimal places
   merged_taxon$measurement <- round(merged_taxon$measurement, digits = 2)
  
  #Observe NA variables
   merged_taxon$measurement[is.na(merged_taxon$measurement)]
  
  # Discard NA values
   merged_taxon$measurement <- na.omit(merged_taxon)
  
  # Write the merged dataframe to a CSV file
   write.csv2(merged_taxon, file = "inst/tax_cleaned2.csv", row.names = FALSE)
     
  
   ########################################################################
   #                                                                      #
   #                    PRUEBAS CON LOS GR??FICOS                          #
   #                                                                      #
   ########################################################################
   
   source("filterDataByIdstudy.R")
   source("getRichnessPlot.R") #by stage
   source("getInventoryPlot.R") #by family
   source("getInventoryPlotByClass.R")
   source("getInventoryPlotByOrder.R")
   source("loadLibraries.R");loadLibraries()
   library("dplyr", "ggplot2")
   taxon <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
   assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
   
   #Extract data for study ploats   
     dataTaxonAsembleagesByStudy <- filterDataByIdstudy(taxon,assembleages,"RU_Muniz-Castro 2011_Tree")
     plot<-getInventoryPlot(dataTaxonAsembleagesByStudy)
     plot
     
     #Extract data for study ploats   
     dataTaxonAsembleagesByStudy <- filterDataByIdstudy(taxon,assembleages,"RU_Muniz-Castro 2011_Tree")
     plot<-getInventoryPlotByClass(dataTaxonAsembleagesByStudy)
     plot

     #Extract data for study ploats   
     dataTaxonAsembleagesByStudy <- filterDataByIdstudy(taxon,assembleages,"RU_Muniz-Castro 2011_Tree")
     plot<-getInventoryPlotByOrder(dataTaxonAsembleagesByStudy)
     plot
     
     #Extract data for study ploats   
     dataTaxonAsembleagesByStudy <- filterDataByIdstudy(taxon,assembleages,"RU_Muniz-Castro 2011_Tree")
     plot<-getRichnessPlot(dataTaxonAsembleagesByStudy)
     plot
     
     
     ####################################################################################
     #                                                                                  #
     #      Pruebas con los graficos generales           #
     #                                                                                  #
     ####################################################################################
     source("filterDataByIdstudy.R")
     source("getplotInventoryByStageGeneral.R") #by stage
     source("getplotRecoveringConditionGeneral.R") #by recovering condition
     source("loadLibraries.R");loadLibraries()
     library("dplyr", "ggplot2")
     #Load combination from taxon and assembleages
     mergedAssembleagesTaxon <- read.csv("inst/filtered_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
     rm(getplotInventoryByStageGeneral)
     source("getplotInventoryByStageGeneral.R")
     plotStage<-getplotInventoryByStageGeneral(mergedAssembleagesTaxon)
     plotStage
     ggplot(plotStage)
     ggplotly(plotStage)
     
     
     mergedAssembleagesTaxon <- read.csv("inst/filtered_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
     rm(getplotRecoveringConditionGeneral)
     source("getplotRecoveringConditionGeneral.R") #by recovering condition
     plotStage<-getplotRecoveringConditionGeneral(mergedAssembleagesTaxon)
     plotStage
     ggplotly(plotStage)
     
     
     mergedAssembleagesTaxon <- read.csv("inst/filtered_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
     rm(getplotRichnessByDisturbanceAgeGeneral)
     source("getplotRichnessByDisturbanceAgeGeneral.R") #by recovering condition
     plotStage<-getplotRichnessByDisturbanceAgeGeneral(mergedAssembleagesTaxon)
     #plotStage
     #ggplotly(plotStage)
     
     plotlyPlot <- ggplotly(plotStage) %>%
       layout(boxmode = 'group', boxgap = 0.1) # Adjust box gap as needed
     plotlyPlot
     
     
     
     
     

     ############################################
     #      Record general plots pictures       #
     ############################################
     mergedAssembleagesTaxon <- read.csv("inst/filtered_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
     
     rm(getplotInventoryByStudyCommonTaxonGeneral)
     source("getplotInventoryByStudyCommonTaxonGeneral.R") #by recovering condition
     plotStage<-getplotInventoryByStudyCommonTaxonGeneral(mergedAssembleagesTaxon)
     
     
     #plotStage

     plotStage
     
     ggsave("www/plotRichnessCloudAgeGeneralPoints.png", plot = plotStage, width = 2.67, height = 1.67, units = "in", dpi = 300)
     URL("inst/getplotInventoryByOrderGeneral.png")
     
     ############################################
     #        Measure time execution            #
     ############################################
     
     execution_time <- system.time({
       getplotInventoryByStageGeneral(mergedAssembleagesTaxon)
     })[["elapsed"]]
     print(paste("Execution time:", execution_time, "seconds"))
     
     execution_time <- system.time({
       getplotRecoveringConditionGeneral(mergedAssembleagesTaxon)
     })[["elapsed"]]
     print(paste("Execution time:", execution_time, "seconds"))
     
     execution_time <- system.time({
       getplotRichnessByDisturbanceAgeGeneral(mergedAssembleagesTaxon)
     })[["elapsed"]]
     print(paste("Execution time:", execution_time, "seconds"))
     
     execution_time <- system.time({
       getplotInventoryByStudyCommonTaxonGeneral(mergedAssembleagesTaxon)
     })[["elapsed"]]
     print(paste("Execution time:", execution_time, "seconds"))
     
     execution_time <- system.time({
       getplotInventoryByClassGeneral(mergedAssembleagesTaxon)
     })[["elapsed"]]
     print(paste("Execution time:", execution_time, "seconds"))
     
     execution_time <- system.time({
       getplotInventoryByOrderGeneral(mergedAssembleagesTaxon)
     })[["elapsed"]]
     print(paste("Execution time:", execution_time, "seconds"))
     
     execution_time <- system.time({
       getplotRichnessCloudAgeGeneral(mergedAssembleagesTaxon)
     })[["elapsed"]]
     print(paste("Execution time:", execution_time, "seconds"))
     
     execution_time <- system.time({
       getplotRichnessCloudAgeGeneralPoints(mergedAssembleagesTaxon)
     })[["elapsed"]]
     print(paste("Execution time:", execution_time, "seconds"))
     
     #Function list
     getplotInventoryByStageGeneral
     getplotRecoveringConditionGeneral
     getplotRichnessByDisturbanceAgeGeneral
     getplotInventoryByStudyCommonTaxonGeneral
     getplotInventoryByClassGeneral
     getplotInventoryByOrderGeneral
     getplotRichnessCloudAgeGeneral
     getplotRichnessCloudAgeGeneralPoints
     
####################################################################################################
#   LETS CREATE GROUP OF VALUES BY TAXON, FAMILY AND SUM VALUES NOT ZERO TO SEE EVOLUTION.
####################################################################################################
     
     #Load data
     source("loadData.R")
     data <- loadData()
     source("loadLibraries.R")
     library("ggplot2")
     taxon <- data$taxon
     taxon_filtered <- dplyr::select(taxon,"id_comm", "id_study", "measurement","class", "order", "family")
     
     assembleages <- data$assembleages
     assembleages_filtered <- dplyr::select(assembleages,"id","id_comm", "id_study", "study_year", "age", "richness")
     assembleages_filtered <- assembleages_filtered %>% unique()
     
     # Merge the dataframes based on common columns
     merged_data <- merge(taxon_filtered, assembleages_filtered, by = c("id_study", "id_comm"), all.x = TRUE)
     
     
     mergedAssembleagesTaxonByTime1<- merged_data %>% filter(measurement != 0)
     # Group by id_study, age, and family, then count the number of rows in each group
     mergedAssembleagesTaxonByTime2 <- mergedAssembleagesTaxonByTime1 %>% group_by(id_study, age, family) %>% summarise(count = n())
     # Write the DataFrame to a CSV file
     write.csv2(mergedAssembleagesTaxonByTime2, "inst/mergedByFamily.csv", row.names = FALSE)
     
     # Group by id_study, age, and family, then count the number of rows in each group
     mergedAssembleagesTaxonByTime2 <- mergedAssembleagesTaxonByTime1 %>% group_by(id_study, age, class) %>% summarise(count = n())
     # Write the DataFrame to a CSV file
     write.csv2(mergedAssembleagesTaxonByTime2, "inst/mergedByClass.csv", row.names = FALSE)
     
     # Group by id_study, age, and family, then count the number of rows in each group
     mergedAssembleagesTaxonByTime2 <- mergedAssembleagesTaxonByTime1 %>% group_by(id_study, age, order) %>% summarise(count = n())
     # Write the DataFrame to a CSV file
     write.csv2(mergedAssembleagesTaxonByTime2, "inst/mergedByOrder.csv", row.names = FALSE)
     
     

     
     
     rm(mergedByFamily2,mergedByClass2,mergedByOrder2)
     mergedByFamily2 <- filter(mergedByFamily, id_study == "CU_Aravena et al. 2002_1 1A Saplings random plots or quadrats")
     mergedByClass2 <- filter(mergedByClass, id_study == "CU_Aravena et al. 2002_1 1A Saplings random plots or quadrats")
     mergedByOrder2 <- filter(mergedByOrder, id_study == "CU_Aravena et al. 2002_1 1A Saplings random plots or quadrats")
     mergedByFamily2$age <- as.numeric(mergedByFamily2$age)
     mergedByClass2$age <- as.numeric(mergedByClass2$age)
     mergedByOrder2$age <- as.numeric(mergedByOrder2$age)
          
     rm(getInventoryPlotByFamily);rm(getInventoryPlotByOrder);rm(getInventoryPlotByClass)
     
     
     source("getInventoryPlotByFamilyPresence.R") #by recovering condition
     source("getInventoryPlotByClassPresence.R") #by recovering condition
     source("getInventoryPlotByOrderPresence.R") #by recovering condition
     
     plotStage<-getInventoryPlotByFamilyPresence(mergedByFamily2);plot(plotStage)
     plotStage<-getInventoryPlotByClassPresence(mergedByClass2);plot(plotStage)
     plotStage<-getInventoryPlotByOrderPresence(mergedByOrder2);plot(plotStage)
     plot(plotStage)
     