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
     
  
  