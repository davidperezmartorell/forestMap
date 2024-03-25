#' Download all assembleages values
#' @param NULL No values input
#' @return list(taxon = taxon, assembleages = assembleages, index = index) No values output
#' @export
#' @examples
#' loadData()
loadData <- function(){
  
  # Set locale to Spanish (Spain) with UTF-8 encoding
  Sys.setlocale("LC_ALL", "es_ES.UTF-8")
  library("dplyr")
  #First check if data exists in file
  #Load taxons
   cat("loadData.R: Loading taxon_clean with studies\n")
   taxon <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ".")
   
   #Transform measurement from char to num and remove NA values
     taxon <- taxon %>% mutate(measurement = as.numeric(measurement)) %>% na.omit()
   # Set scipen option to a higher value
    options(scipen = 999)
   
   # Round the measurement column to two decimal places
    taxon$measurement <- round(taxon$measurement, digits = 2)
   
    # Remove data from taxon because no plants, they are macroinvertebrates
         id_study_to_remove <- c("RU_Negrete-Yankelevich 2007_Soil macroinvertebrates below litter",
                                 "RU_Negrete-Yankelevich 2007_Soil macroinvertebrates in litter")
   # Remove the rows where id_study matches the values to remove
    taxon <- subset(taxon, !(id_study %in% id_study_to_remove))
   
   # Discard NA values
    taxon$measurement <- na.omit(taxon$measurement)
   
   
   

  #Load assembleages
   cat("loadData.R: Loading taxon_clean with assembleages\n")
   #assembleages <- read.csv("inst/comm_nodist_plants.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
   assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
   
   # Remove the rows where id_study matches the values to remove
   assembleages <- subset(assembleages, !(id_study %in% id_study_to_remove))
   
   
   
   
   # Display the differences   
   cat("loadData.R: Values in taxon and not in assembleages\n")
   differences <- anti_join(taxon,assembleages, by = "id_study") %>% dplyr::select(id_study) %>% unique()
   cat("loadData.R:: there are ", nrow(differences) , " values in taxon that are not in assambleages\n")
   
   cat("loadData.R: Values in assembleages and not in taxon\n")
   differences <- anti_join(assembleages, taxon , by = "id_study") %>% dplyr::select(id_study) %>% unique()
   cat("loadData.R:: there are ", nrow(differences) , " values in differences that are not in taxon\n")
  
   
   #Merge assembleages and taxon to send general graphics
   #This function create the filter and record in file
   # source("loadData.R")
   # cat("app.R:Loading Taxon to create mergedData\n"); data <- loadData();taxon <- data$taxon
   # cat("app.R:Loading Taxon to create mergedData\n");assembleages <- data$assembleages
   # mergedAssembleagesTaxon <- mergeAssembleagesTaxon(taxon ,assembleages)
   # write.csv2(mergedAssembleagesTaxon, file = "inst/filtered_data.csv", row.names = FALSE)
   
   #Load combination from taxon and assembleages
   mergedAssembleagesTaxon <- read.csv("inst/filtered_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
  

   
  
   #Filter data grouping taxon, assembleages, age and measurement to plot graphics by presence|absence from each study
   #It's used in graphics tag
         # #Load data
         # source("loadData.R")
         # data <- loadData()
         # source("loadLibraries.R")
         # library("ggplot2")
         # taxon <- data$taxon
         # taxon_filtered <- dplyr::select(taxon,"id_comm", "id_study", "measurement","class", "order", "family")
         # 
         # assembleages <- data$assembleages
         # assembleages_filtered <- dplyr::select(assembleages,"id","id_comm", "id_study", "study_year", "age", "richness")
         # assembleages_filtered <- assembleages_filtered %>% unique()
         # 
         # # Merge the dataframes based on common columns
         # merged_data <- merge(taxon_filtered, assembleages_filtered, by = c("id_study", "id_comm"), all.x = TRUE)
         # 
         # 
         # mergedAssembleagesTaxonByTime1<- merged_data %>% filter(measurement != 0)
         # # Group by id_study, age, and family, then count the number of rows in each group
         # mergedAssembleagesTaxonByTime2 <- mergedAssembleagesTaxonByTime1 %>% group_by(id_study, age, family) %>% summarise(count = n())
         # # Write the DataFrame to a CSV file
         # write.csv2(mergedAssembleagesTaxonByTime2, "inst/mergedByFamily.csv", row.names = FALSE)
         # 
         # # Group by id_study, age, and family, then count the number of rows in each group
         # mergedAssembleagesTaxonByTime2 <- mergedAssembleagesTaxonByTime1 %>% group_by(id_study, age, class) %>% summarise(count = n())
         # # Write the DataFrame to a CSV file
         # write.csv2(mergedAssembleagesTaxonByTime2, "inst/mergedByClass.csv", row.names = FALSE)
         # 
         # # Group by id_study, age, and family, then count the number of rows in each group
         # mergedAssembleagesTaxonByTime2 <- mergedAssembleagesTaxonByTime1 %>% group_by(id_study, age, order) %>% summarise(count = n())
         # # Write the DataFrame to a CSV file
         # write.csv2(mergedAssembleagesTaxonByTime2, "inst/mergedByOrder.csv", row.names = FALSE)
         # 
         # 
         # 
   #Load data filtered by age,[class|order|family] to print particular graphic for each id_study.can be used to id_comm too.
   mergedByFamily <- read.csv("inst/mergedByFamily.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
   mergedByClass <- read.csv("inst/mergedByClass.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
   mergedByOrder <- read.csv("inst/mergedByOrder.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
       
   
   
   
   
   # Return the list
   # Create a list to hold the data frames
   result_list <- list(taxon = taxon, assembleages = assembleages, mergedAssembleagesTaxon=mergedAssembleagesTaxon,
                       mergedByFamily=mergedByFamily,mergedByClass=mergedByClass,mergedByOrder=mergedByOrder)
   return(result_list)
}



