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
  
   

  
   # Return the list
   # Create a list to hold the data frames
   result_list <- list(taxon = taxon, assembleages = assembleages)
   return(result_list)
}



