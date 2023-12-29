#' Download all assembleages values
#' @param NULL No values input
#' @return list(taxon = taxon, assembleages = assembleages, index = index) No values output
#' @export
#' @examples
#' loadData()
loadData <- function(){
  #First check if data exists in file
    #Load taxons
     cat("loadData.R: Loading taxon_clean with studies\n")
     taxon <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
  
    #Load assembleages
     cat("loadData.R: Loading taxon_clean with assembleages\n")
     assembleages <- read.csv("inst/comm_nodist_plants.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
     
   # Display the differences   
   cat("loadData.R: Values in taxon and not in assembleages\n")
   differences <- anti_join(taxon,assembleages, by = "id_study") %>% dplyr::select(id_study) %>% unique()
   cat("loadData.R:: there are ", nrow(differences) , " values in taxon that are not in assambleages\n")
   
   cat("loadData.R: Values in assembleages and not in taxon\n")
   differences <- anti_join(assembleages, taxon , by = "id_study") %>% dplyr::select(id_study) %>% unique()
   cat("loadData.R:: there are ", nrow(differences) , " values in differences that are not in taxon\n")
  
    #Create index table with Common columns
     #check if exists index.csv
     cat("loadData.R: Creating index based in id_study\n")
     index <- taxon %>% dplyr::filter(id_study %in% assembleages$id_study) %>% dplyr::select(id_study)
     

  # Return the list
   # Create a list to hold the data frames
   result_list <- list(taxon = taxon, assembleages = assembleages, index = index)
   return(result_list)
}



