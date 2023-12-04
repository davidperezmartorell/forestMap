#' Select general data
#' @param index #Datastream index
#' @param assemblages #Datastream assemblages
#' @param taxon #Datastream taxon
#' @return unique_merged_data Result of query
#' @export
#' @examples
#' selectGeneral(index,assemblages,taxon)
# Funtion Loaddata ------------------------------------------------------
# Select general data
selectGeneral <- function(index,assemblages,taxon) {
  source("R/loadData.R")
  # Select values from assemblages based on index
  assemblages_data <- assemblages %>%
    filter(id_comm %in% index$id_comm & id_study %in% index$id_study)
  
  assemblages_data <- assemblages_data %>% select(id_comm, id_study, study_year, stage, country, study_common_taxon_clean, taxon_level, exact_lat, exact_long)
  
  # Select values from taxon based on index
  taxon_data <- taxon %>%
    filter(id_comm %in% index$id_comm & id_study %in% index$id_study)
  taxon_data <- taxon_data %>% select(id_comm, id_study,taxon_level , rank)
  
  # Merge taxon and assemblages based on common indices
  merged_data <- merge(assemblages_data, taxon_data, by = c("id_comm", "id_study"))
  
  # Remove duplicated rows
  unique_merged_data <- distinct(merged_data)
  return(unique_merged_data)

}




