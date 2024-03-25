#' Returns all species related to clicked study
#' @param taxon Identifier of individual tear clicked in map
#' @param id_comm Identifier of individual tear clicked in map
#' @return tableHTML Data avilable
#' @export
#' @examples
#' speciesStudyClicked(id_comm)
speciesStudyClicked <- function(taxon, idStudyUnique) {
  library("DT")
  
  cat("speciesStudyMatrix.D: Creating matrix species related\n")
  
  # Select interesting values from assemblages
  taxon <- taxon %>% filter(id_study == idStudyUnique)
  
  # Select necessary columns
  taxon <- taxon %>% dplyr::select(taxon_clean, id_study, measurement)
  

  
  # Pivot the data to create a matrix
  matrix_data <- taxon %>% 
    pivot_wider(names_from = id_study, values_from = measurement, values_fn = list(measurement = list)) %>%
    arrange(taxon_clean)
  
  # Extract row names and column names
  row_names <- matrix_data$taxon_clean
  col_names <- colnames(matrix_data)[-1]  # Exclude the first column which is taxon_clean
  
  # Extract the matrix values
  matrix_values <- as.matrix(matrix_data[, -1])
  
  # Return the matrix along with row and column names
  return(list(matrix = matrix_values, row_names = row_names, col_names = col_names))
}