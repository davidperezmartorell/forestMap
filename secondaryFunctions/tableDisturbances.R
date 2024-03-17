#Para crgar archivo
source("tableDisturbances.R"); #Create table with disturbances info

#En el menu UI
column(12, DT::dataTableOutput("tableDisturbances"))

#esto dentro del main
output$tableDisturbances <- DT::renderDataTable({
  tableDisturbances <- tableDisturbances(assembleages,click$id)
  # Render table with diturbance values
  datatable(tableDisturbances, options = list(dom = 't', pageLength = 100, scrollX = TRUE), caption = tags$caption(tags$h1("Disturbances along the time for ",click$id)))
})


#' Returns table related to the id_comm clicked in the map
#' @param taxon Identifier of individual tear clicked in map
#' @param assemblages Identifier of individual tear clicked in map
#' @param id_comm Identifier of individual tear clicked in map
#' @return tableHTML Data avilable
#' @export
#' @examples
#' tableDisturbances(id_comm)
tableDisturbances <- function(assemblages, identifier) {

  cat("tableDisturbances.R: Creating disturbance table\n")

  # Assuming 'assemblages' is an sf object
  assemblages_ori <- st_drop_geometry(assemblages)
  
  #Select id_study where id (study clicked) pertains
  idStudyCommon <- assemblages_ori %>% filter(str_detect(id, fixed(identifier))) %>% dplyr::select(id_study) %>%
    distinct() %>% pull(id_study)
  
  # Select interesting values from assemblages to print in a table
  selected_cols <- c(
    "age", "stage", "id_study","id", "id_comm", "study_common_taxon", "historic_impact", "predisturbances", "disturbance1_age",
    "disturbance2", "n_disturbances", "current_impact", "notes", "priority",
    "study_common_taxon_clean", "study_common_taxon_strgroup", "disturbance1_age_clean",
    "disturbance2_clean", "predisturbances_clean", "current_impact_clean", "postdisturbance",
    "restoration", "protection", "use"
  )
  
  # Convert all columns to character
  assemblages <- assemblages_ori %>% 
    filter(id_study == idStudyCommon) %>%
    dplyr::select(all_of(selected_cols)) %>%
    mutate_all(as.character)
  
  # Get unique values of "age" to create columns
  age_values <- unique(assemblages$age)
  
  # Create an empty data frame with columns for age and selected_cols
  result <- data.frame(age = character(0), stringsAsFactors = FALSE)
  
  # Check if there are age values before proceeding
  if (length(age_values) > 0) {
    # Loop through each selected column
    for (col_name in selected_cols) {
      # Initialize a vector to store the result for each age
      age_results <- character(length(age_values))
      
      # Loop through each age
      for (i in seq_along(age_values)) {
        age <- age_values[i]
        
        # Filter data for the current age and column
        filtered_vals <- assemblages[assemblages$age == age, col_name]
        
        # Extract unique non-NA values
        unique_vals <- unique(na.omit(filtered_vals))
        
        # Store the result for the current age
        age_results[i] <- paste(unique_vals, collapse = "; ")
      }
      

      # Replace empty or "none" values with "no data available"
      age_results[age_results %in% c("", "none", "character(0)")] <- NA
      age_results[is.na(age_results)] <- "no data available"
      
      
      
      # Add the age_results vector as a new row in the result data frame
      result <- rbind(result, c(col_name, age_results))
    }
    
    # Set column names
    colnames(result) <- c("Variable", age_values)
    
    # If you want "NA" instead of empty strings for missing values
    result[result == ""] <- NA
    
    # Remove the first row, which was used for initialization
    result <- result[-1, ]
  }

  return(result)
}
