#' Returns table related to the study related to the id_comm clicked in the map
#' @param taxon Identifier of individual tear clicked in map
#' @param assemblages Identifier of individual tear clicked in map
#' @param identifier Identifier of individual tear clicked in map
#' @return tableHTML Data avilable
#' @export
#' @examples
#' commRelated(id_comm)
  commRelated <- function(taxon,assemblages, identifier) {
 
    # Assuming 'assemblages' is an sf object
    assemblages <- st_drop_geometry(assemblages)

    #Select interesting values from assemblages to print in a table
    assemblages <- assemblages %>%
      filter(id_comm == identifier) %>%
      dplyr::select(id_study, id_comm, age, stage, study_common_taxon, study_year, taxon_level, disturbance1_age_clean, disturbance2_clean, study_common_taxon_clean,
             predisturbances_clean, current_impact_clean, postdisturbance, restoration, protection, use, recovering_cond, coord_accuracy)

    #Select interesting values from taxon to print in a table
    taxon <- filter(taxon, id_comm == identifier)
    taxon <- taxon$id_comm %>% unique()
    taxon <- data.frame(id_comm = taxon)
    
    taxon_assembleages <- left_join(assemblages, taxon, by = "id_comm")

      # Get column names from the data frame
      col_names <- colnames(taxon_assembleages)
      # Create the title with the extracted values
      title <- paste("Contents of", paste("id_study:", unique(taxon_assembleages$id_study), collapse = ", "), "and", 
                     paste("id_comm:", identifier, collapse = ", "))
      
      # Create the title with the extracted values
      tableHTML <- paste("<h2>", title, "</h2>","<table border='1'>", "<tr>")
      
      # Start building the HTML table
      tableHTML <- "<table border='1'>"
    
      # Add table headers
      tableHTML <- paste(tableHTML, "<tr>", paste("<th>", col_names, "</th>", collapse = ""), "</tr>", sep = "")
      
      # Add table rows
      for (i in nrow(assemblages)) {
        tableHTML <- paste(tableHTML, "<tr>", paste("<td>", unlist(taxon_assembleages  [i, ]), "</td>", collapse = ""), "</tr>", sep = "")
      }
      
      # Close the table
      tableHTML <- paste(tableHTML, "</table>", sep = "")
    
    # Return the HTML
    return(HTML(tableHTML))
  
  }
