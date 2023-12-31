#' Retunrs content of data according we plot and click
#' @param dataWorldMap DAta we are showing in the plot
#' @return tableHTML Data avilable
#' @export
#' @examples
#' generateTableText(inputData)
generateTableText <- function(dataWorldMap) {
  #Select interesting values to print in a table
  dataWorldMap <- dataWorldMap[, c("id_study", "id_comm", "taxon_level.x", "rank", "study_year", "stage", "study_common_taxon_clean", "taxon_level.y")]
  dataWorldMap <- dataWorldMap[!duplicated(dataWorldMap), ]
  
  
  # Get column names from the data frame
  col_names <- colnames(dataWorldMap)
  
  # Create the title with the extracted values
  id_study_values <- unique(dataWorldMap$id_study)
  id_comm_values <- unique(dataWorldMap$id_comm)
  title <- paste("Contents of", paste("id_study:", id_study_values, collapse = ", "), "and", 
                 paste("id_comm:", id_comm_values, collapse = ", "))
  
  # Create the title with the extracted values
  tableHTML <- paste("<h2>", title, "</h2>","<table border='1'>", "<tr>")
  
  # Start building the HTML table
  tableHTML <- "<table border='1'>"

  # Add table headers
  tableHTML <- paste(tableHTML, "<tr>", paste("<th>", col_names, "</th>", collapse = ""), "</tr>", sep = "")
  
  # Add table rows
  for (i in 1:nrow(dataWorldMap)) {
    tableHTML <- paste(tableHTML, "<tr>", paste("<td>", unlist(dataWorldMap[i, ]), "</td>", collapse = ""), "</tr>", sep = "")
  }
  
  # Close the table
  tableHTML <- paste(tableHTML, "</table>", sep = "")
  
  # Return the HTML
  return(HTML(tableHTML))

}
