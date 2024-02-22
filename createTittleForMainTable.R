#' Retunrs content of data according we plot and click
#' @param dataWorldMap DAta we are showing in the plot
#' @return tableHTML Data avilable
#' @export
#' @examples
#' createTittleForMainTable.R(inputData)
createTittleForMainTable <- function(dataWorldMap) {

  cat("createTittleForMainTable.R: Creating tittle table\n")
  #If we want to select only interesting values to print in a table . Discarted exact_lat,	exact_long,	Country,	ISO3 and	geometry columns
  dataWorldMap <- dataWorldMap[, c("id_study", "id_comm")]
  dataWorldMap <- dataWorldMap[!duplicated(dataWorldMap), ]
  
  # Rename columns for better readability
  col_names <- colnames(dataWorldMap)
  col_names <- c("Study", "Community")
  colnames(dataWorldMap) <- col_names
  
  # Create the title with the extracted values
  id_study_values <- unique(dataWorldMap$Study)
  id_comm_values <- unique(dataWorldMap$Community)
  #Format in Htittle table
  titleHTML <- paste("<div id='belowTitle'>",
                   "<h2><strong>Contents of the Study:</strong> ", paste(id_study_values, collapse = ", "), "</h2>",
                   #"<h2><strong>Contents of the Community:</strong> ", paste(id_comm_values, collapse = ", "), "</h2>",
                   "<table border='1'><tbody><tr>",
                   "<th>Study</th><th>Community</th>",
                   "</tr><tr>",
                   "<td>", id_study_values, "</td>",
                   #"<td>", id_comm_values, "</td>",
                   "</tr></tbody></table></div>")


  # Start building the HTML table
  tableHTML <- "<table border='1'>"

  # Add table headers
  tableHTML <- paste(tableHTML, "<tr>", paste("<th>", col_names, "</th>", collapse = ""), "</tr>", sep = "")
  
  # Close the table
  tableHTML <- paste(tableHTML, "</table>", sep = "")
  
  # Combine title and table
  finalHTML <- paste(titleHTML, tableHTML)

  # Return the HTML
  return(HTML(finalHTML))

}
