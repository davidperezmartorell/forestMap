#' Returns Plot with relation abundance, age for all sspecies
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @param  study id_study
#' @return  plot Plot with relation abundance, age for all sspecies
#' @export
#' @examples
#' getInventoryPlot(taxon,assembleages,study)
getInventoryPlot<-function(taxon,assembleages,study){

 taxon <- taxon[taxon$id_study %in% study & !is.na(taxon$measurement) & taxon$measurement >= 0.1, ]

 assembleages <- assembleages[assembleages$id_study %in% study, ]

#Load inventarioCalvino
 inventarioCalvino<- read.csv("inst/inventarioCalvino.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE)#, fileEncoding="latin1")

# Merge the dataframes based on common columns
 merged_data <- merge(taxon, assembleages, by = c("id_study", "id_comm"), all.x = TRUE)

# Merge the result with inventarioCalvino based on common columns
 merged_data <- merge(merged_data, inventarioCalvino, by.x = "taxon_clean", by.y = "scientificName", all.x = TRUE)

# Select the relevant columns
 result <- merged_data[, c("taxon_clean", "measurement", "age", "id_study", "id_comm", "study_common_taxon", "family", "genus", "IsGymnosperm", "form")]

# Filter out rows where symbol_type is NULL or NA
 result <- na.omit(result)
 result_filtered <- result %>% filter(!is.na(age) & !is.na(measurement) & !is.na(taxon_clean) & !is.na(form))

# Create the plot with symbols
 library(plotly)
 inventoryPlot <- plot_ly(result_filtered, x = ~age, y = ~measurement,
                         color = ~family, #color = ~taxon_clean, symbol = ~form,
                         type = "scatter", mode = "markers", size = ~5,
                         hoverinfo = "text",
                         text = ~paste("Family: ", family, "<br>Species: ", taxon_clean, "<br>Form: ", form, "<br>Value: ", measurement)) %>%
  layout(
    xaxis = list(title = "Age"),
    yaxis = list(title = "Measurement (log scale)", type = "log"),
    showlegend = TRUE
  )


return(inventoryPlot)

}
