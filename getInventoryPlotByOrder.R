#' Returns Plot with relation abundance, age for all sspecies
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @param  study id_study
#' @return  plot Plot with relation abundance, age for all sspecies
#' @export
#' @examples
#' getInventoryPlotByorder(taxon,assembleages,study)
getInventoryPlotByOrder<-function(taxon,assembleages,study){

 taxon <- taxon[taxon$id_study %in% study & !is.na(taxon$measurement) & taxon$measurement >= 0.1, ]

 assembleages <- assembleages[assembleages$id_study %in% study, ]

# #Load inventarioCalvino
#  inventarioCalvino<- read.csv("inst/inventarioCalvino.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE)#, fileEncoding="latin1")
# 
# # Merge the dataframes based on common columns
merged_data <- merge(taxon, assembleages, by = c("id_study", "id_comm"), all.x = TRUE)
# 
# # Merge the result with inventarioCalvino based on common columns
#  merged_data <- merge(merged_data, inventarioCalvino, by.x = "taxon_clean", by.y = "scientificName", all.x = TRUE)

# Select the relevant columns
 result <- merged_data[, c("taxon_clean", "measurement", "age", "id_study", "id_comm", "study_common_taxon", "family", "order")]

# Filter out rows where symbol_type is NULL or NA
 result <- na.omit(result)
 result_filtered <- result %>% filter(!is.na(age) & !is.na(measurement) & !is.na(taxon_clean) )

 # Convert measurement and age columns to numeric
 result_filtered$measurement <- as.numeric(gsub(",", ".", result_filtered$measurement))
 result_filtered$age <- as.numeric(result_filtered$age)
 
# Create the plot with symbols
 library(plotly)
 # inventoryPlot <- plot_ly(result_filtered, x = ~age, y = ~measurement,
 #                          color = ~Family,
 #                          type = "scatter", mode = "markers", size = ~5,
 #                          hoverinfo = "text",
 #                          text = ~paste("Family: ", Family, "<br>Species: ", taxon_clean,  "<br>Value: ", measurement)) %>%
 #   layout(
 #     xaxis = list(title = "Age"),
 #     yaxis = list(title = "Measurement (log scale)"),
 #     showlegend = TRUE
 #   )

 # Calculate the median value of measurement for each family at each age
 median_values <- result_filtered %>%
   group_by(age, order) %>%
   summarise(median_measurement = median(measurement))
 
 # Plotting the aggregated data
 plot<-ggplot(median_values, aes(x = age, y = median_measurement, color = order)) +
   geom_line() +
   geom_point(size = 2) +  # Add points for each measurement
   labs(title = "Median Measurement by order at Each Age", x = "Age", y = "Median Measurement") +
   theme_minimal()
 
return(plot)

}
