#' Returns Plot with relation Class, age for all species
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @param  study id_study
#' @return  plot Plot with relation Class, age for all species
#' @export
#' @examples
#' getplotInventoryByClassGeneral(taxon,assembleages,study)
getplotInventoryByClassGeneral <- function(mergedAssembleagesTaxon) {

  result_filtered <- mergedAssembleagesTaxon
  
  # Calculate the median value of measurement for each class at each age
  medianValuesclass <- result_filtered %>%
    group_by(age, class) %>%
    summarise(median_measurement = median(richness))
  
  # Plotting the aggregated data
  plotClass <- ggplot(medianValuesclass, aes(x = age, y = median_measurement, color = class)) +
    geom_line() +
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Median Measurement of richness by class at Each Age", x = "Age", y = "Median Measurement") +
    theme_minimal()
  
  #ggsave("inst/InventoryByClassGeneral.png", plot = plotClass)
  return(plotClass)
}
