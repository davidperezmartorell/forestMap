#' Returns Plot with relation abundance, age for all species
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @return  plot Plot with relation order , age for all species
#' @export
#' @examples
#' getplotInventoryByOrderGeneral(taxon,assembleages,study)
getplotInventoryByOrderGeneral<- function(mergedAssembleagesTaxon) {

  result_filtered <- mergedAssembleagesTaxon
  
  
  # Calculate the median value of measurement for each order at each age
  medianValuesOrder <- result_filtered %>%
    group_by(age, order) %>%
    summarise(median_measurement = median(richness))
  
  # Plotting the aggregated data
  plotOrder <- ggplot(medianValuesOrder, aes(x = age, y = median_measurement, color = order)) +
    geom_line() +
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Median Measurement of richness by Order at Each Age", x = "Age", y = "Median Measurement") +
    theme_minimal()
  
  #ggsave("inst/InventoryByOrderGeneral.png", plot = plotOrder)
  
  return(plotOrder)
}
