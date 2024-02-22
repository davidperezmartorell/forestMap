#' Returns Plot with relation Stage, age for all sspecies
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @param  study id_study
#' @return  plot Plot with relation stage, age for all sspecies
#' @export
#' @examples
#' getplotInventoryByStageGeneral(taxon,assembleages,study)
getplotInventoryByStageGeneral <- function(mergedAssembleagesTaxon) {

  result_filtered <- mergedAssembleagesTaxon
  

  # Calculate the median value of measurement for each stage at each age
  medianValuesStage <- result_filtered %>%
    group_by(age, stage) %>%
    richness  
  # Plotting the aggregated data
  plotStage <- ggplot(medianValuesStage, aes(x = age, y = median_measurement, color = stage)) +
    geom_line() +
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Median Measurement of richness by Stage at Each Age", x = "Age", y = "Median Measurement") +
    theme_minimal()
  #ggsave("inst/InventoryByStageGeneral.png", plot = plotStage)
  return(plotStage)
  
}
