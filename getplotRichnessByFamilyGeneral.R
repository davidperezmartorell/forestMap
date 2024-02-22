#' Returns Plot with relation Family, age for all species
#' @param  mergedAssembleagesTaxon dataframe mixed taxon and assembleages
#' @param  study id_study
#' @return  plot Plot with relation Family, age for all species
#' @export
#' @examples
#' getplotInventoryByFamilyGeneral(taxon,assembleages,study)
getplotRichnessByFamilyGeneral <- function(mergedAssembleagesTaxon) {

  result_filtered <- mergedAssembleagesTaxon
  
  # Apply the filter to remove extreme values from the top
  medianValuesFamily <- result_filtered %>%
    group_by(age, family) %>%
    summarise(median_measurement = median(richness))
  

  # Plotting the aggregated data
  plotFamily <- ggplot(medianValuesFamily, aes(x = age, y = median_measurement, color = family)) +
    geom_line() +
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Median Measurement  of richness by Family at Each Age", x = "Age", y = "Median Measurement") +
    theme_minimal() 
  
  #ggsave("inst/InventoryByRichnessGeneral.png", plot = plotFamily)
  
  return(plotFamily)
}
