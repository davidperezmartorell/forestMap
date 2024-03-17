#' Returns Plot with relation age for all specieswith points
#' @param  mergedAssembleagesTaxon dataframe mixed taxon and assembleages
#' @param  study id_study
#' @return  plot Plot with relation Family, age for all species
#' @export
#' @examples
#' getplotRichnessCloudAgeGeneralPoints(taxon,assembleages,study)
library(ggplot2)
library(dplyr)

getplotRichnessCloudAgeGeneralPoints <- function(mergedAssembleagesTaxon) {
  
  # Filter out repeated values only to increase speedand no importance plotting points that will be overlaped
  mergedAssembleagesTaxon <- distinct(mergedAssembleagesTaxon, age, richness)
  
  # Plotting the data with lines and points for each age group
  plotClass <- ggplot(mergedAssembleagesTaxon, aes(x = age, y = richness, group = 1)) +
    geom_point() +  # Add points
    labs(title = "Richness Variability by Age", x = "Age Group", y = "Richness") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_x_continuous()  # Use continuous scale for numeric x-axis values
  
  #browser;
  #ggsave("www/plotRichnessCloudAgeGeneralPoints.png", plot = plotClass, width = 2.67, height = 1.67, units = "in", dpi = 300)
  rm(mergedAssembleagesTaxon)
  return(plotClass)
}
