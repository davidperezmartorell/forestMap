#' Returns Plot with relation Family, age for all species
#' @param  mergedAssembleagesTaxon dataframe mixed taxon and assembleages
#' @param  study id_study
#' @return  plot Plot with relation Family, age for all species
#' @export
#' @examples
#' getplotRichnessCloudAgeGeneral(taxon,assembleages,study)
library(ggplot2)
library(dplyr)

getplotRichnessCloudAgeGeneral <- function(mergedAssembleagesTaxon) {
  
  # Creating age groups
  result_filtered <- mergedAssembleagesTaxon %>% 
    mutate(age_group = cut(age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,75,100,200,300,400,500), include.lowest = TRUE))
  
  # Convert age_group to a factor to ensure it is plotted correctly
  result_filtered$age_group <- as.factor(result_filtered$age_group)
  
  # Plotting the data with boxplots for each age group
  plotClass <- ggplot(result_filtered, aes(x = age_group, y = richness)) +
    geom_boxplot() +  # Boxplot of richness for each age group
    labs(title = "Richness Variability by Age", x = "Age Group", y = "Richness") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_x_discrete()  
  
  #browser
  #ggsave("www/plotRichnessCloudAgeGeneral.png", plot = plotStage, width = 2.67, height = 1.67, units = "in", dpi = 300)
  rm(result_filtered)
  return(plotClass)
}
