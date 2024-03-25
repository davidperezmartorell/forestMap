#' Returns Plot with relation Class, age for all species
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @param  study id_study
#' @return  plot Plot with relation Class, age for all species
#' @export
#' @examples
#' getplotInventoryByStudyCommonTaxonGeneral(taxon,assembleages,study)
getplotInventoryByStudyCommonTaxonGeneral <- function(mergedAssembleagesTaxon) {
  #Filter possible NA or NULL values
  mergedAssembleagesTaxon %>%  filter(!is.na(study_common_taxon) & !is.na(age) & !is.na(richness))
  
  # Implement the grouping strategy
  mergedAssembleagesTaxon <- mergedAssembleagesTaxon %>%
    mutate(broad_group = case_when(
      str_to_lower(study_common_taxon) %in% c("trees", "trees and shrubs", "tree seedlings and saplings", 
                                              "woody plants", "shrubs", "shrubs and trees", "trees and lianas", 
                                              "trees, shrubs & herbs", "trees, shrubs and lianas seedlings", 
                                              "trees, shrubs and herbs", "woody", "woody and herbaceous") ~ "Trees & Shrubs",
      str_to_lower(study_common_taxon) %in% c("herbs", "herbaceous", "gramanoids", "forbes", "herbaceous plants and sub-shrubs", 
                                              "herbs and ferns", "spring ephemeral herbs", "summer herbs", "plantae",
                                              "plants", "understory plants", "small plants and climbers", "tracheophyta") ~ "Herbaceous",
      TRUE ~ "palms,lianas, and others"
    ))

  # Create age groups
  result_filtered <- mergedAssembleagesTaxon %>%
    #mutate(age_group = cut(age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,75,100,200,300,400,500), include.lowest = TRUE)) %>%
    #mutate(age_group = cut(age, breaks = c(0,10,20,30,40,50,60,70,80,90,100,500), include.lowest = TRUE))
    mutate(age_group = cut(age, breaks = c(0,10,20,30,40,50,60,70,80,90,100,110, 120, 130, 140, 150, 500), include.lowest = TRUE)) %>%
    mutate(age_group = as.factor(age_group))
  
  # Calculate the number of values represented in the plot
  num_values <- nrow(result_filtered)

  # Plotting the data with boxplots for each age group, colored by broad_group
  plotStage <- ggplot(result_filtered, aes(x = age_group, y = richness, fill = broad_group)) +
    geom_boxplot(position = position_dodge(width = 0.8), width = 0.5) +
    labs(title = paste("Richness by Common taxon from all the studies (", num_values, "values represented)"), x = "Age Group", y = "Richness") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_x_discrete() 
  
  # Convert ggplot to plotly
  plotlyPlot <- ggplotly(plotStage)
  
  # Modify layout to add legend at the bottom
  plotlyPlot <- plotlyPlot %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.2))  # Add legend at the bottom
  
  
  #ggsave("www/plotInventoryByStudyCommonTaxonGeneral.png", plot = plotStage, width = 2.67, height = 1.67, units = "in", dpi = 300)
  rm(result_filtered)
  return(plotlyPlot)
  
}

#ggsave("inst/InventoryByClassGeneral.png", plot = plotClass)
  

