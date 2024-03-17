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
    mutate(age_group = cut(age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,75,100,200,300,400,500), include.lowest = TRUE)) %>%
    mutate(age_group = as.factor(age_group))
  
  # Plotting the data with boxplots for each age group, colored by broad_group
  plotStudyCommonTaxon <- ggplot(result_filtered, aes(x = age_group, y = richness, fill = broad_group)) +
    geom_boxplot() +  # Boxplot of richness for each age group, colored by broad_group
    labs(title = "Richness Variability by Grouped Categories and Age Group", x = "Age Group", y = "Richness") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_x_discrete() 
  
  #browser;
  #ggsave("www/plotInventoryByStudyCommonTaxonGeneral.png", plot = plotStage, width = 2.67, height = 1.67, units = "in", dpi = 300)
  rm(result_filtered)
  return(plotStudyCommonTaxon)
  
}

#ggsave("inst/InventoryByClassGeneral.png", plot = plotClass)
  

