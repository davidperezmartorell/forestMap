#' Returns Plot with relation Class, age for all species
#' @param  mergedAssembleagesTaxon inventory of taxon and asse3mbleages
#' @return  plot Plot with relation Class, age for all species
#' @export
#' @examples
#' getplotInventoryByClassGeneral(taxon,assembleages,study)
getplotInventoryByClassGeneral <- function(mergedAssembleagesTaxon) {

  # Calculate median richness per class and select top 25
  top_classes <- mergedAssembleagesTaxon %>%
    group_by(class) %>%
    #summarise(median_richness = median(richness), .groups = 'drop') %>%
    #arrange(desc(median_richness)) %>%
    slice_head(n = 10) %>%
    pull(class)
  
  # Filter for top classes and create age groups
  result_filtered <- mergedAssembleagesTaxon %>%
    filter(class %in% top_classes) %>%
    mutate(age_group = cut(age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,75,100,200,300,400,500), include.lowest = TRUE)) %>%
    mutate(age_group = as.factor(age_group))  # Ensure age_group is a factor for correct plotting
  
  # Plotting the data with boxplots for each age group, colored by class
  plotClass <- ggplot(result_filtered, aes(x = age_group, y = richness, fill = class)) +
    geom_boxplot() +  # Boxplot of richness for each age group, colored by class
    labs(title = "Richness Variability by Top 10 Classes and Age Group", x = "Age Group", y = "Richness") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_x_discrete() + # Use discrete scale for age groups
    scale_fill_manual(values = rainbow(15))
  
  rm(result_filtered)
  rm(top_classes)
  #ggsave("www/plotInventoryByClassGeneral.png", plot = plotStage, width = 2,67, height = 1,67, units = "in", dpi = 300)
  return(plotClass)
}


  

