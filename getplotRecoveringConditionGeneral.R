#' Returns Plot with relation recovering condition versus age for all species
#' @param  result_filtered dataframe mixed taxon and assembleages
#' @return  plot Plot with relation Family, age for all species
#' @export
#' @examples
#' getplotRecoveringConditionGeneral(taxon,assembleages,study)
getplotRecoveringConditionGeneral <- function(mergedAssembleagesTaxon) {
  
  # Convert the age column to a factor with labels corresponding to the new age groups
  result_filtered <- mergedAssembleagesTaxon %>% 
    mutate(age_group = cut(age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,75,100,200,300,400,500), include.lowest = TRUE))

  # Plotting the aggregated data with adjusted width and position of bars
  plotStage <- ggplot(result_filtered, aes(x = age_group, y = richness, fill = recovering_cond)) +
    geom_boxplot(position = position_dodge(width = 0.8), width = 0.7) +
    labs(title = "Richness by Recovering condition from all the studies", x = "Age groups", y = "Richness") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_x_discrete()  
  
  #browser
  #ggsave("www/plotRecoveringConditionGeneral.png", plot = plotStage, width = 2.67, height = 1.67, units = "in", dpi = 300)
  rm(result_filtered)
  return(plotStage)
}






