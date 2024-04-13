#' Returns Plot with relation Stage, age for all sspecies
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @param  study id_study
#' @return  plot Plot with relation stage, age for all sspecies
#' @export
#' @examples
#' getplotInventoryByStageGeneral(taxon,assembleages,study)
getplotInventoryByStageGeneral <- function(mergedAssembleagesTaxon) {

  # Convert the age column to a factor with labels corresponding to the new age groups
  result_filtered <- mergedAssembleagesTaxon %>% 
    #mutate(age_group = cut(age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,75,100,200,300,400,500), include.lowest = TRUE))
    mutate(age_group = cut(age, breaks = c(0,10,20,30,40,50,60,70,80,90,100,500), include.lowest = TRUE))
    #mutate(age_group = cut(age, breaks = c(0,10,20,30,40,50,60,70,80,90,100,110, 120, 130, 140, 150, 500), include.lowest = TRUE))
  
  # Calculate the number of values represented in the plot
  num_values <- nrow(result_filtered)
  
  # Plotting the aggregated data with log values
  plotStage <- ggplot(result_filtered, aes(x = age_group, y = richness, fill = stage)) +
    geom_boxplot(position = position_dodge(width = 0.8), width = 0.1) +
    labs(title = paste("Taxon number by Stage from all the studies (", num_values, "values represented)"),x = "Age groups", y = "Taxon number",
         fill = "Stage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_x_discrete()
  
  # # Convert ggplot to plotly
  # plotlyPlot <- ggplotly(plotStage)
  
  # # Modify layout to add legend at the bottom
  # plotlyPlot <- plotlyPlot %>%
  #   layout(legend = list(orientation = "h", x = 0, y = -0.2))  # Add legend at the bottom
  
  
  #ggsave("www/plotInventoryByStageGeneral.png", plot = plotStage, width = 2.67, height = 1.67, units = "in", dpi = 300)
  

  rm(result_filtered)
  # return(plotlyPlot)
  return(plotStage)

  
}
