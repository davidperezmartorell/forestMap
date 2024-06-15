#' Returns Plot with relation Family, age for all species
#' @param  mergedAssembleagesTaxon dataframe mixed taxon and assembleages
#' @param  study id_study
#' @return  plot Plot with relation Family, age for all species
#' @export
#' @examples
#' getplotRichnessByDisturbanceAgeGeneral(taxon,assembleages,study)
getplotRichnessByDisturbanceAgeGeneral <- function(mergedAssembleagesTaxon) {

  result_filtered <- mergedAssembleagesTaxon %>%
    filter(disturbance1_age_clean != "") %>%
    filter(disturbance1_age_clean != "none") %>%
    mutate(combined_disturbance = ifelse(disturbance1_age_clean %in% c("burning", "burning/logging/farming", "burning/farming"), "burning",
                                   ifelse(disturbance1_age_clean == "mining", "mining",
                                    ifelse(disturbance1_age_clean %in% c("logging", "logging/farming", "forest uses", "forest uses/logging"), "logging",
                                     ifelse(disturbance1_age_clean %in% c("cultivation","farming", "animal farming", "plantation", "plantation/logging"), "farming", "others")))))

  result_filtered <- result_filtered %>% filter(combined_disturbance != "others")
  
  # Convert the age column to a factor with labels corresponding to the new age groups
  result_filtered <- result_filtered %>% 
    #mutate(age_group = cut(age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,75,100,200,300,400,500), include.lowest = TRUE))
    mutate(age_group = cut(age, breaks = c(0,10,20,30,40,50,60,70,80,90,100,500), include.lowest = TRUE))
    #mutate(age_group = cut(age, breaks = c(0,10,20,30,40,50,60,70,80,90,100,110, 120, 130, 140, 150, 500), include.lowest = TRUE))
  
  # Calculate the number of values represented in the plot
  num_values <- nrow(result_filtered)

  
  # Plotting the aggregated data with adjusted width and position of bars
  # plotStage <- ggplot(result_filtered, aes(x = age_group, y = richness, fill = combined_disturbance)) +
  #   geom_boxplot(position = position_dodge(width = 0.8), width = 1) +
  #   labs(title = paste("Taxon number by disturbance from all the studies (", num_values, "values represented)"), x = "Age Group", y = "Taxon number",
  #        fill = "Disturbances") +
  #   theme_minimal() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  #   scale_x_discrete() 
  plotStage <- ggplot(result_filtered, aes(x = factor(age_group), y = richness, fill = combined_disturbance, color = combined_disturbance)) +
    geom_boxplot(position = position_dodge(width = 0.8), width = 1) +
    labs(title = paste("Taxon number by disturbance from all the studies (", num_values, "values represented)"), 
         x = "Age Group", y = "Taxon number", fill = "Disturbances") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_x_discrete() +
    geom_smooth(aes(group = combined_disturbance), method = "lm", se = FALSE)  # Añadir líneas de tendencia por grupo
  
  
  
  # # Convert ggplot to plotly
  # plotlyPlot <- ggplotly(plotStage)
  # 
  # # Modify layout to add legend at the bottom
  # plotlyPlot <- plotlyPlot %>%
  #   layout(legend = list(orientation = "h", x = 0, y = -0.2))  # Add legend at the bottom
  

  ggsave("www/plotRichnessByDisturbanceAgeGeneral.png", plot = plotStage, width = 2.67, height = 1.67, units = "in", dpi = 300)
  rm(result_filtered,mergedAssembleagesTaxon)
  # return(plotlyPlot)
  return(plotStage)
}






