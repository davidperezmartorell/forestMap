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
    mutate(age_group = cut(age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,75,100,200,300,400,500), include.lowest = TRUE))
  
  # Plotting the aggregated data with adjusted width and position of bars
  plotStage <- ggplot(result_filtered, aes(x = age_group, y = richness, fill = combined_disturbance)) +
    geom_boxplot(position = position_dodge(width = 0.8), width = 0.7) +
    labs(title = "Richness by Disturbance Age from all the studies", x = "Age groups", y = "Richness") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_x_discrete() 
  
  
  #browser
  #ggsave("www/plotRichnessByDisturbanceAgeGeneral.png", plot = plotStage, width = 2.67, height = 1.67, units = "in", dpi = 300)
  rm(result_filtered)
  return(plotStage)
}






