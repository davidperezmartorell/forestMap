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
                                                ifelse(disturbance1_age_clean %in% c("logging", "logging/farming", "forest uses", "forest uses/logging"), "forest",
                                                       ifelse(disturbance1_age_clean %in% c("cultivation", "animal farming", "plantation", "plantation/logging"), "cultivation", "others")))))
  result_filtered <- result_filtered %>% filter(combined_disturbance != "others")
  
  # Define age groups
  result_filtered$age_group <- cut(result_filtered$age, breaks = c(0, 5, 10, 15, 20, 50, 500), include.lowest = TRUE)
  
  # Apply the filter to remove extreme values from the top
  medianValuesDisturbance <- result_filtered %>%
    group_by(age_group,combined_disturbance) %>%
    summarise(median_measurement = sum(richness), .groups = "drop")

  # Plotting the aggregated data
  plotDisturbance <- ggplot(medianValuesDisturbance, aes(x = age_group, y = median_measurement, color = combined_disturbance)) +
    geom_line(aes(group = combined_disturbance)) +  # Unir puntos en horizontal
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Median Measurement by richness and Disturbance", x = "Age", y = "Measurement", color = "Disturbance") +
    theme_minimal() +
    scale_x_discrete(breaks = unique(medianValuesDisturbance$age_group))

  return(plotDisturbance)
}
