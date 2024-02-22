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
    mutate(age_group = cut(age, breaks = c(0,1,2,3,4,5,6,7,8,9,10, 15,20,25,30,35,40,45,50,100,200,300,400,500), include.lowest = TRUE))
  
  # Calculate the median value of measurement for each stage at each age group
  median_measurement <- result_filtered %>%
    group_by(age_group, stage) %>%
    summarise(median_measurement = mean(richness))
  
  # Filter only the values where stage is equal to "recovering"
  medianValuesStage <- median_measurement %>% filter(stage == "recovering")
  
  
  # Plotting the aggregated data with log values
  plotStage <- ggplot(medianValuesStage, aes(x = age_group, y = median_measurement, fill = stage)) +
    geom_col(position = "dodge") +  # Use geom_col to plot columns
    labs(title = "Mean measurement of richness by Stage", x = "Age", y = "Measurement") +
    theme_minimal() +
    scale_x_discrete(breaks = unique(medianValuesStage$age_group)) +  # Specify x-axis breaks
    theme(legend.position = "none")  # Remove legend
  
  return(plotStage)
}

