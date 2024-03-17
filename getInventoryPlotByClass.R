#' Returns Plot with relation abundance, age for all sspecies
#' @param  result_filtered filtered data from taxon and assembleages and id_study
#' @return  plot Plot with relation abundance, age for all sspecies
#' @export
#' @examples
#' getInventoryPlotByclass(taxon,assembleages,study)
getInventoryPlotByClass<-function(result_filtered){

  # Calculate the median value of measurement for each family at each age
  mean_values <- result_filtered %>%
    group_by(age, class) %>%
    summarise(mean_values = mean(richness))
  
  #Choose 10 more important values
  summed_data <- mean_values %>%
    group_by(class) %>%
    summarise(total_measurement = sum(mean_values)) %>%
    ungroup()
  summed_data2 <- summed_data%>% head(10) %>% select(class)
  
  mean_values2 <- mean_values %>% filter(trimws(class) %in% summed_data2$class)
  
  # Plotting the aggregated data
  plot<-ggplot(mean_values2, aes(x = age, y = mean_values, color = class)) +
    geom_line() +
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Mean more important values by richness class and age", x = "Age", y = "Mean by richness") +
    theme_minimal()
  return(plot)

}


