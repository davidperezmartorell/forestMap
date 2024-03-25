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
    summarise(mean_values = median(richness))
  
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
    geom_point(size = 5) +  # Add points for each measurement
    labs(title = "Median 10 best values by richness class and age", x = "Age", y = "Richness median") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(mean_values2$mean_values) * 1.1)) +  # Ensure y-axis starts from 0 and extends a bit beyond the maximum value
    theme_minimal() +
    theme(
      panel.grid = element_line(linewidth = 0.3, color = "grey"),
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 1, color = "black"),
      text = element_text(size = 16),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16)
    )
  return(plot)

}


