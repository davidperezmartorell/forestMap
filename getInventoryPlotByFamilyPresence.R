#' Returns Plot with relation abundance, age for all sspecies
#' @param  result_filtered filtered data from taxon and assembleages and id_study
#' @return  plot Plot with relation abundance, age for all sspecies
#' @export
#' @examples
#' getInventoryPlotByFamilyPresence(taxon,assembleages,study)
getInventoryPlotByFamilyPresence<-function(result_filtered){

  # Convert 'age' to numeric
  result_filtered$age <- as.numeric(result_filtered$age)
  # Plotting the aggregated data
  plot <- ggplot(result_filtered, aes(x = factor(age), y = family, size = count, color = family)) +
    geom_point(alpha = 0.7) +
    labs(title = "Family presence along the time", x = "Age", y = "Presence by family") +
    scale_size(range = c(1, 15)) +  # Adjusts the minimum and maximum size of the circles
    theme_minimal() +
    theme(
      panel.grid = element_line(linewidth = 0.3, color = "grey"),
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 1, color = "black"),
      text = element_text(size = 16),
      axis.text = element_text(size = 16),
      legend.position = "none" # Hides the legend
    )
    theme(legend.position = "none") # Hides the legend
  
  plot + scale_x_continuous(limits = c(0, NA)) # Ensures x-axis starts at 0
  
  return(plot)

}


