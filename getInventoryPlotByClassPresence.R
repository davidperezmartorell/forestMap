#' Returns Plot with relation abundance, age for all sspecies
#' @param  result_filtered filtered data from taxon and assembleages and id_study
#' @return  plot Plot with relation abundance, age for all sspecies
#' @export
#' @examples
#' getInventoryPlotByClassPresence(taxon,assembleages,study)
getInventoryPlotByClassPresence<-function(result_filtered){
  
  # Convert 'age' to numeric
  result_filtered$age <- as.numeric(result_filtered$age)
  
  # Plotting the aggregated data with jittering
  plot <- ggplot(result_filtered, aes(x = factor(age), y = class, size = count, color = class)) +
    geom_point(alpha = 1.2) +
    labs(title = "Class presence along the time", x = "Age", y = "Order") +
    scale_size(range = c(3, 20)) +  # Adjusts the minimum and maximum size of the circles
    theme_minimal() +
    theme(legend.position = "bottom")
  return(plot)

}


