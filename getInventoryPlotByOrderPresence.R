#' Returns Plot with relation abundance, age for all sspecies
#' @param  result_filtered filtered data from taxon and assembleages and id_study
#' @return  plot Plot with relation abundance, age for all sspecies
#' @export
#' @examples
#' getInventoryPlotByOrderPresence(taxon,assembleages,study)
getInventoryPlotByOrderPresence<-function(result_filtered){
  # Convert 'age' to numeric
  result_filtered$age <- as.numeric(result_filtered$age)
  
  #Select the more important values
  # Step 1: Calculate the total count for each family
  order_counts <- aggregate(count ~ order, data = result_filtered, FUN = sum)
  
  # Step 2: Sort the families based on their total count
  sorted_order <- order_counts[order(order_counts$count, decreasing = TRUE), ]
  
  # Step 3: Select the top 10 families
  top_10_order <- head(sorted_order$order, 10)
  # Filter result_filtered based on the top 10 families
  result_filtered <- result_filtered[result_filtered$order %in% top_10_order, ]
  
  #Group by class      
  result_filtered <- result_filtered %>%
    group_by(order, age) %>%
    summarise(total_count = sum(count)) %>%
    top_n(10, total_count)  
  
  # Plotting the aggregated data with jittering
  plot <- ggplot(result_filtered, aes(x = age, y = total_count, color = order)) +
    geom_line() +
    geom_point(size = 5) +
    labs(title = "Taxon by order", x = "Age", y = "Taxon count") +
    theme_minimal() +
    theme(
      panel.grid = element_line(linewidth = 0.3, color = "grey"),
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 1, color = "black"),
      text = element_text(size = 16),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16)
    )
  
  
  plot + scale_x_continuous(limits = c(0, NA)) # Ensures x-axis starts at 0
  
  return(plot)

}


