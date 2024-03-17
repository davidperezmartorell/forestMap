#' Returns Plot with relation abundance, age for all species
#' @param  mergedAssembleagesTaxon inventory of taxon and asse3mbleages
#' @return  plot Plot with relation order , age for all species
#' @export
#' @examples
#' getplotInventoryByOrderGeneral(taxon,assembleages,study)
library(ggplot2)
library(dplyr)

getplotInventoryByOrderGeneral <- function(mergedAssembleagesTaxon) {
  
  # Calculate median richness per order
  orders_summary <- mergedAssembleagesTaxon %>%
    group_by(order) 
    #summarise(median_richness = median(richness), .groups = 'drop') %>%
    #arrange(desc(median_richness))
  
  # Identify top 10 orders
  top_orders <- head(orders_summary$order, 10)
  
  # Group all other orders into "Other"
  mergedAssembleagesTaxon$grouped_order <- ifelse(mergedAssembleagesTaxon$order %in% top_orders, mergedAssembleagesTaxon$order, "Other")
  
  # Filter and create age groups (no need to filter here as we are grouping non-top orders as "Other")
  result_filtered <- mergedAssembleagesTaxon %>%
    mutate(age_group = cut(age, breaks = c(0,5,10,15,20,25,30,35,40,45,50,75,100,200,300,400,500), include.lowest = TRUE)) %>%
    mutate(age_group = as.factor(age_group))  # Ensure age_group is a factor for correct plotting
  
  # Plotting the data with boxplots for each age group, colored by order
  plotOrder <- ggplot(result_filtered, aes(x = age_group, y = richness, fill = grouped_order)) +
    geom_boxplot() +  # Boxplot of richness for each age group, colored by grouped order
    labs(title = "Richness Variability by Top 10 Orders and Age Group", x = "Age Group", y = "Richness") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    scale_fill_manual(values = rainbow(11))  # Plus one for the "Other" category
  
  rm(orders_summary)
  rm(top_orders)
  rm(result_filtered)
  #ggsave("www/plotInventoryByOrderGeneral.png", plot = plotStage, width = 2.67, height = 1.67, units = "in", dpi = 300)
  return(plotOrder)
}

