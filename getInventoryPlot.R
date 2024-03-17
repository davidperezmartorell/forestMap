#' Returns Plot with relation abundance, age for all sspecies
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @param  study id_study
#' @return  plot Plot with relation abundance, age for all sspecies
#' @export
#' @examples
#' getInventoryPlot(taxon,assembleages,study)
getInventoryPlot<-function(result_filtered){
 

 # Calculate the median value of measurement for each family at each age
 mean_values <- result_filtered %>%
   group_by(age, family) %>%
   summarise(mean_values = mean(richness))
 
 #Choose 10 more important values
 summed_data <- mean_values %>%
        group_by(family) %>%
        summarise(total_measurement = sum(mean_values)) %>%
        ungroup()
 summed_data2 <- summed_data%>% head(10) %>% select(family)
 
 mean_values2 <- mean_values %>% filter(trimws(family) %in% summed_data2$family)
 
 # Plotting the aggregated data
 plot<-ggplot(mean_values2, aes(x = age, y = mean_values, color = family)) +
   geom_line() +
   geom_point(size = 2) +  # Add points for each measurement
   labs(title = "Mean more important values by richness family and age", x = "Age", y = "Mean by richness") +
   theme_minimal()
return(plot)

 
}
