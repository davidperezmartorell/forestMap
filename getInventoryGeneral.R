#' Returns Plot with relation abundance, age for all sspecies
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @param  study id_study
#' @return  plot Plot with relation abundance, age for all sspecies
#' @export
#' @examples
#' getInventoryGeneral(taxon,assembleages,study)
getInventoryGeneral <- function(taxon, assembleages) {
 
  # Merge the dataframes based on common columns
  merged_data <- merge(taxon, assembleages, by = c("id_study", "id_comm"), all.x = TRUE)
  
  # Select the relevant columns
  result <- merged_data[, c("taxon_clean", "measurement", "age", "id_study", "id_comm", "study_common_taxon", "family", "class", "order", "stage")]
  
  # Filter out rows where symbol_type is NULL or NA
  result <- na.omit(result)
  result_filtered <- result %>% filter(!is.na(age) & !is.na(measurement) & !is.na(taxon_clean) )
  
  # Convert measurement and age columns to numeric
  result_filtered$measurement <- as.numeric(gsub(",", ".", result_filtered$measurement))
  result_filtered$age <- as.numeric(result_filtered$age)
  
  # Calculate the median value of measurement for each class at each age
  medianValuesclass <- result_filtered %>%
    group_by(age, class) %>%
    summarise(median_measurement = median(measurement))
  
  # Plotting the aggregated data
  plotclass <- ggplot(medianValuesclass, aes(x = age, y = median_measurement, color = class)) +
    geom_line() +
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Median Measurement by class at Each Age", x = "Age", y = "Median Measurement") +
    theme_minimal()
  
  # Calculate the median value of measurement for each family at each age
  medianValuesFamily <- result_filtered %>%
    group_by(age, family) %>%
    summarise(median_measurement = median(measurement))
  
  # Plotting the aggregated data
  plotFamily <- ggplot(medianValuesFamily, aes(x = age, y = median_measurement, color = Family)) +
    geom_line() +
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Median Measurement by Family at Each Age", x = "Age", y = "Median Measurement") +
    theme_minimal()
  
  # Calculate the median value of measurement for each order at each age
  medianValuesOrder <- result_filtered %>%
    group_by(age, order) %>%
    summarise(median_measurement = median(measurement))
  
  # Plotting the aggregated data
  plotOrder <- ggplot(medianValuesOrder, aes(x = age, y = median_measurement, color = Order)) +
    geom_line() +
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Median Measurement by Order at Each Age", x = "Age", y = "Median Measurement") +
    theme_minimal()
  
  # Calculate the median value of measurement for each stage at each age
  medianValuesStage <- result_filtered %>%
    group_by(age, stage) %>%
    summarise(median_measurement = median(measurement))
  
  # Plotting the aggregated data
  plotStage <- ggplot(medianValuesStage, aes(x = age, y = median_measurement, color = Stage)) +
    geom_line() +
    geom_point(size = 2) +  # Add points for each measurement
    labs(title = "Median Measurement by Stage at Each Age", x = "Age", y = "Median Measurement") +
    theme_minimal()
  
  return(list(plotclass = plotclass, plotFamily = plotFamily, plotOrder = plotOrder, plotStage = plotStage))
}
