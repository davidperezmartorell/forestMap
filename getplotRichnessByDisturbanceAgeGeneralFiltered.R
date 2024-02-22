#' Returns Plot with relation Family, age for all species
#' @param  mergedAssembleagesTaxon dataframe mixed taxon and assembleages
#' @param  study id_study
#' @return  plot Plot with relation Family, age for all species
#' @export
#' @examples
#' getplotRichnessByDisturbanceAgeGeneral(taxon,assembleages,study)
getplotRichnessByDisturbanceAgeGeneral <- function(mergedAssembleagesTaxon) {
  result_filtered1 <- filtered_data
  result_filtered1 <- assembleages %>%
    filter(disturbance1_age_clean != "") %>%
    filter(disturbance1_age_clean != "none") %>%
    mutate(combined_disturbance = ifelse(disturbance1_age_clean %in% c("burning", "burning/logging/farming", "burning/farming"), "burning",
                                         ifelse(disturbance1_age_clean == "mining", "mining", 
                                                ifelse(disturbance1_age_clean %in% c("logging", "logging/farming", "forest uses", "forest uses/logging"), "forest",
                                                       ifelse(disturbance1_age_clean %in% c("cultivation", "animal farming", "plantation", "plantation/logging"), "cultivation", "others")))))
  
  result_filtered2 <- result_filtered1 %>%
    mutate(recovering_cond_group = case_when(
      recovering_cond %in% c("protection") ~ "Protection",
      recovering_cond %in% c("restoration", "restoration/protection", "restoration/use") ~ "Restoration",
      TRUE ~ "Others"
    ))
  
  
  result_filtered3 <- result_filtered2 %>% filter(combined_disturbance %in% c("cultivation", "forest"))
  result_filtered4 <- result_filtered3 %>% filter(recovering_cond_group %in% c("Restoration","Protection"))
  result_filtered4$age <- as.numeric(as.character(result_filtered4$age))
  result_filtered4 <- result_filtered4 %>% filter(age < 100)
  # Define age groups
  #result_filtered4$age_group <- cut(result_filtered4$age, breaks = c(0,5, 10, 15, 20, 25,50,100,250, 500), include.lowest = TRUE)
  
  # Apply the filter to remove extreme values from the top
  medianValuesDisturbance <- result_filtered4 %>%
    group_by(age,combined_disturbance,recovering_cond_group) %>%
    summarise(median_measurement = mean(richness), .groups = "drop")

  medianValuesDisturbance <- medianValuesDisturbance %>%
    mutate(combined_group = paste(combined_disturbance, recovering_cond_group, sep = "_"))
  
  # Plotting the aggregated data
  plotDisturbance <- ggplot(medianValuesDisturbance, aes(x = age, y = median_measurement, color = combined_group)) +
    geom_smooth(method = "loess", se = FALSE, aes(group = combined_group)) +  # Add trend line
    labs(title = "Mean richess respect disturbance and stage", x = "Age", y = "Median Measurement", color = "Disturbance") +
    theme_minimal()
  plotDisturbance
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # FILTER STUDIES WHERE DISTURBANCE WAS FOREST OR LOGGING
  setwd("C:/Users/piura/Desktop/forestMap")
  library("dplyr"); library("ggplot2");library("plotly")
  source("mergeAssembleagesTaxon.R")
  taxon <- read.csv("inst/tax_cleaned.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
  # Discard NA values
  taxon$measurement <- na.omit(taxon$measurement)
  # Round the measurement column to two decimal places
  taxon$measurement <- round(taxon$measurement, digits = 2)
  #assembleages <- read.csv("inst/comm_nodist_plants.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding="latin1")
  assembleages <- read.csv("inst/comm_nodist_plants.csv", sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")

    
  filtered_data <- mergeAssembleagesTaxon(taxon,assembleages)
  filtered_data <- filtered_data %>% filter(recovering_cond %in% c("restoration", "protection", "use"))
  filtered_data <- filtered_data %>% filter(disturbance1_age_clean != "none")
  #filtered_data <- filtered_data %>% filter(age < 101)
  # Convert the age column to a factor with labels corresponding to the new age groups
  filtered_data <- filtered_data %>% 
    mutate(age_group = cut(age, breaks = c(0,1,2,3,4,5, 10, 25, 50, 100, 500), include.lowest = TRUE))
  
  # Concatenate disturbance1_age_clean and recovering_cond into a new variable
  filtered_data$combined_group <- paste(filtered_data$disturbance1_age_clean, filtered_data$recovering_cond, sep = "_")
  filtered_data <- filtered_data %>% select(-taxon_clean,-measurement, -id_study, -id_comm, -study_common_taxon, -family, -class, -order)
  # Define the unique values for the combined_group variable
  unique_combined <- unique(filtered_data$combined_group)
  
  # Create dropdown menu options for the combined_group variable
  dropdown_combined <- list(
    title = "Combined Group",
    options = lapply(unique_combined, function(x) list(label = x, method = "update", args = list(list(visible = TRUE), list(title = paste("Measurement by Combined Group: ", x)), list(selector = TRUE, showlegend = TRUE)))),
    active = 0
  )
  
  # Summarize the data based on combined groups and age
  summary_data <- filtered_data %>%
    group_by(combined_group, age_group) %>%
    summarise(
      meanMeasurement = mean(richness, na.rm = TRUE)
    )
  
  # Get unique combined_group values
  unique_combined <- unique(summary_data$combined_group)
  
  # Generate a vector of symbols
  symbols <- c('circle', 'square', 'diamond', 'cross', 'x', 'triangle-up', 'triangle-down', 'triangle-left', 'triangle-right', 'pentagon', 'hexagon', 'hexagon2', 'octagon', 'star', 'hexagram', 'star-triangle-up', 'star-triangle-down', 'star-square', 'star-diamond', 'diamond-tall', 'diamond-wide')
  
  # Create a named vector with symbols assigned to each unique combined_group
  symbol_mapping <- setNames(symbols[1:length(unique_combined)], unique_combined)
  
  # Map symbols to combined_group in summary_data
  summary_data$symbol <- symbol_mapping[summary_data$combined_group]
  
  # Now, proceed with creating the plot
  data_plotly <- plot_ly(summary_data, x = ~age_group, y = ~meanMeasurement, color = ~combined_group,
                         type = 'scatter', mode = 'markers+lines',
                         marker = list(size = 10, symbol = ~symbol), 
                         text = ~paste('Age Group:', age_group, '<br>Mean Richness:', meanMeasurement, '<br>Disturbance-stage:', combined_group)) %>%
    layout(title = "Measurement by Age and Combined Group",
           xaxis = list(title = "Age Group"),
           yaxis = list(title = "Mean Richness"),
           showlegend = TRUE,
           updatemenus = list(dropdown_combined))
  
  # Show the interactive plot
  data_plotly
  