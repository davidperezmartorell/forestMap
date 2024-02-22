#' Returns plot with richness along the time bassed in ages
#' @param data All the data
#' @param identifier Identifier of individual study
#' @return var_importance_for_richness Tree decision about richness
#' @export
#' @examples
#' getRichnessPlot(data)
getRichnessPlot <- function(data, identifier) {
  
  #Select id_study where id (study clicked) pertains
  idStudyCommon <- data %>% filter(str_detect(id, fixed(identifier))) %>% dplyr::select(id_study) %>%
    distinct() %>% pull(id_study)
  
  data <- data %>% filter(id_study == idStudyCommon)
  # Convert 'age_ori' to numeric
  data$age <- as.numeric(data$age)
  data$predisturbances_clean <- ifelse(is.na(data$predisturbances_clean), "", data$predisturbances_clean) # Replace NA values in data$richness with 1
  data$disturbance1_age_clean <- ifelse(is.na(data$disturbance1_age_clean), "", data$disturbance1_age_clean) # Replace NA values in data$richness with 1
  data$disturbance2_clean <- ifelse(is.na(data$disturbance2_clean), "", data$disturbance2_clean) # Replace NA values in data$richness with 1
  
  # Calculate breaks for both axes
  data$richness <- ifelse(is.na(data$richness), 1, data$richness) # Replace NA values in data$richness with 1
  richness_breaks <- seq(0, max(data$richness), length.out = 5)
  
  # Calculate the number of unique values in the stage column
  num_colors <- n_distinct(data$stage)
  
  # Create a color palette with the desired number of colors
  color_palette <- viridisLite::viridis(num_colors)
  
  # Use the calculated color palette in the plot
  richness_plot <- ggplot(data, aes(x = age, y = richness)) +
    geom_point(aes(color = stage), size = 5) +
    geom_text(aes(x = age, y = max(richness) + 5, label = disturbance1_age_clean),
              vjust = -3, size = 5, hjust = 1, angle = 90,
              nudge_x = 0.5, nudge_y = 0.5) +
    geom_segment(aes(x = age, xend = age, y = 0, yend = max(richness)), 
                 linetype = "dashed", color = "darkgray", alpha = 0.5) +
    labs(title = "Number of taxons vs. Age",
         x = "Age",
         y = "Number of taxons") +
    scale_color_manual(values = color_palette, name = "Stage") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 1, color = "black"),
      text = element_text(size = 12),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14)
    )
  
  
  return(richness_plot)
  
}