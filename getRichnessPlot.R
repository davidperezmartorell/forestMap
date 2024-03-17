#' Returns plot with richness along the time bassed in ages
#' @param data All the data
#' @param identifier Identifier of individual study
#' @return var_importance_for_richness Tree decision about richness
#' @export
#' @examples
#' getRichnessPlot(data)
getRichnessPlot <- function(data) {
  
  data <- data %>%
    group_by(age, stage) %>%
    summarise(mean_richness = mean(richness, na.rm = TRUE))
  
  data$age <- as.numeric(data$age)
  
  # Determine unique stages in the data
  stages <- unique(data$stage)
  
  # Define colors based on stages
  color_palette <- case_when(
    stages == "recovering" ~ "green",
    stages == "reference" ~ "blue",
    stages == "disturbed" ~ "darkred",
    stages == "protection" ~ "yellow"
  )
  
  # Use the calculated color palette in the plot
  richness_plot <- ggplot(data, aes(x = age, y = mean_richness, color = stage)) +
    geom_line() +
    geom_point(size = 5) +
    labs(title = "Taxon during disturbances and Age", x = "Age", y = "Mean Richness") +
    theme_minimal() +
    scale_color_manual(values = color_palette, name = "Stage")+
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      #axis.line = element_line(linewidth = 1, color = "black"),
      text = element_text(size = 16),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16)
    )

   richness_plot + coord_cartesian(expand = FALSE)  
  return(richness_plot)
}

