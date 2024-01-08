#' Returns plot with richness along the time bassed in ages
#' @param data All the data
#' @param identifier Identifier of individual study
#' @return var_importance_for_richness Tree decision about richness
#' @export
#' @examples
#' getPlotRichness(data)
getPlotRichness <- function(data, identifier) {

    data <- data %>% filter(id_study == identifier)
    # Convert 'age_ori' to numeric
    data$age <- as.numeric(data$age)
    # Calculate breaks for both axes
    abund_breaks <- seq(0, max(data$abund), length.out = 5)
    richness_breaks <- seq(0, max(data$richness), length.out = 5)
    
    # Create the plot using ggplot2
    library(ggplot2)
    
      # Convert 'age_ori' to numeric
      data$age <- as.numeric(data$age)
    
      # Create the initial plot with the first set of points
      richness_plot <- ggplot(data, aes(x = age)) +
        geom_point(aes(y = richness, color = "Richness"), size = 3) +
        geom_text(aes(x = age, y = max(richness) + 5, label = disturbance1_age), 
                  vjust = -2, size = 5, hjust = 1, angle = 90, 
                  nudge_x = 0.5, nudge_y = 0.5) +  # Adjust alignement of disturbance1_age
        geom_segment(aes(x = age, xend = age, y = 0, yend = max(richness)), 
                     linetype = "dashed", color = "gray", alpha = 0.5) +  # Add vertical dashed lines
        labs(title = "Richness vs. Age",
             x = "Age",
             y = "Richness") +
        scale_color_manual(values = c("Richness" = "red")) +
        scale_x_continuous(trans = "reverse") +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(linewidth = 1, color = "black"),
          text = element_text(size = 12)
        ) +
        guides(color = "none")
      
      # Create a new plot with the second set of points
      abundance_plot <- ggplot(data, aes(x = age)) +
        geom_point(aes(y = abund, color = "Abundance"), size = 3) +
        geom_text(aes(x = age, y = max(abund) + 5, label = disturbance1_age),
                  vjust = -2, size = 5, hjust = 1, angle = 90  , 
                  nudge_x = 0.5, nudge_y = 0.5) +  # Adjust alignement of disturbance1_age
        geom_segment(aes(x = age, xend = age, y = 0, yend = max(richness)), 
                     linetype = "dashed", color = "gray", alpha = 0.5) +  # Add vertical dashed lines
        labs(title = "Abundance vs. Age",
             x = "Age",
             y = "Abundance") +
        scale_color_manual(values = c("Abundance" = "blue")) +  # Color for new points
        scale_x_continuous(trans = "reverse") +  # Reverse the x-axis
        theme_minimal() +
        theme(
          panel.grid = element_blank(),  # Remove grid lines
          panel.border = element_blank(),  # Remove panel border
          axis.line = element_line(linewidth = 1, color = "black"),  # Add axis lines
          text = element_text(size = 12)  # Adjust font size
        ) +
        guides(color = "none")  # Hide the legend
      
      # Combine the two plots
      combined_plot <- cowplot::plot_grid(
        richness_plot + theme(legend.position = "none"),  # Hide the legend for the first plot
        abundance_plot,
        ncol = 2,
        align = "v"
      )
      
      # Add the secondary axis to the combined plot
      combined_plot <- combined_plot + scale_y_continuous(
        sec.axis = sec_axis(
          ~.,  # Identity transformation
          breaks = abund_breaks,
          name = "Abundance",
          labels = scales::comma_format(scale = 1e3)  # Format labels as thousand
        )
      )
      
      return(combined_plot)

}
