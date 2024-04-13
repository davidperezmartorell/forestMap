#' Returns plot with richness along the time bassed in ages
#' @param speciesList All the data
#' @return plot_list THAT CONTAINS3 graphs
#' @export
#' @examples
#' getPieGraphs(data)
getPieGraphs <- function(speciesList) {


  # Calculate the counts for each category
  class_counts <- count(speciesList, Class) %>%
    arrange(desc(n))
  family_counts <- count(speciesList, Family) %>%
    arrange(desc(n))
  order_counts <- count(speciesList, Order) %>%
    arrange(desc(n))
  
  # Select the top 5 categories and sum the counts of the rest
  top_n_values <- 5
  class_top <- class_counts %>%
    slice_head(n = top_n_values) %>%
    mutate(Class = as.character(Class))
  class_others <- class_counts %>%
    slice_tail(n = -(top_n_values - 1)) %>%
    summarise(Class = "Others",
              n = sum(n))
  class_final <- bind_rows(class_top, class_others)
  
  family_top <- family_counts %>%
    slice_head(n = top_n_values) %>%
    mutate(Family = as.character(Family))
  family_others <- family_counts %>%
    slice_tail(n = -(top_n_values - 1)) %>%
    summarise(Family = "Others",
              n = sum(n))
  family_final <- bind_rows(family_top, family_others)
  
  order_top <- order_counts %>%
    slice_head(n = top_n_values) %>%
    mutate(Order = as.character(Order))
  order_others <- order_counts %>%
    slice_tail(n = -(top_n_values - 1)) %>%
    summarise(Order = "Others",
              n = sum(n))
  order_final <- bind_rows(order_top, order_others)
  
  # Pie chart for Class
  # Calculate percentage
  class_final <- class_final %>%
    mutate(percentage = n / sum(n) * 100)
  
  # Pie chart for Class
  pieSpecieClass <- ggplot(class_final, aes(x = "", fill = Class, y = n)) +
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes(label = paste0(round(percentage), "%")), 
              position = position_stack(vjust = 0.5)) +  # Add labels inside the pie chart
    coord_polar("y", start = 0) +
    labs(title = "Distribution of Class") +
    theme_void() +  # Remove axis values and numbers
    theme(
      text = element_text(size = 16),
      legend.text = element_text(size = 16)
    )
  
  # Calculate percentage
  family_final <- family_final %>%
    mutate(percentage = n / sum(n) * 100)
  
  # Pie chart for Family
  pieSpecieFamily <- ggplot(family_final, aes(x = "", fill = Family, y = n)) +
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes(label = paste0(round(percentage), "%")), 
              position = position_stack(vjust = 0.5)) +  # Add labels inside the pie chart
    coord_polar("y", start = 0) +
    labs(title = "Distribution of Family") +
    theme_void() +  # Remove axis values and numbers
    theme(
      text = element_text(size = 16),
      legend.text = element_text(size = 16)
    )
  
  
  # Pie chart for Order
  # Calculate percentage
  order_final <- order_final %>%
    mutate(percentage = n / sum(n) * 100)
  
  # Pie chart for Order
  pieSpecieOrder <- ggplot(order_final, aes(x = "", fill = Order, y = n)) +
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes(label = paste0(round(percentage), "%")), 
              position = position_stack(vjust = 0.5)) +  # Add labels inside the pie chart
    coord_polar("y", start = 0) +
    labs(title = "Distribution of Order") +
    theme_void() +  # Remove axis values and numbers
    theme(
      text = element_text(size = 16),
      legend.text = element_text(size = 16)
    )
  
  
  # Create a list to store the plots
  plot_list <- list(pieSpecieClass = pieSpecieClass,
                    pieSpecieOrder = pieSpecieOrder,
                    pieSpecieFamily = pieSpecieFamily
                    )
  
  return(plot_list)
}

