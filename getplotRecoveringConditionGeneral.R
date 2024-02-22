#' Returns Plot with relation recovering condition versus age for all species
#' @param  data dataframe mixed taxon and assembleages
#' @return  plot Plot with relation Family, age for all species
#' @export
#' @examples
#' getplotRecoveringConditionGeneral(taxon,assembleages,study)
getplotRecoveringConditionGeneral <- function(data) {

  filtered_data<-data %>% select(age, richness,disturbance1_age_clean, recovering_cond )
  # Convert the age column to a factor with labels corresponding to the new age groups
  filtered_data2 <- filtered_data %>% 
    mutate(age_group = cut(age, breaks = c(0,5,10, 15,20,50,500), include.lowest = TRUE))
  
  # Define the three recovering condition groups
  filtered_data3 <- filtered_data2 %>%
    mutate(recovering_cond_group = case_when(
      recovering_cond %in% c("protection") ~ "Protection",
      recovering_cond %in% c("restoration", "restoration/protection", "restoration/use") ~ "Restoration",
      TRUE ~ "Others"
    ))
  
  # Summarize the data by age group and recovering condition group
  summary_data <- filtered_data3 %>%
    group_by(age_group, recovering_cond_group) %>%
    summarise(mean_measurement = mean(richness, na.rm = TRUE))
  
  
  plot<-ggplot(summary_data, aes(x = age_group, y = mean_measurement, fill = recovering_cond_group)) +
    geom_col(position = "dodge") +
    labs(title = "Mean abundance respect recovering contition over the Years",
         x = "Age",
         y = "Measurement",
         fill = "Recovering") +
    theme_minimal()


  # ggplot(summary_data, aes(x = age_group, y = log(mean_measurement + 1), fill = recovering_cond_group)) +
  #   geom_col(position = "dodge") +
  #   labs(title = "Log Measurement Changes Over the Years",
  #        x = "Age Group",
  #        y = "Log Mean Measurement",
  #        fill = "Recovering Condition Group") +
  #   theme_minimal()
return(plot)
} 





