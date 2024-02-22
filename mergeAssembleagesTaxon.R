#' It create a file merging taxon and asembleages. It's not needed toe xecuted every time program loads. It's enough create file and record
 #' @param taxon Datastream with taxon
 #' @param assembleages Datastream with assembleages
 #' @return mergedAndFiltered Return datastream merged and filtered in the top deppending a value
 #' @export
 #' @examples
 #' mergeAssembleagesTaxon(taxon, assembleages)
 mergeAssembleagesTaxon <- function(taxon, assembleages){
   # Merge the dataframes based on common columns
   merged_data <- merge(taxon, assembleages, by = c("id_study", "id_comm"), all.x = TRUE)
   
   # Select the relevant columns
   result <- merged_data[, c("taxon_clean", "richness", "age", "id_study", "id_comm", "study_common_taxon", "family", "class", "order", "stage", "disturbance1_age_clean", "recovering_cond")]
   
   # Filter out rows where symbol_type is NULL or NA
   result <- na.omit(result)
   result_filtered <- result %>% filter(!is.na(age) & !is.na(richness) & !is.na(taxon_clean))
   
   # Convert measurement and age columns to numeric
   result_filtered$richness <- as.numeric(gsub(",", ".", result_filtered$richness))
   result_filtered$age <- as.numeric(result_filtered$age)
   
   top_percentage_to_remove <- 5  # Adjust this value as needed
   
   # Calculate the upper threshold based on the specified percentage
   upper_threshold <- quantile(result_filtered$richness, 1 - (top_percentage_to_remove / 100))
   
   # Apply the filter to remove extreme values from the top
   filtered_data <- result_filtered %>% filter(richness <= upper_threshold)
   
   write.csv2(filtered_data, "inst/filtered_data.csv", row.names = FALSE)

   return(filtered_data)
 }
 