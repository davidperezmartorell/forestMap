#' Returns datastream to create a plot filtered
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of taxon
#' @param  study id_study
#' @return  median_values Contains datastream to create a plot filtered
#' @export
#' @examples
#' filterDataByIdstudy(taxon,assembleages,study)
filterDataByIdstudy<-function(taxon,assembleages,study){
  
 #taxon <- taxon[taxon$id_study == study & !is.na(taxon$measurement) & taxon$measurement >= 0.1, ]
 assembleages <- assembleages[assembleages$id_study %in% study, ]
 # Convert age column to numeric in the taxon data frame
 assembleages$age <- as.numeric(assembleages$age)
 
 
# Merge the dataframes based on common columns
#merged_data <- merge(taxon, assembleages, by = c("id_study", "id_comm"), all.x = TRUE)
merged_data <- merge(taxon, assembleages, by = c("id_study"), all.x = TRUE)


# Select the relevant columns
 result <- merged_data[, c("taxon_clean", "measurement", "age", "id_study","stage", "id_comm.x", "study_common_taxon", "family", "class","order", "richness", "predisturbances_clean", "disturbance1_age_clean", "disturbance2_clean")]

# Filter out rows where symbol_type is NULL or NA
 #result <- na.omit(result)
 result_filtered <- result %>% filter(!is.na(age) & !is.na(measurement) & !is.na(taxon_clean) )

 # Convert measurement and age columns to numeric
 result_filtered$measurement <- as.numeric(gsub(",", ".", result_filtered$measurement))
 result_filtered$age <- as.numeric(result_filtered$age)
 

return(result_filtered)

}


