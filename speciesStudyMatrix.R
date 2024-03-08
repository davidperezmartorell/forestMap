#' Returns all species related to clicked study
#' @param assembleages dataframe with info
#' @param taxon dataframe with info
#' @param id_comm Identifier of individual tear clicked in map
#' @return tableHTML Data avilable
#' @export
#' @examples
#' speciesStudyMatrix(id_comm)
speciesStudyMatrix <- function(assembleages,taxon, idStudyUnique) {
  library("DT")
  library("dplyr")
  library("tidyr")

  cat("speciesStudyMatrix.D: Creating matrix species related\n")

  # Select interesting values from assemblages
  taxon2 <- taxon %>% filter(id_study == idStudyUnique)
  # Select necessary columns. Here we can add more info about species
  taxon3 <- taxon2 %>% dplyr::select(taxon_clean, id_comm, measurement)

  # Merge taxon and assemblages by id_comm
  merged_data <- left_join(taxon3, assembleages, by = "id_comm")
  merged_data2 <- merged_data %>% dplyr::select(taxon_clean, measurement, site)
  
  # Sum measurements for each combination of taxon_clean and site
  summed_data <- merged_data2 %>%
    group_by(taxon_clean, site) %>%
    summarise(total_measurement = sum(measurement)) %>%
    ungroup()
  
  # Pivot the data to create the desired dataframe
  pivot_data <- summed_data %>%
    pivot_wider(names_from = site, values_from = total_measurement, values_fill = 0)
  
  # Reorder columns to have taxon_clean as the first column
  pivot_data <- pivot_data %>%
    select(taxon_clean, everything())

  
  
  # Create the HTML tittle
      tittle_html <- paste("<h3>FORESTMAP Matrix inventory specie-community site and measurement</h3>")
      #Select values interesting in this inventory because can mesure presence/absence, abundance or maybe others. I want to explain in the table
          assembleages_subset <- assembleages %>%
            select(study_common_taxon, organism_threshold, metric, metric_source) %>%
            slice(1) %>%  # Select only the first row assuming it's constant for all rows
            rename(
              Study_Common_Taxon = study_common_taxon,
              Organism_Threshold = organism_threshold,
              Metric = metric,
              Metric_Source = metric_source
            )
          
          # Format each variable and its description
          for (field in colnames(assembleages_subset)) {
            tittle_html <- paste(
              tittle_html,
              "<strong>", field, ":</strong> ", assembleages_subset[1, field],
              sep = " "
            )
          }
          # Add a line break after the title
          tittle_html <- paste(tittle_html, "<br>", sep = " ")
  
  list(data = pivot_data, tittle = tittle_html)
}
