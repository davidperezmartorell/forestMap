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
  
  # Rename the taxon_clean column to Taxon
   pivot_data <- pivot_data %>% rename(Taxon = taxon_clean)
  
  
  # Modify the column names to add "Site:" prefix
   colnames(pivot_data)[-1] <- paste("Community:", colnames(pivot_data)[-1], sep = " ")

  # Wrap the taxon names in <i> tags
   pivot_data$Taxon <- paste("<i>", pivot_data$Taxon, "</i>", sep = "")
   
  # Convert the dataframe to HTML table format
   table_html <- datatable(pivot_data, escape = FALSE, options = list(dom = 't', paging = FALSE))
  
  
   
  # Create the HTML tittle
      tittle_html <- ""
      #Select values interesting in this inventory because can mesure presence/absence, abundance or maybe others. I want to explain in the table
          assembleages_subset <- assembleages %>%
            select(study_common_taxon, organism_threshold, metric, metric_source) %>%
            slice(1) %>%  # Select only the first row assuming it's constant for all rows
            rename(
              "Study Common Taxon" = study_common_taxon,
              "Organism Threshold" = organism_threshold,
              "Metric" = metric,
              "Metric Source" = metric_source
            )

          # Format each variable and its description
          for (field in colnames(assembleages_subset)) {
            tittle_html <- paste(
              tittle_html,
              "<h4><strong>", field, " : </strong></h4>", assembleages_subset[1, field],
              sep = " "
            )
          }
          # Add a line break after the title
          tittle_html <- paste(tittle_html, "<br>", sep = " ")
  
  list(data = pivot_data, tittle = tittle_html)
}
