#' Returns table related to the id_comm clicked in the map
#' @param assemblages Identifier of individual tear clicked in map
#' @param id_comm Identifier of individual tear clicked in map
#' @return tableHTML Data avilable
#' @export
#' @examples
#' studyClicked(id_comm)
studyClicked <- function(assembleages, idStudyUnique) {

  # Load citation data
  citation_data <- read.csv("inst/citation_data.csv", stringsAsFactors = FALSE, sep = ";", header = TRUE, fileEncoding = "latin1", dec = ",")
  
  # Assuming 'assemblages' is an sf object
  assemblages_ori <- st_drop_geometry(assembleages)
  
  # Merge citation data with assemblages based on the citation column
  assemblages_merged <- merge(assemblages_ori, citation_data, by = "citation", all.x = TRUE)

  #To see how many studies have not DOI assigned
   withoutDOI <- assemblages_merged %>% filter (is.na(DOI)) %>% select (id_study, DOI)
   cat("studyClicked.R: There are ", nrow(withoutDOI)," studies without DOI\n")
  
  # Filter rows for the given study ID
  assemblagesCommon <- assemblages_merged %>% 
    filter(id_study == idStudyUnique) %>% 
    slice(1) %>%  # Take only the first row
    rename(
      Database = database.x,
      Citation = citation,
      DOI = DOI,
      "Study ID" = id_study,
      "Number of communities" = n_comm_available,
      "Coord accuracy" = coord_accuracy,
      "Study year" = study_year,
      "Common taxon" = study_common_taxon_clean,
      "Organism threshold" = organism_threshold,
      Metric = metric,
      "Metric source" = metric_source,
      "Sampling method" = sampling_method
    )
  
  # Create a hyperlink for the Citation field
  assemblagesCommon <- assemblagesCommon %>% select("Database", "Citation","DOI", "Study ID", "Number of communities", "Coord accuracy", "Study year", "Common taxon",
        "Metric","Metric source","Sampling method")
                 

  # Dataframe with values repeated in all the id_comm from that study
  assemblagesUnique <- assemblages_merged %>% 
    filter(id_study == idStudyUnique) %>% 
    select(site, lat, lon, age, age_ori, stage, disturbance1_age_clean, predisturbances, postdisturbance, current_impact, recovering_cond, taxon_level, error_class, sampling_effort_within, notes)
  
  # Modifying Citation column to include HTML markup and opened in new navigator window
  assemblagesCommon <- assemblagesCommon %>% mutate(DOI = paste0('<html><a href="', DOI, '" target="_blank">', DOI, '</a></html>'))

  
  # Rename columns
  colnames(assemblagesUnique) <- c("Site", "Latitude", "Longitude", "Age", "Age Original", "Stage", "Disturbance age", "Predisturbances", "Postdisturbance", "Current impact", "Recovering condition", "Taxon level", "Error class", "Sampling effort", "Notes")
  
  return(list(assemblagesCommon = assemblagesCommon, assemblagesUnique = assemblagesUnique))
  
}

