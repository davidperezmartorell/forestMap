#' Returns table related to the id_comm clicked in the map
#' @param assemblages Identifier of individual tear clicked in map
#' @param id_comm Identifier of individual tear clicked in map
#' @return tableHTML Data avilable
#' @export
#' @examples
#' studyClicked(id_comm)
studyClicked <- function(assemblages, idStudyUnique) {

  # Assuming 'assemblages' is an sf object
  assemblages_ori <- st_drop_geometry(assemblages)
 
  # Select interesting valsues from assemblages to print in a table
  assemblagesCommon <- assemblages_ori %>% dplyr::select( database,citation, id_study, n_comm_available, coord_accuracy,study_year, study_common_taxon_clean, organism_threshold, metric,metric_source, sampling_method)

#  assemblagesCommon <- filtered_assemblages(assemblagesCommon) %>% filter(id_study == idStudyUnique) %>% distinct()
  assemblagesCommon <- assemblagesCommon %>% filter(id_study == idStudyUnique) %>% head(1)
  
  assemblagesCommon <- assemblagesCommon %>% dplyr::select(database,citation, id_study, n_comm_available, coord_accuracy, study_year, study_common_taxon_clean, organism_threshold, metric, metric_source, sampling_method) %>%   
    dplyr::rename( Database = database, Citation = citation, "Study ID" = id_study, "Number of communities" = n_comm_available, "Coord accuracy" = coord_accuracy, "Study year" = study_year, "Common taxon" = study_common_taxon_clean, "Organism threshold" = organism_threshold, Metric = metric, "Metric source" = metric_source, "Sampling method" = sampling_method)
  assemblagesCommon <- assemblagesCommon %>% head(1)

  
  # Dataframe with values repeated in all the id_comm from that study
  assemblagesUnique <- assemblages_ori %>% filter(id_study == idStudyUnique) %>% dplyr::select( site, ,lat, lon, age,age_ori,stage,disturbance1_age_clean, predisturbances, postdisturbance,current_impact,recovering_cond,taxon_level,error_class,sampling_effort_within, notes)

  # Replace NA values in the 'notes' column with "No data available"
  assemblagesUnique <- assemblagesUnique %>%
    mutate(
      notes = ifelse(is.na(notes), "No data available", notes),
      predisturbances = ifelse(is.na(predisturbances), "No data available", predisturbances),
      postdisturbance = ifelse(is.na(postdisturbance), "No data available", postdisturbance),
      current_impact = ifelse(is.na(current_impact), "No data available", current_impact)
    )
  
  
  #Rename columns name
    assemblagesUnique <- assemblagesUnique %>%
      dplyr::rename( Site = site,Latitude = lat,Longitude = lon,Age = age,"Age Original" = age_ori,Stage = stage,"Disturbance age" = disturbance1_age_clean,
        Predisturbances = predisturbances,Postdisturbance = postdisturbance,"Current impact" = current_impact,"Recovering condition" = recovering_cond,
        "Taxon level" = taxon_level,"Error class" = error_class, "Sampling effort" = sampling_effort_within, Notes = notes
      )
    
  return(list(assemblagesCommon = assemblagesCommon, assemblagesUnique = assemblagesUnique))
  
}
