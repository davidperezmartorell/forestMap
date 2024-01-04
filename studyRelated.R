#' Returns table related to the id_comm clicked in the map
#' @param taxon Identifier of individual tear clicked in map
#' @param assemblages Identifier of individual tear clicked in map
#' @param id_comm Identifier of individual tear clicked in map
#' @return tableHTML Data avilable
#' @export
#' @examples
#' studyRelated(id_comm)
  studyRelated <- function(taxon,assemblages, identifier) {

    #Search id_study relater
    idStudyUnique <- givemeIdStudy(taxon,identifier)
    taxon <- filter(taxon, id_study == idStudyUnique)
    
    # Assuming 'assemblages' is an sf object
    assemblages <- st_drop_geometry(assemblages)
    
    # Select interesting values from assemblages to print in a table
    assemblages <- assemblages %>% 
      filter(id_study == idStudyUnique) %>%
      #dplyr::select(id_study, id_comm, age, stage, study_common_taxon, study_year, taxon_level, disturbance1_age_clean, disturbance2_clean, study_common_taxon_clean,
      #              predisturbances_clean, current_impact_clean, postdisturbance, restoration, protection, use, recovering_cond, coord_accuracy)
      dplyr::select(id_comm, age, stage, study_common_taxon, study_year, taxon_level, disturbance1_age_clean, disturbance2_clean, study_common_taxon_clean,
                    predisturbances_clean, current_impact_clean, postdisturbance, restoration, protection, use, recovering_cond, coord_accuracy)
    # Convert assemblages to an interactive datatable
    datatable_obj <- datatable(assemblages, options = list(dom = 't', pageLength = 10, scrollX = TRUE, columnDefs = list(
      list(className = 'dt-right', targets = c(3, 6, 14, 15)),
      list(orderable = FALSE, targets = 2), #1:2),
      list(name = ' ', targets = 0),
      #list(name = 'id_study', targets = 1),
      list(name = 'id_comm', targets = 2),
      list(name = 'age', targets = 3),
      list(name = 'stage', targets = 4),
      list(name = 'study_common_taxon', targets = 5),
      list(name = 'study_year', targets = 6),
      list(name = 'taxon_level', targets = 7),
      list(name = 'disturbance1_age_clean', targets = 8),
      list(name = 'disturbance2_clean', targets = 9),
      list(name = 'study_common_taxon_clean', targets = 10),
      list(name = 'predisturbances_clean', targets = 11),
      list(name = 'current_impact_clean', targets = 12),
      list(name = 'postdisturbance', targets = 13),
      list(name = 'restoration', targets = 14),
      list(name = 'protection', targets = 15),
      list(name = 'use', targets = 16),
      list(name = 'recovering_cond', targets = 17),
      list(name = 'coord_accuracy', targets = 18)
    )))
    
    # Extract the data frame from the datatable object
    df <- datatable_obj$x$data
    # Return the data frame
    return(df)

}
