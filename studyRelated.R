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
  assemblages_ori <- st_drop_geometry(assemblages)
  
  # Select interesting values from assemblages to print in a table
  assemblages <- assemblages_ori %>% 
    filter(id_study == idStudyUnique) %>%
    dplyr::select(-id,-id_comm, -database, -id_study, -id_study_ori, -citation , -study, -site, -plot, -exact_lat, -exact_long, -historic_impact, -notes, -country, -lon, -lat, -ScaleRank, -LabelRank, -FeatureCla,
                  -SOV_A3, -ADM0_DIF, -ADM0_DIF, -LEVEL, -TYPE, -ADMIN, -ADM0_A3, -GEOU_DIF, -GEOUNIT, -GU_A3, -SU_DIF, -SUBUNIT, -SU_A3 , -NAME , -ABBREV , -POSTAL , -NAME_FORMA ,
                  -TERR_, -NAME_SORT, -MAP_COLOR, -POP_EST, -GDP_MD_EST, -FIPS_10_, -ISO_A2, -ISO_A3, -ISO_N3, -ISO3, -LON, -LAT, -ISO3.1, -ADMIN.1, -REGION, -continent, -GEO3major ,
                  -GEO3, -IMAGE24, -GLOCAF, -Stern, -SRESmajor, -SRES, -GBD, -AVOIDnumeric, -AVOIDname, -LDC, -SID, -LLDC, -Country)
  

  
      # Function to select common values in the study and all id_comm
        filtered_assemblages <- function(assemblages) {
          assemblages %>%
            select_if(~ length(unique(.)) == 1)
        }
        filtered_data <- filtered_assemblages(assemblages) %>% distinct()
      
      # Dataframe with values repeated in all the id_comm from that study
      columns_to_remove <- setdiff(names(assemblages), names(filtered_data))
      assemblagesRemoved <- assemblages %>% dplyr::select(-one_of(columns_to_remove)) %>% distinct()
      
      # Dataframe with values NOT repeated in all the id_comm from that study
      assemblages <- assemblages %>% dplyr::select(columns_to_remove)
      
      
  
  # Convert assemblages to an interactive datatable
  datatable_obj <- datatable(assemblages, options = list(dom = 't', pageLength = 10, scrollX = TRUE))

  
  # Extract the data frame from the datatable object
  df <- datatable_obj$x$data
  # Return the data frame df with contents and 
  return(list(df = df, assemblagesRemoved = assemblagesRemoved))
  
}