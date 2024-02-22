#' Returns table related to the id_comm clicked in the map
#' @param study Study that we want specie list
#' @param  taxon inventory of taxon
#' @param  assembleages inventory of asssembleages
#' @return result Return table with info
#' @export
#' @examples
#' givemeGbifInfo(specie)
  givemeGbifInfo <- function(taxon,assembleages, study) {

    cat("givemeGbifInfo.R: Discover info from species in GBIF\n")
    
    # Filter to obtain the species list from the clicked study
    unique_id_study <- unique(study)
    filtered_taxon <- taxon %>% filter(id_study == unique_id_study) %>% dplyr::select(taxon_clean, kingdom, phylum, class, family, genus, IsGymnosperm)
    filtered_taxon <- filtered_taxon %>% distinct()
    
    # Return the result data frame
    return(filtered_taxon)
  }
