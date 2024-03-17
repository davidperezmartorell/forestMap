#' Returns abundance from assembleages ad in_comm for each taxon_clean
#' @param  taxon inventory of taxon
#' @param  input_id_comm ID Community that we are searching for
#' @param  input_taxon_clean inventory of asssembleages
#' @return result Return table with info
#' @export
#' @examples
#' givemeAbundance(specie)
givemeAbundance <- function(taxon, input_id_comm, input_taxon_clean) {
  
  abundance_value <- taxon %>%
    filter(id_study == input_id_comm & taxon_clean == input_taxon_clean) %>%
    dplyr::select(measurement) %>%
    pull()  # Extract the numeric value from the abund column

  return(abundance_value)
}
