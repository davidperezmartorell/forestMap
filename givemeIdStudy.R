#' Returns id_study related to id_comm
#' @param taxon Input list of values from dataframe
#' @param identifier Value id_comm to search
#' @return idStudyUnique Id_study related to idcomm
#' @export
#' @examples
#' givemeIdStudy(id_comm)
givemeIdStudy <- function(taxon,identifier) {
 
  #Search id_study relater
  idStudyUnique <- filter(taxon, id_comm == identifier) %>% pull(id_study) %>% unique()
  return(idStudyUnique)
  
  }
