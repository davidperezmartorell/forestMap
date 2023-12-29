#' Relate ISO3 code with country
#' @param data data for main map
#' @return result Dataframe adding ISO3 code for that country
#' @export
#' @examples
#' getCapital(selectGeneral)
  getIso3 <- function(data){
    library("rworldmap")
    result <- data %>% rowwise() %>%  mutate(iso3 = rwmGetISO3(country))
    cat("Loading ISO3 codes\n")
  return(result)
}

