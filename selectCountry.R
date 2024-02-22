#' Select country data
#' @param selectGeneral #Datastream with indexed values
#' @param input_country #Country selected
#' @return data for this country selected
#' @export
#' @examples
#' selectCountry(selectGeneral,country)
# Funtion selectCountry ------------------------------------------------------
selectCountry <- function(selectGeneral,input_country) {
  filtered_data <- selectGeneral %>% filter(grepl(country, input_country, ignore.case = TRUE))
  return(filtered_data)
}




