#' Relate country with capital
#' @param selectGeneral data for main map
#' @return result Dataframe adding capital, latitud, longitud of a coutry to plot in main screen
#' @export
#' @examples
#' getCapital(selectGeneral)
getCapital <- function(selectGeneral){
  #source http://techslides.com/list-of-countries-and-capitals#google_vignette
  capitals <- read.csv("inst/country-capitals.csv", stringsAsFactors = FALSE, sep = ",", header = TRUE, fileEncoding="latin1")
  result <- selectGeneral %>% left_join(capitals, by = c("country" = "CountryName"))
  result$CapitalLatitude <- as.numeric(result$CapitalLatitude)
  result$CapitalLongitude <- as.numeric(result$CapitalLongitude)

  # Filter out rows with NA in the country column
  result <- result %>% filter(!is.na(country))
  result <- result %>% filter(!is.na(CapitalLatitude) & !is.na(CapitalLongitude))

  return(result)
}

