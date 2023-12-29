#' Discover countries according latitude and longitude
#' @param assembleages Input data with values
#' @return assembleages assembleages with countrie filled
#' @export
#' @examples
#' fillCountryAccordingLatLon()
fillCountryAccordingLatLon <- function(assembleages){

  com <- assembleages %>% 
    group_by(id_study) %>% 
    mutate(lon = mean(exact_long, na.rm = TRUE),
           lat = mean(exact_lat, na.rm = TRUE)) %>% 
    filter(!is.na(lon))
  
  
  #Country names
  library(rworldmap)
  
  countriesSP <- st_as_sf(getMap(resolution='low'))
  comesp2 <- st_as_sf(com, 
                      coords = c('lon', 'lat'),
                      crs = 4326, #WGS84
                      remove = F)
  sf_use_s2(FALSE)
  
  cat("fillCountryAccordingLatLon.R: Unifying CRS coordinates with WSG84 values\n")
  # Set CRS to WGS84 for comesp2
   comesp2 <- st_set_crs(comesp2, st_crs("+proj=longlat +datum=WGS84"))
  # Set CRS to WGS84 for countriesSP
   countriesSP <- st_set_crs(countriesSP, st_crs("+proj=longlat +datum=WGS84"))

  cat("fillCountryAccordingLatLon.R: Joining data from assembleages and general info from countries\n")
  country <- st_join(comesp2, countriesSP, join = st_within)
  
  #Mutate column name with country
  country <- country %>% mutate(Country = SOVEREIGNT) %>% dplyr::select(-SOVEREIGNT)

  #If you want to see countries ==NA, you can list them here
  #Filter country in my data is incorrect
   country_errors <- country %>% dplyr::select(country) %>% dplyr::filter(is.na(.$country))
   cat ("Has been detected", nrow(country_errors), " registers with NA value in country column.")
   countries_OK <- country %>% dplyr::select(Country) %>% dplyr::filter(is.na(.$Country))
   cat ("Has been detected", nrow(countries_OK), " registers with NA value in new country column.")

  rm(com, comesp2, countriesSP,country_errors, countries_OK) #Remove variables that will not be used anymore
  return(country)
}
  
