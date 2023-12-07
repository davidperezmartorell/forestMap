#' Download shape file for all countries
#' @param NULL Input values
#' @return NULL No values output
#' @export
#' @examples
#' downloadAllShapes()
# Funtion downloadAllShapes ------------------------------------------------------
# Download info from https://github.com/rspatial/geodata/issues/
# It's available in geodata library
downloadAllShapes <- function() {
  library(sf)  #Library to store shape
  mapa_BEL <- geodata::gadm(country = "BEL", level = 1, path = "inst") %>% st_as_sf()
  mapa_MYS <- geodata::gadm(country = "MYS", level = 1, path = "inst") %>% st_as_sf()
  mapa_IDN <- geodata::gadm(country = "IDN", level = 1, path = "inst") %>% st_as_sf()
  mapa_BRA <- geodata::gadm(country = "BRA", level = 1, path = "inst") %>% st_as_sf()
  mapa_PRT <- geodata::gadm(country = "PRT", level = 1, path = "inst") %>% st_as_sf()
  mapa_SWE <- geodata::gadm(country = "SWE", level = 1, path = "inst") %>% st_as_sf()
  mapa_SLB <- geodata::gadm(country = "SLB", level = 1, path = "inst") %>% st_as_sf()
  mapa_CRI <- geodata::gadm(country = "CRI", level = 1, path = "inst") %>% st_as_sf()
  mapa_COL <- geodata::gadm(country = "COL", level = 1, path = "inst") %>% st_as_sf()
  mapa_LAO <- geodata::gadm(country = "LAO", level = 1, path = "inst") %>% st_as_sf()
  mapa_ECU <- geodata::gadm(country = "ECU", level = 1, path = "inst") %>% st_as_sf()
  mapa_CHL <- geodata::gadm(country = "CHL", level = 1, path = "inst") %>% st_as_sf()
  mapa_AUS <- geodata::gadm(country = "AUS", level = 1, path = "inst") %>% st_as_sf()
  mapa_UGA <- geodata::gadm(country = "UGA", level = 1, path = "inst") %>% st_as_sf()
  mapa_CHN <- geodata::gadm(country = "CHN", level = 1, path = "inst") %>% st_as_sf()
  mapa_THA <- geodata::gadm(country = "THA", level = 1, path = "inst") %>% st_as_sf()
  mapa_ARG <- geodata::gadm(country = "ARG", level = 1, path = "inst") %>% st_as_sf()
  mapa_JPN <- geodata::gadm(country = "JPN", level = 1, path = "inst") %>% st_as_sf()
  mapa_BOL <- geodata::gadm(country = "BOL", level = 1, path = "inst") %>% st_as_sf()
  mapa_PRI <- geodata::gadm(country = "PRI", level = 1, path = "inst") %>% st_as_sf()
  mapa_MEX <- geodata::gadm(country = "MEX", level = 1, path = "inst") %>% st_as_sf()
  mapa_USA <- geodata::gadm(country = "USA", level = 1, path = "inst") %>% st_as_sf()
  mapa_CAN <- geodata::gadm(country = "CAN", level = 1, path = "inst") %>% st_as_sf()
  mapa_MMR <- geodata::gadm(country = "MMR", level = 1, path = "inst") %>% st_as_sf()
  mapa_PAN <- geodata::gadm(country = "PAN", level = 1, path = "inst") %>% st_as_sf()
  mapa_NZL <- geodata::gadm(country = "NZL", level = 1, path = "inst") %>% st_as_sf()
  mapa_DEU <- geodata::gadm(country = "DEU", level = 1, path = "inst") %>% st_as_sf()
  mapa_GHA <- geodata::gadm(country = "GHA", level = 1, path = "inst") %>% st_as_sf()
  mapa_USA <- geodata::gadm(country = "USA", level = 1, path = "inst") %>% st_as_sf()
  mapa_CAN <- geodata::gadm(country = "CAN", level = 1, path = "inst") %>% st_as_sf()
  mapa_MMR <- geodata::gadm(country = "MMR", level = 1, path = "inst") %>% st_as_sf()
  mapa_PAN <- geodata::gadm(country = "PAN", level = 1, path = "inst") %>% st_as_sf()
  mapa_NZL <- geodata::gadm(country = "NZL", level = 1, path = "inst") %>% st_as_sf()
  mapa_DEU <- geodata::gadm(country = "DEU", level = 1, path = "inst") %>% st_as_sf()
  mapa_GHA <- geodata::gadm(country = "GHA", level = 1, path = "inst") %>% st_as_sf()
  cat("Downloading shapefiles for all maps\n")
}
