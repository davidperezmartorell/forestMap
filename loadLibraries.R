#' Load necessary libraries
#' @param NULL No values output
#' @return NULL No values output
#' @export
#' @examples
#' loadLibraries()
loadLibraries<-function(){
cat("loadLibraries.R: Loading loaded\n")
library("readr")
library("dplyr")  #Work with dataframes
library("ggplot2") #Plot maps
library("leaflet") #Plot maps
library("leaflet.providers") #Plot maps extras layers https://rstudio.github.io/leaflet/basemaps.html
library("shiny") #To shiny menus and frames
library("shinyjs") #Special options from shiny
library("rnaturalearth") #Rivers data
library("rnaturalearthdata") #Rivers data
library("raster")
library("sf")  #To add buttons in menu
library("tidyverse")
library("terra")
library("rgbif") #Discover from GBIF database information from each specie
library("DT") #To create tables in result format under the plot
library("reactable") #To create exciting tables
library("plotly") #To plot graphs for inventory
library("RColorBrewer") #To play with colours in leaflet map

  packages <- c("dplyr", "readr", "ggplot2", "leaflet", "leaflet.providers", 
                "shiny", "shinyjs", "rnaturalearth", "rnaturalearthdata", 
                "raster", "sf", "tidyverse", "terra", "rgbif", "DT", "reactable", "plotly" , "RColorBrewer")
  
  # # Check and install packages if not already installed
  # for (package in packages) {
  #   if (!requireNamespace(package, quietly = TRUE)) {
  #     install.packages(package)
  #   }
  # }
  
  # # Load all the libraries
  # lapply(packages, library, character.only = TRUE)



cat("loadLibraries.R: Libraries has been loaded\n")
}
