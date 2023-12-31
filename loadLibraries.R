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
library("randomForest")
#install.packages("DT", repos = "http://cran.us.r-project.org")
library("DT") #To create tables in result format under the plot


cat("loadLibraries.R: Libraries has been loaded\n")
}
