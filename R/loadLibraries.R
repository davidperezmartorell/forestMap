#' Load necessary libraries
#' @param NULL No values output
#' @return NULL No values output
#' @export
#' @examples
#' loadLibraries()
loadLibraries<-function(){
library("readr")
library("dplyr")
library("ggplot2")
library("leaflet")
library("shiny")

  # Define .onLoad function to load libraries
  .onLoad <- function(libname, pkgname) {
    library(dplyr, character.only = TRUE, quietly = TRUE)
    library(readr, character.only = TRUE, quietly = TRUE)
    library(ggplot2, character.only = TRUE, quietly = TRUE)
    library(leaflet, character.only = TRUE, quietly = TRUE)
    library(shiny, character.only = TRUE, quietly = TRUE)
    # Add other libraries as needed
  }
}