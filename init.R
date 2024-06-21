my_packages <- c("shiny", "leaflet", "shinyjs", "dplyr", "ggplot2", "leaflet.providers", 
                 "rnaturalearth", "rnaturalearthdata", "raster", "sf", "tidyverse", 
                 "terra", "rgbif", "DT", "reactable")

install_if_missing <- function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

