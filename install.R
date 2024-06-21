# List of packages to install
packages <- c(
  "readr", 
  "pryr", 
  "dplyr", 
  "ggplot2", 
  "leaflet", 
  "leaflet.providers", 
  "shiny", 
  "shinyjs", 
  "rnaturalearth", 
  "rnaturalearthdata", 
  "raster", 
  "sf", 
  "tidyverse", 
  "terra", 
  "rgbif", 
  "DT", 
  "reactable", 
  "plotly", 
  "RColorBrewer", 
  "pdfetch", 
  "rmarkdown", 
  "htmlTable", 
  "webshot", 
  "pagedown"
)

# Check if each package is already installed; if not, install it
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}
