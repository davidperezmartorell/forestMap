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

# Install packages if not already installed
install.packages(packages, repos = "https://cloud.r-project.org")

