#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples
#' versions()

versions <- function(){
# Pie de pagina -----------------------------------------------------------
fluidRow(
  column(12, HTML("<h6> FORESTMAP Forest Inventory Map based on taxon and assemblages data provided by Dr. Veronica Cruz</h6>")),
  column(12, HTML("<h6> Author: David Perez Martorell davidperezmartorell@gmail.com</h6>")),
  column(12, HTML("<h6> Master's Thesis for Ecosystem Restoration</h6>")),
  column(12, HTML("<h6> Version 1.0.0 01/12/2023 In testing phase. Creation of environment, form, and base map</h6>")),
  column(12, HTML("<h6> Version 1.0.1 10/12/2023 Added more country data, capital, ISO3, latitude, and longitude</h6>")),
  column(12, HTML("<h6> Version 1.0.2 12/12/2023 Loading of elevation map</h6>")),
  column(12, HTML("<h6> Version 1.0.4 14/12/2023 Loading of country borders map</h6>")),
  column(12, HTML("<h6> Version 1.0.5 16/12/2023 Loading of rivers map</h6>")),
  column(12, HTML("<h6> Version 1.0.6 16/12/2023 Loading of rivers map</h6>")),
  column(12, HTML("<h6> Version 1.1.0 30/12/2023 Program conversion from package to application and skipping layers</h6>")),
  column(12, HTML("<h6> Version 1.1.1 07/01/2024 Publication of data from selected idcomm and id_study with common and individual data</h6>")),
  column(12, HTML("<h6> Version 1.1.2 08/01/2024 Added graphs showing abundance and richness relationship over years with disturbances</h6>")),
  column(12, HTML("<h6> Version 1.1.3 12/01/2024 Improved graphs, map width, added tab for graphs</h6>")),
  column(12, HTML("<h6> Version 1.1.4 13/01/2024 Creation of table with disturbance data and other interesting data</h6>")),
  column(12, HTML("<h6> Version 1.1.5 14/01/2024 Improvement of table header names and indexing by id_study instead of id_comm</h6>")),
  column(12, HTML("<h6> Version 1.1.6 15/01/2024 Maps work based on id_study instead of being granular to id_comm</h6>")),
  column(12, HTML("<h6> Version 1.1.7 18/01/2024 Added species associated with the study</h6>")),
  column(12, HTML("<h6> Version 1.1.8 22/01/2024 List of species converted into matrix according to communities of each study and other minor changes</h6>")),
  column(12, HTML("<h6> Version 1.2.0 15/02/2024 Changes in combined data, creation of new graphs, new tabs, filtering</h6>")),
  column(12, HTML("<h6> Version 1.2.1 22/02/2024 Added 2 layers in main map to view by stage or disturbance</h6>")),
  column(12, HTML("<h6> Version 1.2.2 08/03/2024 Updated data input</h6>")),
  column(12, HTML("<h6> Version 1.2.3 17/03/2024 Added total and year-range divided general graphs</h6>")),
  column(12, HTML("<h6> Version 1.3.0 17/03/2024 Stable Version</h6>")),
  column(12, HTML("<h6> Version 1.3.1 25/03/2024 Vew graphics based on presence/absence instead median in study values.</h6>")),
  
  
)#Fin pie de pagina fluidRow
}

