#' Check if map is in local
#'
#' @param pais country selected
#' @param nivel level of detail. 1,2,3 or 4
#' @return MapaBase is the file with the MAP if this exists
#' @export
#' @examples
#' footer()

footer <- function(){
# Pie de pagina -----------------------------------------------------------
fluidRow(
  column(12, HTML("<h6> Mapa plote de inventarios forestales bsato en datos de taxon y assembleages proporcionado por Veornica Cruz</h6>")),
  column(12, HTML("<h6> Autor David Perez Martorell davidperezmartorell@gmail.com</h6<>")),
  column(12, HTML("<h6> Trabajo TFM para el Master Restauraci??n de Ecosistemaas</h6>")),
  column(12, HTML("<h6> Version 1.0.0 01/12/2023 En pruebas.Creaci??n de entorno, formulario y mapa base</h6>")),
  column(12, HTML("<h6> Version 1.0.1 10/12/2023 grego mas datos de pais, capital, ISO3 y latitud y longitud/h6>")),
  column(12, HTML("<h6> Version 1.0.2 12/12/2023 Carga de mapa de elevaciones/h6>")),
  column(12, HTML("<h6> Version 1.0.4 14/12/2023 Carga de mapa de bordes pa??s/h6>")),
  column(12, HTML("<h6> Version 1.0.5 16/12/2023 Carga de mapa de r??os/h6>")),
  column(12, HTML("<h6> Version 1.0.6 16/12/2023 Carga de mapa de r??os/h6>")),
  column(12, HTML("<h6> Version 1.1.0 30/12/2023 Reconversi??n de programa de paquete a aplicaci??n y omitir layers/h6>")),

  
  
)#Fin pie de pagina fluidRow
}

