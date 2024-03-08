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
  column(12, HTML("<h6> FORESTMAP  Mapa de inventarios forestales basado en datos de tablas taxon y assembleages proporcionado por doctora Veronica Cruz</h6>")),
  column(12, HTML("<h6> Autor David Perez Martorell davidperezmartorell@gmail.com</h6<>")),
  column(12, HTML("<h6> Trabajo TFM para el Master Restauracion de Ecosistemaas</h6>")),
  column(12, HTML("<h6> Version 1.0.0 01/12/2023 En pruebas.Creacion de entorno, formulario y mapa base</h6>")),
  column(12, HTML("<h6> Version 1.0.1 10/12/2023 grego mas datos de pais, capital, ISO3 y latitud y longitud/h6>")),
  column(12, HTML("<h6> Version 1.0.2 12/12/2023 Carga de mapa de elevaciones/h6>")),
  column(12, HTML("<h6> Version 1.0.4 14/12/2023 Carga de mapa de bordes pa??s/h6>")),
  column(12, HTML("<h6> Version 1.0.5 16/12/2023 Carga de mapa de rios/h6>")),
  column(12, HTML("<h6> Version 1.0.6 16/12/2023 Carga de mapa de rios/h6>")),
  column(12, HTML("<h6> Version 1.1.0 30/12/2023 Reconversi??n de programa de paquete a aplicaci??n y omitir layers/h6>")),
  column(12, HTML("<h6> Version 1.1.1 07/01/2024 Se publican datos del idcomm seleccionado y los id_study con datis comunes y individuales/h6>")),
  column(12, HTML("<h6> Version 1.1.2 08/01/2024 Se agregan graficos donde se muetra relacion abundancia y riqueza respecto a??os con las disturbances/h6>")),
  column(12, HTML("<h6> Version 1.1.3 12/01/2024 Se mejoran graficos, amplitud del mapa, se agrega pesta??a para gr??ficos/h6>")),
  column(12, HTML("<h6> Version 1.1.4 13/01/2024 Creacion de tabla con datos de perturbaciones y otros datos de interes/h6>")),
  column(12, HTML("<h6> Version 1.1.5 14/01/2024 Mejora de nombres de cabeceras de tablas y indexar por id_study en lugar de id_comm/h6>")),
  column(12, HTML("<h6> Version 1.1.6 15/01/2024 Los mapas trabajan en funcion de id_study en lugar de granularse hasta id_comm/h6>")),
  column(12, HTML("<h6> Version 1.1.7 18/01/2024 Se agregan especies asociadas al estudio/h6>")),
  column(12, HTML("<h6> Version 1.1.8 22/01/2024 Lista de especies conertida en matriz segun comunicades de cada estudio y otros cambios menores/h6>")),
  column(12, HTML("<h6> Version 1.2.0 15/02/2024 Cambios en datos combinados, creaci??n de nuevos gr??ficos, nuevas pesta??as, filtrados/h6>")),
  column(12, HTML("<h6> Version 1.2.1 22/02/2024 Agrego 2 capas en mapa principal para ver por stage o disturbance/h6>")),
  column(12, HTML("<h6> Version 1.2.2 08/03/2024 Actualizo entrada de datos/h6>")),
  
)#Fin pie de pagina fluidRow
}

