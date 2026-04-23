#' Obtener métricas de test por combinación FS + modelo
#'
#' Esta función extrae las métricas de evaluación en el conjunto de test
#' para una combinación específica de método de selección de características y modelo.
#'
#' @param object Objeto de clase \code{ml_cs} que contiene los resultados del proceso de clasificación.
#' @param metodoFS Nombre del método de selección de características utilizado.
#' @param modelo Nombre del modelo evaluado.
#' @param ... Argumentos adicionales (no utilizados actualmente).
#'
#' @return Un data frame con las métricas de evaluación en test para la combinación especificada.
#' @name get_test_metrics
#' @export
get_test_metrics <- function(object, metodoFS, modelo, ...) {
  if (!inherits(object, "ml_cs")) {
    stop("El objeto no es de clase 'ml_cs'.")
  }
  # Crea la clave de acceso a los resultados: método de selección + modelo
  llave <- paste(metodoFS, modelo, sep = "+")
  # Comprueba que la combinación exista en la lista de métricas
  if (!llave %in% names(object$metrics)) {
    stop("La combinación 'metodoFS+modelo' no existe en las métricas de test.")
  }
  # Extrae y devuelve el data frame de métricas correspondiente
  met <- object$metrics[[llave]]

  return(met)
}


