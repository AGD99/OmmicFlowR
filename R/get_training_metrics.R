#' Obtener métricas de entrenamiento por combinación FS + modelo
#'
#' Esta función permite extraer las métricas de validación cruzada obtenidas
#' durante el entrenamiento de un modelo específico, asociado a un método de selección de características.
#'
#' @param object Objeto de clase \code{ml_cs} que contiene los resultados del proceso de clasificación.
#' @param metodoFS Nombre del método de selección de características utilizado.
#' @param modelo Nombre del modelo evaluado.
#'
#' @return Un data frame con las métricas de validación cruzada para la combinación especificada.
#' @name get_training_metrics
#' @export

get_training_metrics <- function(object, metodoFS, modelo) {
  if (!inherits(object, "ml_cs")) {
    stop("El objeto no es de clase 'ml_cs'.")
  }
  # Crea la clave para acceder a la combinación de método y modelo
  llave <- paste(metodoFS, modelo, sep = "+")
  # Comprueba que exista dicha combinación en los resultados de CV
  if (!llave %in% names(object$resultados_cv)) {
    stop("La combinación 'metodo+modelo' no existe en los resultados.")
  }
  # Recupera el objeto de CV ajustado
  ajuste <- object$resultados_cv[[llave]]
  # Extrae las métricas con tune::collect_metrics
  met <- tune::collect_metrics(ajuste)

  # Filtra solo las métricas de interés (para que devuelva las mismas metrica que get_test_metrics)
  met_filtrado <- met %>%
    dplyr::filter(.metric %in% c("accuracy", "kap")) %>%
    dplyr::select(-n, -.config, -std_err)

  return(met_filtrado)
}

