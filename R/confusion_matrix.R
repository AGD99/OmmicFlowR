#' Obtener matriz de confusión para una combinación FS + modelo
#'
#' Extrae la matriz de confusión almacenada en un objeto de clase \code{ml_cs}
#' para un método de selección de características y modelo específico.
#'
#' @param object Objeto de clase \code{ml_cs}.
#' @param metodoFS Nombre del método de selección de características (carácter).
#' @param modelo Nombre del modelo (carácter).
#' @return Matriz de confusión correspondiente a la combinación dada.
#' @name confusion_matrix.ml_cs
#' @export
confusion_matrix.ml_cs <- function(object, metodoFS, modelo) {
  if (!inherits(object, "ml_cs")) {
    stop("El objeto no es de clase 'ml_cs'.")
  }
  # Crea la clave que identifica la combinación método de selección + modelo
  llave <- paste(metodoFS, modelo, sep = "+")

  # Comprueba que la combinación exista en la lista de métricas
  if (!llave %in% names(object$confusion_matrices)) {
    stop("La combinación de método de selección y modelo no se encuentra en los resultados.")
  }
  # Devuelve la matriz de confusión correspondiente a esa combinación
  return(object$confusion_matrices[[llave]])
}

