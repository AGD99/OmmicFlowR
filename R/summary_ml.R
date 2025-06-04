#' Resumen de objeto ml_preprocessor
#'
#' Muestra un resumen básico del objeto `ml_preprocessor`, incluyendo
#' la variable objetivo, tamaño y distribución de clases en los conjuntos
#' de entrenamiento y test.
#'
#' @param object Objeto de clase `ml_preprocessor`.
#'
#' @export
summary.ml_preprocessor <- function(object, ...) {
  # Título del resumen
  cat("Resumen del objeto 'ml_preprocessor'\n")
  cat("-----------------------------------\n")
  # Mostrar la variable objetivo que se va a predecir
  cat("Variable objetivo:", object$target, "\n\n")
  # Información sobre el conjunto de entrenamiento
  cat("Conjunto de entrenamiento:\n")
  # Número de observaciones (filas)
  cat("- Observaciones:", nrow(object$train), "\n")
  # Número de variables (columnas)
  cat("- Variables:", ncol(object$train), "\n")
  # Distribución de clases de la variable objetivo
  cat("- Distribución de clases:\n")
  print(table(object$train[[object$target]]))
  cat("\n")

  # Información sobre el conjunto de test
  cat("Conjunto de test:\n")
  # Número de observaciones (filas)
  cat("- Observaciones:", nrow(object$test), "\n")
  # Número de variables (columnas)
  cat("- Variables:", ncol(object$test), "\n")
  # Distribución de clases de la variable objetivo
  cat("- Distribución de clases:\n")
  print(table(object$test[[object$target]]))
}
