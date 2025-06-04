#' Resumen de un objeto de clase `ml_cs`
#'
#' Muestra un resumen del proceso de evaluación de modelos, incluyendo
#' los métodos de selección de características utilizados, los modelos evaluados,
#' el número total de combinaciones evaluadas y los tiempos de entrenamiento.
#'
#' @param object Objeto de clase \code{ml_cs} devuelto por la función \code{ml_cs2()}.
#' @param ... Argumentos adicionales (no utilizados actualmente).
#' @name summary.ml_cs
#' @return Imprime un resumen en consola.
#' @export
summary.ml_cs <- function(object, ...) {
  cat("Resumen del objeto 'ml_cs'\n")
  cat("----------------------------------\n")


  cat("Métodos de selección de características (FS):\n")
  cat(paste("- ", names(object$seleccion_de_caracteristicas)), sep = "\n")
  cat("\n")


  cat("Modelos evaluados:\n")
  cat(paste("- ", names(object$modelos)), sep = "\n")
  cat("\n")


  cat("Total de combinaciones FS + modelo evaluadas: ", length(object$resultados), "\n\n")

  cat("Tiempos de entrenamiento por combinación:\n")
  tiempos_df <- data.frame(
    Combinacion = names(object$tiempos),
    Tiempo_segundos = round(unlist(object$tiempos), 2)
  )
  print(tiempos_df, row.names = FALSE)
}
