#' Resumen del objeto `ml_fs`
#'
#' Devuelve un resumen con el número de variables seleccionadas y el tiempo de ejecución para cada método de selección de características.
#'
#' @param object Objeto de clase `ml_fs`.
#' @return Un data frame con el resumen .
#' @export
summary.ml_fs <- function(object, ...) {
  if (!inherits(object, "ml_fs")) {
    stop("El objeto no es de clase 'ml_fs'")
  }

  # Dataframe resumen con:
  # - Método: nombre del método de selección de características
  # - Variables seleccionadas: número de variables seleccionadas por cada método
  # - Tiempo (s): tiempo que tardó cada método, redondeado a 2 decimales
  resumen <- data.frame(
    Método = object$methods,
    `Variables seleccionadas` = sapply(object$selected_features, function(x) {
      if (is.null(x) || all(is.na(x))) 0 else length(x)
    }),
    `Tiempo (s)` = round(object$times, 2),
    check.names = FALSE,
    row.names = NULL
  )


  print(resumen)


  invisible(resumen)
}

