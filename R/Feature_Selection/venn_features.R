#' Diagrama de Venn para visualizar FS
#'
#' Genera un diagrama de Venn usando el paquete `ggvenn` para visualizar la intersección
#' de variables seleccionadas por distintos métodos de selección de características.
#'
#' @param ml_fs Objeto de clase `ml_fs`.
#' @param methods Vector de nombres de métodos a incluir en el diagrama (mínimo 2, máximo 5).
#'
#' @return Un objeto `ggplot` con el diagrama de Venn.
#' @export
venn_features <- function(ml_fs, methods) {
  if (!inherits(ml_fs, "ml_fs")) {
    stop("El objeto debe ser de clase 'ml_fs'.")
  }
  # Verificar que el número de métodos esté entre 2 y 5,
  if (length(methods) < 2 || length(methods) > 5) {
    stop("Debes seleccionar entre 2 y 5 métodos.")
  }
  # Extraer las características seleccionadas correspondientes a los métodos indicados
  sets <- ml_fs$selected_features[methods]
  # Crear el diagrama de Venn usando la librería ggvenn, que es compatible con ggplot2
  ggvenn::ggvenn(sets) +
    ggplot2::theme_void()
}
