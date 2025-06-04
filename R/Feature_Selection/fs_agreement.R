#' Calcula el acuerdo entre métodos de selección de características
#'
#' Esta función evalúa cuántos métodos de selección han seleccionado cada característica.
#' Devuelve un data frame donde cada fila representa una característica, con columnas indicando
#' si fue seleccionada por cada método y cuántos métodos la seleccionaron en total.
#'
#' @param ml_fs_obj Un objeto de clase \code{ml_fs} que contiene los resultados de la selección de características.
#'
#' @return Un data frame con:
#' \itemize{
#'   \item Una fila por cada característica seleccionada por al menos un método.
#'   \item Columnas lógicas indicando qué métodos seleccionaron la característica.
#'   \item Una columna \code{agree} con el número total de métodos que seleccionaron esa característica.
#' }
#'
#' @export
fs_agreement <- function(ml_fs_obj) {
  if (!inherits(ml_fs_obj, "ml_fs")) {
    stop("El objeto debe ser de clase 'ml_fs'")
  }

  # Obtener todas las características seleccionadas por al menos un método, sin duplicados
  all_features <- unique(unlist(ml_fs_obj$selected_features))
  # Obtener la lista de métodos usados
  methods <- ml_fs_obj$methods

  # Crear una matriz lógica donde cada fila es una característica y cada columna un método,
  # indicando con TRUE si la característica fue seleccionada por ese método
  selection_matrix <- sapply(methods, function(m) {
    feature_set <- ml_fs_obj$selected_features[[m]]
    all_features %in% feature_set
  })

  # Convertir la matriz a dataframe
  selection_df <- as.data.frame(selection_matrix)
  # Asignar los nombres de las características como nombres de fila
  rownames(selection_df) <- all_features

  # Calcular la suma de selecciones para cada característica (cuántos métodos la seleccionaron)
  selection_df$agree <- rowSums(selection_df)

  # Ordenar el data.frame descendientemente según el número de acuerdos
  selection_df <- selection_df[order(-selection_df$agree), ]

  return(selection_df)
}
