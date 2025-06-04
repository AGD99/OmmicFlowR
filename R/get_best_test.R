#' Obtener mejores métricas de test por combinación
#'
#' Extrae y ordena el desempeño en datos de test según la métrica seleccionada.
#'
#' @param ml_cs_object Objeto de clase \code{ml_cs} que contiene resultados de evaluación.
#' @return Tibble ordenado con combinaciones y su métrica en test.
#' @name get_best_test
#' @export
get_best_test <- function(ml_cs_object) {
  if (!inherits(ml_cs_object, "ml_cs")) {
    stop("El objeto debe ser de clase 'ml_cs'.")
  }
  # Extrae la métrica que se usará para determinar el mejor modelo
  metric <- ml_cs_object$mejor_metric

  if (is.null(metric) || !is.character(metric) || length(metric) != 1) {
    stop("El campo 'mejor_metric' debe estar definido como un único string en ml_cs_object.")
  }
  # Para cada combinación de método de selección y modelo, extrae el valor de la métrica indicada
  # (por ejemplo, "accuracy") del conjunto de test. Devuelve un data frame con una fila por combinación
  # y una columna con el valor de dicha métrica. Luego ordena las combinaciones de mayor a menor puntuación.

  df_resultado <- purrr::map_dfr(
    ml_cs_object$metrics,
    function(res) {
      if (!is.null(res) && metric %in% res$.metric) {
        score <- res %>%
          dplyr::filter(.metric == metric) %>%
          dplyr::pull(.estimate)
        tibble::tibble(!!metric := score[1])
      } else {
        NULL
      }
    },
    .id = "combinacion" # Añade como columna el nombre del elemento en la lista (método+modelo)
  ) %>%
    dplyr::arrange(dplyr::desc(!!rlang::sym(metric)))

  return(df_resultado)
}
