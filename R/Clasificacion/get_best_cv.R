#' Obtener resumen de mejores métricas CV por combinación
#'
#' Extrae y ordena el desempeño de validación cruzada según la métrica seleccionada.
#'
#' @param ml_cs_object Objeto de clase \code{ml_cs} generado tras evaluación.
#' @return Tibble ordenado con combinaciones y su métrica CV.
#' @name get_best_cv
#' @export
get_best_cv <- function(ml_cs_object) {

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
    ml_cs_object$resultados_cv,
    function(res) {
      metrics_tbl <- tryCatch(tune::collect_metrics(res), error = function(e) NULL)
      if (!is.null(metrics_tbl) && metric %in% metrics_tbl$.metric) {
        score <- metrics_tbl %>%
          dplyr::filter(.metric == metric) %>%
          dplyr::arrange(desc(mean)) %>%
          dplyr::slice(1) %>%
          dplyr::pull(mean)

        return(tibble::tibble(!!metric := score))
      } else {
        return(NULL)
      }
    },
    .id = "combinacion"
  )


  df_resultado <- df_resultado %>%
    dplyr::arrange(dplyr::desc(!!rlang::sym(metric)))

  return(df_resultado)
}
