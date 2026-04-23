#' Graficar una métrica de validación cruzada por modelo y método FS
#'
#' Esta función genera una gráfica de barras con errores estándar para una métrica de validación cruzada
#' (por ejemplo, accuracy, ROC AUC, etc.) según el modelo y el método de selección de características.
#'
#' @param object Objeto de clase \code{ml_cs} que contiene los resultados de validación cruzada.
#' @param metrica Nombre de la métrica a graficar (por ejemplo, \code{"accuracy"}, \code{"roc_auc"}).
#'
#' @return Un objeto \code{ggplot} con la visualización de la métrica seleccionada.
#' @name plot_metric_cv
#' @export
#'
plot_metric_cv <- function(object, metrica) {
  if (!inherits(object, "ml_cs")) {
    stop("El objeto debe ser de clase 'ml_cs'")
  }

  # Extraer métricas de CV para cada combinación
  lista_metricas <- lapply(names(object$resultados_cv), function(llave) {
    res <- object$resultados_cv[[llave]]
    met <- tune::collect_metrics(res)
    met$llave <- llave
    return(met)
  })

  # Une todos los resultados
  df_metricas <- dplyr::bind_rows(lista_metricas)


  if (!metrica %in% df_metricas$.metric) {
    stop(paste0("La métrica '", metrica, "' no está disponible en los resultados de CV."))
  }

  # Filtra por la métrica deseada
  df_metricas <- df_metricas %>%
    dplyr::filter(.metric == metrica) %>%
    dplyr::select(llave, mean, std_err)

  # Separar método de FS y modelo
  df_metricas <- df_metricas %>%
    tidyr::separate(llave, into = c("metodoFS", "modelo"), sep = "\\+")

  # Dibuja grafico
  ggplot2::ggplot(df_metricas, ggplot2::aes(x = modelo, y = mean, fill = metodoFS)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - std_err, ymax = mean + std_err),
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.2
    ) +
    ggplot2::labs(
      title = "Rendimiento de la clasificación (CV)",
      x = "Modelo",
      y = paste(metrica, "(CV)"),
      fill = "Método FS"
    ) +
    ggplot2::theme_minimal()
}
