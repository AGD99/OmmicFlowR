#' Plot de métricas de test para modelos de clasificación
#'
#' Genera un gráfico de barras con los valores de una métrica de test para
#' cada combinación de método de selección de características y modelo.
#'
#' @param object Un objeto de clase `ml_cs`, generado por la función `ml_cs()`.
#' @param metrica Cadena de texto que indica la métrica de evaluación a graficar.
#' @name plot_metric_test
#' @return Un gráfico de ggplot2 con la métrica de test para cada modelo y método FS.
#' @export
plot_metric_test <- function(object, metrica) {
  if (!inherits(object, "ml_cs")) {
    stop("El objeto debe ser de clase 'ml_cs'")
  }

  # Construye una lista donde cada elemento es un data.frame de métricas para cada combinación método+modelo
  # Añade una columna 'llave' con el nombre de la combinación para identificarlo luego
  lista_metricas <- lapply(names(object$metrics), function(llave) {
    met <- object$metrics[[llave]]
    met$llave <- llave
    return(met)
  })

  # Une todas las tablas en un solo dataframe
  df_metricas <- dplyr::bind_rows(lista_metricas)


  if (!metrica %in% df_metricas$.metric) {
    stop(paste0("La métrica '", metrica, "' no está disponible en los resultados de test."))
  }

  # Filtra el dataframe para quedarse solo con la métrica solicitada y la columna de estimaciones
  df_metricas <- df_metricas %>%
    dplyr::filter(.metric == metrica) %>%
    dplyr::select(llave, .estimate)

  # Separa la columna 'llave' en dos: método de selección de características y modelo de clasificacion
  df_metricas <- df_metricas %>%
    tidyr::separate(llave, into = c("metodoFS", "modelo"), sep = "\\+")

  # Dibuja un gráfico de barras
  ggplot2::ggplot(df_metricas, ggplot2::aes(x = modelo, y = .estimate, fill = metodoFS)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::labs(
      title = "Rendimiento de la clasificación (Test)",
      x = "Modelo",
      y = paste(metrica, "(Test)"),
      fill = "Método FS"
    ) +
    ggplot2::theme_minimal()
}

