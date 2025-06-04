#' Graficar la curva ROC para una combinación de modelo y método de selección
#'
#' Esta función genera la curva ROC para una combinación específica de método de selección de características y modelo,
#' utilizando los datos almacenados en un objeto de clase \code{ml_cs}.
#'
#' @param ml_cs_object Objeto de clase \code{ml_cs} generado por la función de evaluación.
#' @param metodo_fs Nombre del método de selección de características utilizado.
#' @param modelo Nombre del modelo de clasificación evaluado.
#'
#' @return Un objeto \code{ggplot} con la curva ROC.
#' @name plot_roc
#' @export
plot_roc <- function(ml_cs_object, metodo_fs, modelo) {
  if (!inherits(ml_cs_object, "ml_cs")) {
    stop("El objeto debe ser de clase 'ml_cs'")
  }
  # Construye la clave que identifica la combinación método de selección + modelo
  combinacion <- paste(metodo_fs, modelo, sep = "+")
  # Obtiene los datos de la curva ROC para esa combinación desde el objeto ml_cs
  roc_data <- ml_cs_object$curvas_roc[[combinacion]]

  if (is.null(roc_data)) {
    stop(paste("No se encontró curva ROC para la combinación:", combinacion))
  }

  # Extraer AUC desde el objeto ml_cs
  auc_row <- ml_cs_object$metrics[[combinacion]] %>%
    dplyr::filter(.metric == "roc_auc")

  # Incluir en el titulo el AUC con 3 decimales, o NA si no está disponible
  auc <- if (nrow(auc_row) > 0) round(auc_row$.estimate[1], 3) else NA
  auc_text <- if (!is.na(auc)) paste0(" (AUC = ", auc, ")") else ""
  # Genera el gráfico ROC
  ggplot2::ggplot(roc_data, ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
    ggplot2::geom_line(color = "blue", linewidth = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(
      title = paste("Curva ROC -", combinacion, auc_text),
      x = "TFP (1 - Especificidad)",
      y = "TVP (Sensibilidad)"
    ) +
    ggplot2::theme_minimal()
}
