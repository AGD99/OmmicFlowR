#' Gráfico del tiempo total por combinación de FS + modelo
#'
#' Esta función visualiza el tiempo total de computación por combinación de método
#' de selección de características (FS) y modelo de clasificación, sumando los tiempos
#' de la fase de selección y del entrenamiento del modelo.
#'
#' @param ml_fs Objeto de clase \code{ml_fs} con los tiempos de selección de características.
#' @param ml_cs Objeto de clase \code{ml_cs} con los tiempos de entrenamiento de modelos.
#'
#' @return Un gráfico de barras (\code{ggplot}) con los tiempos totales por combinación.
#' @name plot_times
#' @export
plot_times <- function(ml_fs, ml_cs) {
  if (!inherits(ml_fs, "ml_fs")) {
    stop("El objeto 'ml_fs' debe ser de clase 'ml_fs'.")
  }
  if (!inherits(ml_cs, "ml_cs")) {
    stop("El objeto 'ml_cs' debe ser de clase 'ml_cs'.")
  }

  # Convierte los tiempos del proceso de selección de características (FS) en un tibble con columnas 'FS' y 'tiempo_fs'
  # Asegura que 'tiempo_fs' sea numérico para cálculos posteriores
  tiempos_fs <- tibble::enframe(ml_fs$times, name = "FS", value = "tiempo_fs") %>%
    dplyr::mutate(tiempo_fs = as.numeric(tiempo_fs))


  # Convierte los tiempos de entrenamiento de los modelos en un tibble, separando la clave 'combinacion' en FS y modelo
  # Convierte 'tiempo_modelo' a numérico
  tiempos_modelos <- tibble::enframe(ml_cs$tiempos, name = "combinacion", value = "tiempo_modelo") %>%
    tidyr::separate(combinacion, into = c("FS", "modelo"), sep = "\\+") %>%
    dplyr::mutate(tiempo_modelo = as.numeric(tiempo_modelo))

  # Une los tiempos de FS y de modelo por el método de selección de características (FS)
  # Calcula el tiempo total (FS + modelo)
  # Ordena los resultados de mayor a menor tiempo total y prepara el factor para que el gráfico mantenga ese orden en el eje X
  tiempos_completos <- dplyr::left_join(tiempos_modelos, tiempos_fs, by = "FS") %>%
    dplyr::mutate(
      tiempo_total = tiempo_fs + tiempo_modelo,
      combinacion = paste(FS, modelo, sep = "+")
    ) %>%
    dplyr::arrange(dplyr::desc(tiempo_total)) %>%
    dplyr::mutate(combinacion = factor(combinacion, levels = combinacion))

  # Genera un gráfico de barras donde cada barra representa el tiempo total invertido en la combinación FS + modelo
  ggplot2::ggplot(tiempos_completos, ggplot2::aes(x = combinacion, y = tiempo_total, fill = FS)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = "Tiempo total por combinación (FS + Modelo)",
      x = "Combinación FS + Modelo",
      y = "Tiempo total (segundos)",
      fill = "Método FS"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

