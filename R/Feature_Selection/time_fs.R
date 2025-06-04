#' Gráfico de tiempos de ejecución de métodos de FS
#'
#' Genera un gráfico de barras con los tiempos (en segundos) que tardó cada método de selección.
#'
#' @param ml_fs Objeto de clase `ml_fs`.
#'
#' @return Un objeto `ggplot`.
#' @name time_fs
#' @export

time_fs <- function(ml_fs) {
  if (!inherits(ml_fs, "ml_fs")) {
    stop("El objeto debe ser de clase 'ml_fs'.")
  }
  # Dataframe con los nombres de los métodos y sus tiempos de ejecución
  df <- data.frame(
    Metodo = names(ml_fs$times),
    Tiempo = unname(ml_fs$times)
  )
  #Crear un gráfico de barras horizontal usando ggplot2 para mostrar los tiempos de ejecución
  ggplot2::ggplot(df, ggplot2::aes(x = reorder(Metodo, Tiempo), y = Tiempo, fill = Metodo)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(
      title = "Tiempo de ejecución por método de selección",
      x = "Método",
      y = "Tiempo (segundos)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip()
}

