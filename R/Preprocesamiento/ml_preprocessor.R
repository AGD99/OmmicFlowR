#' Crear objeto ml_preprocessor con paralelización
#'
#' Esta función preprocesa los datos, divide en entrenamiento/test y devuelve un objeto de clase `ml_preprocessor`.
#' Utiliza paralelización automática con `future` para acelerar el preprocesamiento.
#'
#' @param data Data frame con los datos originales.
#' @param target Nombre de la variable objetivo (como string).
#' @param seed Semilla para la aleatorización.
#' @param workers Número de núcleos o workers a usar para la paralelización.
#' @param verbose Mensajes de progreso.
#'
#' @return Un objeto de clase `ml_preprocessor` con los datos procesados y divididos.
#' @export
ml_preprocessor <- function(data, target, seed = 99, workers = 2,verbose = TRUE) {
  if (verbose) message("Iniciando preprocesamiento...")

  if (verbose) message("Activando backend paralelo con ", workers, " workers.")
  future::plan(future::multisession, workers = workers)
  doFuture::registerDoFuture()
  on.exit(future::plan(future::sequential))
  set.seed(seed)
  #Eliminamos filas con NA en la variable objetivo
  data <- data[!is.na(data[[target]]), ]
  #Convertimos la variable objetivo a factor
  if (verbose) message("Convirtiendo variable objetivo en factor...")
  data[[target]] <- as.factor(data[[target]])

  #Dividimos los datos en entrenamiento (70%) y test (30%) con estratificación
  if (verbose) message("Dividiendo en entrenamiento y test ...")
  split_obj <- rsample::initial_split(data, prop = 0.7, strata = target)
  train_data <- rsample::training(split_obj)
  test_data  <- rsample::testing(split_obj)
  #Creamos la receta de preprocesamiento para el conjunto de entrenamiento
  #  - Imputa valores faltantes numéricos con la media de la variable.
  #  - Detecta categorías nuevas en variables categóricas para manejarlas correctamente.
  #  - Convierte variables categóricas en variables dummy (onehot encoding).
  #  - Elimina variables con varianza cero que no aportan información.
  #  - Normaliza variables numéricas para que estén en el rango [0, 1]
  if (verbose) message("Creando receta de preprocesamiento...")
  receta <- recipes::recipe(as.formula(paste(target, "~ .")), data = train_data) %>%
    recipes::step_impute_mean(recipes::all_predictors()) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_range(recipes::all_numeric_predictors(), min = 0, max = 1)
  #Prepararamos la receta con los datos de entrenamiento
  if (verbose) message("Preparando receta ...")
  receta_preparada <- recipes::prep(receta, training = train_data)
  #Aplicar la receta preparada a los datos de entrenamiento y test
  if (verbose) message("Aplicando preprocesamiento a los datos de entrenamiento y test...")
  train_procesado <- recipes::bake(receta_preparada, new_data = train_data)
  test_procesado  <- recipes::bake(receta_preparada, new_data = test_data)


  #Construimos el objeto resultante con los datos procesados
  if (verbose) message("Construyendo objeto ml_data...")

  objeto <- list(
    train = train_procesado,
    test = test_procesado,
    target = target,
    split = split_obj
  )
  class(objeto) <- "ml_preprocessor"

  if (verbose) message("Proceso completado correctamente.")

  return(objeto)
}


