#' Selección de características con múltiples métodos en paralelo
#'
#' Esta función aplica distintos métodos de selección de características
#' sobre los datos de entrenamiento de un objeto \code{ml_data},
#' usando paralelismo para acelerar el cálculo.
#'
#' @param ml_data Lista con datos ya particionados, debe contener al menos
#'   \code{train} (data frame de entrenamiento) y \code{target} (nombre de la variable objetivo).
#' @param methods Vector de caracteres con nombres de métodos de selección
#'   a ejecutar. Métodos disponibles:
#'   \itemize{
#'     \item \code{"information.gain"}: Ganancia de información (Information Gain)
#'     \item \code{"chi.squared"}: Chi-cuadrado
#'     \item \code{"relief"}: Algoritmo Relief
#'     \item \code{"cfs"}: Selección por subconjunto correlacionado (Correlation-based Feature Selection)
#'     \item \code{"boruta"}: Algoritmo Boruta basado en árboles aleatorios
#'     \item \code{"ga"}: Algoritmo genético con Random Forest (gafs)
#'     \item \code{"rfe"}: Recursive Feature Elimination con Random Forest
#'     \item \code{"lasso"}: Selección mediante Lasso (regresión regularizada)
#'   }

#' @param seed Número entero para fijar la semilla aleatoria.
#' @param workers Número de procesos paralelos para usar.
#' @param verbose Lógico. Si \code{TRUE}, muestra mensajes durante la ejecución)
#'
#' @return Objeto de clase \code{ml_fs} con los métodos ejecutados, las variables
#'   seleccionadas por cada método y los tiempos de ejecución.
#' @export
ml_fs <- function(ml_data, methods = c(""), seed, workers, verbose = TRUE, max_features = NULL) {
  #semilla
  set.seed(seed)
  # Extraer datos de entrenamiento y variable objetivo
  train_data <- ml_data$train
  target <- ml_data$target

  # Crear fórmula para selección de características: target ~ todas las demás variables
  formula <- as.formula(paste(target, "~ ."))
  # Configurar paralelización con futuros usando el número de workers especificado

  future::plan(future::multisession, workers = workers)
  doFuture::registerDoFuture()
  on.exit(future::plan(future::sequential))

  # Ejecutar cada método de selección en paralelo con future_lapply
  resultados <- future.apply::future_lapply(methods, function(method) {
    method_result <- NULL

    if (verbose) message("Ejecutando método: ", method)
    tictoc::tic(method)  # Iniciar temporizador
    # Para cada método, calcular la selección de características correspondiente:

    if (method == "information.gain") {
      scores <- FSelector::information.gain(formula, train_data)
      selected <- FSelector::cutoff.biggest.diff(scores)
      if (!is.null(max_features)) {
        selected <- head(selected[order(-scores[selected, 1])], max_features)
      }
      method_result <- selected

    } else if (method == "chi.squared") {
      scores <- FSelector::chi.squared(formula, train_data)
      selected <- FSelector::cutoff.biggest.diff(scores)
      if (!is.null(max_features)) {
        selected <- head(selected[order(-scores[selected, 1])], max_features)
      }
      method_result <- selected

    } else if (method == "relief") {
      scores <- FSelector::relief(formula, train_data, neighbours.count = 5)
      selected <- FSelector::cutoff.biggest.diff(scores)
      if (!is.null(max_features)) {
        selected <- head(selected[order(-scores[selected, 1])], max_features)
      }
      method_result <- selected

    } else if (method == "cfs") {
      selected <- FSelector::cfs(formula, train_data)
      if (!is.null(max_features)) {
        selected <- head(selected, max_features)
      }
      method_result <- selected

    } else if (method == "boruta") {
      borutaFS <- Boruta::Boruta(formula, data = train_data, doTrace = 0)
      selected <- Boruta::getSelectedAttributes(borutaFS, withTentative = FALSE)

      if (!is.null(max_features)) {
        imp <- Boruta::attStats(borutaFS)
        selected <- intersect(selected, rownames(imp))
        mean_imps <- imp[selected, "meanImp", drop = FALSE]
        ordered_selected <- rownames(mean_imps)[order(mean_imps[, 1], decreasing = TRUE)]
        selected <- head(ordered_selected, max_features)
      }

      method_result <- selected

    } else if (method == "ga") {
      suppressPackageStartupMessages({
        ga_functions <- caret::rfGA
        control <- caret::gafsControl(
          functions = ga_functions,
          method = "cv",
          number = 5,
          verbose = FALSE
        )
        GAmodel <- caret::gafs(
          x = train_data[, setdiff(names(train_data), target), drop = FALSE],
          y = train_data[[target]],
          iters = 5,
          gafsControl = control
        )
      })

      selected <- GAmodel$optVariables
      if (!is.null(max_features)) {
        selected <- head(selected, max_features)
      }
      method_result <- selected

    } else if (method == "rfe") {
      suppressPackageStartupMessages({
        ctrlrfe <- caret::rfeControl(
          functions = caret::rfFuncs,
          method = "cv",
          number = 5,
          verbose = FALSE,
          allowParallel = TRUE
        )
        rfe_result <- caret::rfe(
          x = train_data[, setdiff(names(train_data), target)],
          y = train_data[[target]],
          sizes = seq_len(ncol(train_data) - 1),
          rfeControl = ctrlrfe,
          metric = "Accuracy",
          maximize = TRUE
        )
      })

      selected <- rfe_result$optVariables
      if (!is.null(max_features)) {
        selected <- head(selected, max_features)
      }
      method_result <- selected

    } else if (method == "lasso") {
      x <- as.matrix(train_data[, setdiff(names(train_data), target)])
      y <- train_data[[target]]
      lasso_model <- glmnet::cv.glmnet(
        x = x,
        y = y,
        alpha = 1,
        family = "binomial",
        standardize = FALSE,
        nfolds = 5,
        parallel = FALSE
      )
      coefs <- as.matrix(coef(lasso_model, s = "lambda.1se"))
      selected <- rownames(coefs)[which(coefs[,1] != 0)]
      selected <- setdiff(selected, "(Intercept)")
      if (!is.null(max_features)) {
        imp <- abs(coefs[selected, 1])
        selected <- head(names(sort(imp, decreasing = TRUE)), max_features)
      }
      method_result <- selected
    }
    # Registrar tiempo transcurrido para el método
    log_tic <- tictoc::toc(log = TRUE, quiet = TRUE)
    elapsed_time <- log_tic$toc - log_tic$tic
    if (verbose) message("Tiempo: ", round(elapsed_time, 2), " segundos")
    # Devolver lista con variables seleccionadas y tiempo usado
    list(selected = method_result, time = elapsed_time)
  }, future.seed = TRUE)
  # Asignar nombres a la lista de resultados con los nombres de los métodos
  names(resultados) <- methods
  # Extraer las características seleccionadas de cada método
  selected_features <- lapply(resultados, function(x) x$selected)
  # Extraer tiempos usados por cada método
  tiempos <- sapply(resultados, function(x) x$time)
  names(tiempos) <- methods
  # Construir objeto de clase ml_fs con resultados y tiempos
  objeto <- list(
    methods = methods,
    selected_features = selected_features,
    times = tiempos
  )

  class(objeto) <- "ml_fs"
  return(objeto)
}



