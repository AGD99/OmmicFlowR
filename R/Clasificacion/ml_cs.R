#' Evaluación de modelos de clasificación con variables seleccionadas
#'
#' Esta función entrena y evalúa distintos modelos de clasificación usando
#' variables seleccionadas por distintos métodos de selección de características
#' contenidos en un objeto \code{ml_fs}, aplicando validación cruzada y evaluando
#' sobre datos de test.
#'
#' @param ml_preprocessor Objeto de clase \code{ml_preprocessor} con datos de entrenamiento, test y variable objetivo.
#' @param ml_fs Objeto de clase \code{ml_fs} que contiene las variables seleccionadas por cada método.
#' @param modelos_seleccionados Vector con los nombres de los modelos a evaluar. Por defecto:
#'   \code{c("knn", "naive_bayes", "svm", "ann", "tree", "forest")}.
#' @param seed Semilla para reproducibilidad.
#' @param grid Número de combinaciones de hiperparámetros para el ajuste.
#' @param mejor_metric Métrica usada para seleccionar el mejor modelo. Por defecto: \code{"accuracy"}.
#'
#' @return Objeto de clase \code{ml_cs} con los modelos entrenados, métricas, matrices de confusión,
#'   resultados de validación cruzada, tiempos de cómputo y curvas ROC.
#' @name ml_cs
#' @export
ml_cs <- function(ml_preprocessor, ml_fs, modelos_seleccionados,
                   seed = 99, grid = 5, mejor_metric = "accuracy") {

  if (!inherits(ml_preprocessor, "ml_preprocessor")) {
    stop("El objeto debe ser de clase 'ml_preprocessor'")
  }

  if (!inherits(ml_fs, "ml_fs")) {
    stop("El objeto debe ser de clase 'ml_fs'")
  }

  #Modelos por defecto
  if (missing(modelos_seleccionados)) {
    modelos_seleccionados <- c("knn", "naive_bayes", "svm", "ann", "tree", "forest")
  }

  set.seed(seed)
  # Extracción de datos de otros objetos
  train_data <- ml_preprocessor$train
  test_data <- ml_preprocessor$test
  target <- ml_preprocessor$target
  FS <- ml_fs$selected_features

  # Comprobación si el target es binario
  niveles_target <- levels(train_data[[target]])
  binario <- length(niveles_target) == 2

  #Definición de todos los modelos posibles
  todos_los_modelos <- list(
    knn = nearest_neighbor(neighbors = tune(), weight_func = "rectangular") %>%
      set_engine("kknn") %>% set_mode("classification"),

    naive_bayes = naive_Bayes(Laplace = tune()) %>%
      set_engine("naivebayes") %>% set_mode("classification"),

    svm = svm_rbf(cost = tune(), rbf_sigma = 0.5) %>%
      set_engine("kernlab") %>% set_mode("classification"),

    ann = mlp(hidden_units = tune(), penalty = 0.1, epochs = 50) %>%
      set_engine("nnet") %>% set_mode("classification"),

    tree = decision_tree(cost_complexity = tune(), tree_depth = 5) %>%
      set_engine("rpart") %>% set_mode("classification"),

    forest = rand_forest(mtry = tune(), trees = 100) %>%
      set_engine("ranger") %>% set_mode("classification")
  )
  # Filtrar los modelos seleccionados por el usuario
  modelos <- todos_los_modelos[modelos_seleccionados]
  # Conjunto de métricas a calcular (esto necesita cambios)
  metricas <- yardstick::metric_set(
    yardstick::roc_auc,
    yardstick::accuracy,
    yardstick::sens,
    yardstick::spec,
    yardstick::precision,
    yardstick::f_meas,
    yardstick::kap
  )
  # Inicialización de listas
  resultados_modelos <- list()
  confusion_matrices <- list()
  metrics <- list()
  resultados_cv <- list()
  tiempos_final_fit <- list()
  curvas_roc <- list()
  # Iterar sobre cada método de selección de características
  for (metodo in names(FS)) {
    selected_vars <- FS[[metodo]]
    variables <- c(selected_vars, target)
    # Subconjuntos de entrenamiento y test con las variables seleccionadas
    train_subset <- train_data %>% dplyr::select(all_of(variables))
    test_subset <- test_data %>% dplyr::select(all_of(variables))

    # Iterar sobre cada modelo seleccionado
    for (modelo in names(modelos)) {
      modelo_actual <- modelos[[modelo]]
      # Crear receta y flujo de trabajo (workflow)
      receta <- recipe(as.formula(paste(target, "~ .")), data = train_subset)
      wf <- workflow() %>% add_model(modelo_actual) %>% add_recipe(receta)
      # Validación cruzada estratificada
      folds <- vfold_cv(train_subset, v = 10, strata = all_of(target))
      # Comenzar medición del tiempo
      tictoc::tic()
      # Ajuste con tune_grid y validación cruzada
      suppressMessages({
        ajuste <- tune_grid(
          wf,
          resamples = folds,
          grid = grid,
          metrics = metricas,
          control = control_grid(save_pred = TRUE)
        )
      })
      # Selección del mejor modelo según la métrica
      mejor <- select_best(ajuste, metric = mejor_metric)
      wf_final <- finalize_workflow(wf, mejor)
      # Ajuste final del modelo con todos los datos de entrenamiento
      modelo_final <- fit(wf_final, data = train_subset)
      # Predicciones sobre el conjunto de test
      test_preds <- augment(modelo_final, new_data = test_subset)
      # Probabilidades para métricas como AUC o PR AUC
      test_probs <- predict(modelo_final, new_data = test_subset, type = "prob") %>%
        bind_cols(test_subset %>% dplyr::select(all_of(target)))

      # Métricas de clasificación
      class_metrics <- test_preds %>%
        metrics(truth = !!sym(target), estimate = .pred_class)

      niveles <- levels(test_probs[[target]])
      # Para binario, la clase positiva
      clase_positiva <- niveles[2]

      # Métricas adicionales y curva ROC si el problema es binario
      if (binario) {
        prob_metrics <- bind_rows(
          test_probs %>%
            yardstick::roc_auc(truth = !!sym(target), !!sym(paste0(".pred_", clase_positiva)), event_level = "second"),
          test_probs %>%
            yardstick::pr_auc(truth = !!sym(target), !!sym(paste0(".pred_", clase_positiva)), event_level = "second")
        )
        test_metrics <- bind_rows(class_metrics, prob_metrics)

        curvas_roc[[paste(metodo, modelo, sep = "+")]] <- roc_curve(
          test_probs,
          truth = !!sym(target),
          !!sym(paste0(".pred_", clase_positiva)),
          event_level = "second"
        )
      } else {
        test_metrics <- class_metrics
        curvas_roc[[paste(metodo, modelo, sep = "+")]] <- NULL
      }
      # Matriz de confusión
      cm <- conf_mat(test_preds, truth = !!sym(target), estimate = .pred_class)

      # Fin de medición del tiempo
      tiempo <- tictoc::toc(quiet = TRUE)

      # Guardar todos los resultados en listas
      llave <- paste(metodo, modelo, sep = "+")
      resultados_modelos[[llave]] <- modelo_final
      confusion_matrices[[llave]] <- cm
      metrics[[llave]] <- test_metrics
      resultados_cv[[llave]] <- ajuste
      tiempos_final_fit[[llave]] <- tiempo$toc - tiempo$tic
      names(tiempos_final_fit)[length(tiempos_final_fit)] <- llave
    }
  }
  # Devolver todo como un objeto de clase "ml_cs"
  resultado <- list(
    resultados = resultados_modelos,
    modelos = modelos,
    seleccion_de_caracteristicas = FS,
    metricas = metricas,
    confusion_matrices = confusion_matrices,
    metrics = metrics,
    resultados_cv = resultados_cv,
    tiempos = tiempos_final_fit,
    mejor_metric = mejor_metric,
    curvas_roc = curvas_roc
  )

  class(resultado) <- "ml_cs"
  return(resultado)
}
