OmicFlowR is a modular and automated framework for supervised classification of omics data. It provides a sequential, three-stage pipeline covering data preprocessing, multi-method feature selection, and model evaluation — all with cross-validation, hyperparameter tuning, and parallel computing support.

Installation
r# Install from GitHub
# install.packages("devtools")
devtools::install_github("AGD99/OmmicFlowR")
Dependencies
OmicFlowR requires the following packages:
rinstall.packages(c(
  "dplyr", "recipes", "rsample", "future", "doFuture", "future.apply",
  "tictoc", "FSelector", "Boruta", "caret", "glmnet", "ggvenn", "ggplot2",
  "RColorBrewer", "yardstick", "tune", "workflows", "parsnip", "kknn",
  "naivebayes", "kernlab", "nnet", "rpart", "ranger", "purrr", "tibble",
  "rlang", "broom", "tidyr", "discrim"
))

Pipeline overview
The framework follows a strict sequential order:
ml_preprocessor()  →  ml_fs()  →  ml_cs()
StageFunctionDescription1ml_preprocessor()Imputation, encoding, normalization, train/test split2ml_fs()Parallel feature selection with multiple methods3ml_cs()Model training, cross-validation, and test evaluation

Usage
Stage 1 — Preprocessing
rprep <- ml_preprocessor(
  data    = my_data,       # data.frame
  target  = "class",       # name of the target variable
  seed    = 42,
  workers = 4,             # parallel cores
  verbose = TRUE
)

summary(prep)
ml_preprocessor() handles the full preprocessing pipeline: missing value imputation, categorical encoding, feature normalization, and stratified train/test partitioning. It returns an ml_preprocessor object ready for the next stage.
summary() reports the target variable, number of observations and features in each split, and class distribution.

Stage 2 — Feature selection
rfs <- ml_fs(
  prep_object  = prep,
  methods      = c("information.gain", "relief", "boruta", "lasso", "ga"),
  seed         = 42,
  workers      = 4,
  max_features = 20        # optional: cap on selected features
)

summary(fs)
Available methods: "information.gain", "chi.squared", "relief", "cfs", "boruta", "ga", "rfe", "lasso".
All methods run in parallel. The result is an ml_fs object containing the selected variables and computation time for each method.
Utility functions
r# Agreement across methods — which features were selected by the most methods?
fs_agreement(fs)

# Venn diagram for 2–5 methods
venn_features(fs, methods = c("relief", "boruta", "lasso"))

# Bar chart of computation times per method
time_fs(fs)

Stage 3 — Model evaluation
rcs <- ml_cs(
  prep_object = prep,
  fs_object   = fs,
  models      = c("knn", "svm", "forest"),
  metric      = "accuracy"
)

summary(cs)
Available classifiers: knn, naive_bayes, svm, ann, tree, forest.
For every combination of feature selection method × classifier, ml_cs() performs cross-validation with hyperparameter search, selects the best configuration, and evaluates it on the held-out test set. Results include metrics, confusion matrices, ROC curves, training times, and fitted model objects.
Results and visualisation
r# Best combinations by cross-validation performance
get_best_cv(cs, metric = "accuracy")

# Best combinations on the test set
get_best_test(cs, metric = "accuracy")

# Detailed metrics for a specific combination
get_training_metrics(cs, metodoFS = "boruta", modelo = "forest")
get_test_metrics(cs,     metodoFS = "boruta", modelo = "forest")

# Confusion matrix
confusion_matrix(cs, metodoFS = "boruta", modelo = "forest")

# Visualisations
plot_metric_cv(cs, metric = "accuracy")   # cross-validation bar chart
plot_roc(cs, metodo_fs = "boruta", modelo = "forest")  # ROC curve
plot_times(fs, cs)                        # total compute time per combination

Full example
rlibrary(OmicFlowR)

# 1. Preprocess
prep <- ml_preprocessor(data = omics_data, target = "diagnosis", seed = 123, workers = 4)

# 2. Feature selection
fs <- ml_fs(prep, methods = c("boruta", "lasso", "relief"), seed = 123, workers = 4)
fs_agreement(fs)

# 3. Classification
cs <- ml_cs(prep, fs, models = c("svm", "forest", "knn"), metric = "roc_auc")
get_best_test(cs, metric = "roc_auc")
plot_metric_cv(cs, metric = "roc_auc")

Author
Adrián Gutiérrez Domínguez
xagd99@gmail.com

License
This project is licensed under the MIT License. See the LICENSE file for details.

Bug reports
Please open an issue at https://github.com/AGD99/OmmicFlowR/issues.





