#!/usr/bin/env Rscript

# Run once in a clean R environment. Analysis scripts themselves never install
# packages automatically, which keeps computational runs auditable.

cran_packages <- c(
  "readxl", "openxlsx", "mice", "caret", "randomForest", "glmnet",
  "earth", "kknn", "xgboost", "gbm", "kernlab", "lightgbm", "ggplot2",
  "rBayesianOptimization", "Boruta", "remotes"
)

missing <- cran_packages[!cran_packages %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing, dependencies = TRUE)

# fastshap 0.3.0 is maintained in the author's repository. It was archived on
# CRAN in May 2026, so install it from the maintained upstream source.
if (!requireNamespace("fastshap", quietly = TRUE)) {
  remotes::install_github("bgreenwell/fastshap")
}

