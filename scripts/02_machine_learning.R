#!/usr/bin/env Rscript

# Leakage-controlled, article-grouped multi-model comparison.
# Important: this script reads the raw incomplete data, not 01_imputation output.

script_root <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    script <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
    return(normalizePath(file.path(dirname(script), ".."), mustWork = FALSE))
  }
  normalizePath(Sys.getenv("LDH_PROJECT_ROOT", unset = getwd()), mustWork = FALSE)
}

fit_caret_once <- function(method, tune_row, x, y, extra_args = list()) {
  control <- caret::trainControl(method = "none", allowParallel = FALSE)
  arguments <- c(
    list(
      x = as.data.frame(x, check.names = FALSE),
      y = as.numeric(y),
      method = method,
      trControl = control,
      tuneGrid = as.data.frame(tune_row, check.names = FALSE)
    ),
    extra_args
  )
  do.call(caret::train, arguments)
}

predict_fitted_model <- function(object, model_name, new_x) {
  if (identical(model_name, "LightGBM")) {
    return(as.numeric(stats::predict(object, as.matrix(new_x))))
  }
  as.numeric(stats::predict(object, newdata = as.data.frame(new_x, check.names = FALSE)))
}

build_fold_cache <- function(training_data, validation_folds, cfg) {
  cache <- vector("list", length(validation_folds))
  for (fold in seq_along(validation_folds)) {
    cat("  Preprocessing CV fold", fold, "of", length(validation_folds), "\n")
    validation_index <- validation_folds[[fold]]
    fitting_index <- setdiff(seq_len(nrow(training_data)), validation_index)
    preprocessor <- fit_analysis_preprocessor(training_data[fitting_index, , drop = FALSE], cfg)
    validation_processed <- apply_analysis_preprocessor(
      preprocessor,
      training_data[validation_index, , drop = FALSE],
      cfg
    )
    cache[[fold]] <- list(
      fold = fold,
      train_index = fitting_index,
      validation_index = validation_index,
      x_train = preprocessor$training_x,
      y_train = training_data[[cfg$target_var]][fitting_index],
      x_validation = validation_processed$x,
      y_validation = training_data[[cfg$target_var]][validation_index]
    )
  }
  cache
}

evaluate_caret_parameter <- function(
    model_name,
    method,
    tune_row,
    fold_cache,
    extra_args = list(),
    quiet = TRUE) {
  metrics <- vector("list", length(fold_cache))
  errors <- character(length(fold_cache))
  for (fold in seq_along(fold_cache)) {
    item <- fold_cache[[fold]]
    fit <- tryCatch(
      fit_caret_once(method, tune_row, item$x_train, item$y_train, extra_args),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      errors[fold] <- conditionMessage(fit)
      metrics[[fold]] <- data.frame(R2 = NA_real_, RMSE = NA_real_, MAE = NA_real_, N = length(item$y_validation))
      next
    }
    prediction <- tryCatch(
      predict_fitted_model(fit, model_name, item$x_validation),
      error = function(e) e
    )
    if (inherits(prediction, "error")) {
      errors[fold] <- conditionMessage(prediction)
      metrics[[fold]] <- data.frame(R2 = NA_real_, RMSE = NA_real_, MAE = NA_real_, N = length(item$y_validation))
    } else {
      metrics[[fold]] <- regression_metrics(item$y_validation, prediction)
    }
  }
  metrics <- do.call(rbind, metrics)
  metrics$Fold <- seq_len(nrow(metrics))
  metrics$Model <- model_name
  metrics$Error <- errors
  valid <- is.finite(metrics$R2) & is.finite(metrics$RMSE) & is.finite(metrics$MAE)
  summary <- data.frame(
    Model = model_name,
    Mean_R2 = if (any(valid)) mean(metrics$R2[valid]) else NA_real_,
    SD_R2 = if (sum(valid) > 1L) stats::sd(metrics$R2[valid]) else NA_real_,
    Mean_RMSE = if (any(valid)) mean(metrics$RMSE[valid]) else NA_real_,
    SD_RMSE = if (sum(valid) > 1L) stats::sd(metrics$RMSE[valid]) else NA_real_,
    Mean_MAE = if (any(valid)) mean(metrics$MAE[valid]) else NA_real_,
    SD_MAE = if (sum(valid) > 1L) stats::sd(metrics$MAE[valid]) else NA_real_,
    Successful_Folds = sum(valid),
    row.names = NULL
  )
  if (!quiet) print(summary)
  list(summary = summary, fold_metrics = metrics, tune = as.data.frame(tune_row, check.names = FALSE))
}

parameter_text <- function(data_frame) {
  apply(data_frame, 1, function(row) paste(paste(names(row), row, sep = "="), collapse = "; "))
}

rbind_fill <- function(data_frames) {
  all_names <- unique(unlist(lapply(data_frames, names), use.names = FALSE))
  prepared <- lapply(data_frames, function(data_frame) {
    missing <- setdiff(all_names, names(data_frame))
    for (name in missing) data_frame[[name]] <- NA
    data_frame[all_names]
  })
  do.call(rbind, prepared)
}

evaluate_caret_grid <- function(model_name, method, grid, fold_cache, extra_args = list()) {
  cat("Tuning", model_name, "with", nrow(grid), "candidate settings\n")
  evaluations <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    evaluations[[i]] <- evaluate_caret_parameter(
      model_name,
      method,
      grid[i, , drop = FALSE],
      fold_cache,
      extra_args
    )
  }
  tuning <- do.call(rbind, lapply(seq_along(evaluations), function(i) {
    cbind(
      Candidate = i,
      evaluations[[i]]$tune,
      evaluations[[i]]$summary[, setdiff(names(evaluations[[i]]$summary), "Model"), drop = FALSE]
    )
  }))
  eligible <- which(is.finite(tuning$Mean_R2) & tuning$Successful_Folds == length(fold_cache))
  if (!length(eligible)) stop("No valid tuning candidate for ", model_name, call. = FALSE)
  ordering <- eligible[order(-tuning$Mean_R2[eligible], tuning$Mean_RMSE[eligible], tuning$Mean_MAE[eligible])]
  best_index <- ordering[1]
  list(
    model_name = model_name,
    best_tune = grid[best_index, , drop = FALSE],
    summary = evaluations[[best_index]]$summary,
    fold_metrics = evaluations[[best_index]]$fold_metrics,
    tuning_results = tuning
  )
}

caret_generated_grid <- function(method, x, y, length_out, search = "grid") {
  info <- caret::getModelInfo(method, regex = FALSE)[[1]]
  set.seed(42)
  grid <- info$grid(x = as.data.frame(x), y = y, len = as.integer(length_out), search = search)
  unique(as.data.frame(grid, check.names = FALSE))
}

tune_random_forest <- function(fold_cache, full_x, cfg) {
  require_packages("rBayesianOptimization")
  min_features <- min(vapply(fold_cache, function(x) ncol(x$x_train), integer(1)))
  lower <- if (min_features >= 2L) 2L else 1L
  upper <- max(lower, min(30L, min_features))
  objective <- function(mtry) {
    mtry <- max(lower, min(upper, as.integer(round(mtry))))
    result <- evaluate_caret_parameter(
      "Random_Forest",
      "rf",
      data.frame(mtry = mtry),
      fold_cache,
      extra_args = list(ntree = as.integer(cfg$modeling$rf_tuning_trees), importance = TRUE)
    )
    score <- result$summary$Mean_R2
    if (!is.finite(score)) score <- -1e6
    list(Score = score, Pred = 0)
  }
  set.seed(as.integer(cfg$modeling$seed))
  optimization <- rBayesianOptimization::BayesianOptimization(
    FUN = objective,
    bounds = list(mtry = c(lower, upper)),
    init_points = as.integer(cfg$modeling$bayesian_init_points),
    n_iter = as.integer(cfg$modeling$bayesian_n_iter),
    acq = cfg$modeling$bayesian_acq,
    kappa = cfg$modeling$bayesian_kappa,
    eps = 0,
    verbose = TRUE
  )
  best_tune <- data.frame(mtry = as.integer(round(unname(optimization$Best_Par[["mtry"]]))))
  final_cv <- evaluate_caret_parameter(
    "Random_Forest",
    "rf",
    best_tune,
    fold_cache,
    extra_args = list(ntree = as.integer(cfg$modeling$rf_final_trees), importance = TRUE)
  )
  list(
    model_name = "Random_Forest",
    best_tune = best_tune,
    summary = final_cv$summary,
    fold_metrics = final_cv$fold_metrics,
    tuning_results = as.data.frame(optimization$History),
    optimization = optimization
  )
}

tune_xgboost <- function(fold_cache, cfg) {
  require_packages(c("rBayesianOptimization", "xgboost"))
  objective <- function(max_depth, eta, gamma, subsample, colsample_bytree, min_child_weight) {
    tune <- data.frame(
      nrounds = 100L,
      max_depth = as.integer(round(max_depth)),
      eta = eta,
      gamma = gamma,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      subsample = subsample
    )
    result <- evaluate_caret_parameter(
      "XGBoost",
      "xgbTree",
      tune,
      fold_cache,
      extra_args = list(verbosity = 0, nthread = 1, lambda = 1, alpha = 0.5)
    )
    score <- result$summary$Mean_R2
    if (!is.finite(score)) score <- -1e6
    list(Score = score, Pred = 0)
  }
  set.seed(as.integer(cfg$modeling$seed))
  optimization <- rBayesianOptimization::BayesianOptimization(
    FUN = objective,
    bounds = list(
      max_depth = c(3L, 10L),
      eta = c(0.01, 0.30),
      gamma = c(0, 5),
      subsample = c(0.5, 1),
      colsample_bytree = c(0.5, 1),
      min_child_weight = c(1, 10)
    ),
    init_points = as.integer(cfg$modeling$bayesian_init_points),
    n_iter = as.integer(cfg$modeling$bayesian_n_iter),
    acq = cfg$modeling$bayesian_acq,
    kappa = cfg$modeling$bayesian_kappa,
    eps = 0,
    verbose = TRUE
  )
  bp <- optimization$Best_Par
  best_tune <- data.frame(
    nrounds = 100L,
    max_depth = as.integer(round(bp[["max_depth"]])),
    eta = bp[["eta"]],
    gamma = bp[["gamma"]],
    colsample_bytree = bp[["colsample_bytree"]],
    min_child_weight = bp[["min_child_weight"]],
    subsample = bp[["subsample"]]
  )
  final_cv <- evaluate_caret_parameter(
    "XGBoost",
    "xgbTree",
    best_tune,
    fold_cache,
    extra_args = list(verbosity = 0, nthread = 1, lambda = 1, alpha = 0.5)
  )
  list(
    model_name = "XGBoost",
    best_tune = best_tune,
    summary = final_cv$summary,
    fold_metrics = final_cv$fold_metrics,
    tuning_results = as.data.frame(optimization$History),
    optimization = optimization
  )
}

sample_lightgbm_parameters <- function(seed) {
  set.seed(seed)
  list(
    objective = "regression",
    metric = "rmse",
    num_leaves = sample(c(15L, 31L, 63L, 127L), 1L),
    learning_rate = stats::runif(1L, 0.02, 0.15),
    feature_fraction = stats::runif(1L, 0.6, 1),
    bagging_fraction = stats::runif(1L, 0.6, 1),
    bagging_freq = sample(c(0L, 1L, 5L), 1L),
    min_data_in_leaf = sample(5L:50L, 1L),
    lambda_l1 = stats::runif(1L, 0, 1),
    lambda_l2 = stats::runif(1L, 0, 2),
    feature_pre_filter = FALSE,
    seed = as.integer(seed),
    num_threads = 1L,
    verbosity = -1L
  )
}

evaluate_lightgbm_parameter <- function(parameters, fold_cache, cfg) {
  metrics <- vector("list", length(fold_cache))
  best_iterations <- integer(length(fold_cache))
  errors <- character(length(fold_cache))
  for (fold in seq_along(fold_cache)) {
    item <- fold_cache[[fold]]
    dtrain <- lightgbm::lgb.Dataset(as.matrix(item$x_train), label = item$y_train)
    dvalid <- lightgbm::lgb.Dataset(as.matrix(item$x_validation), label = item$y_validation)
    fit <- tryCatch(
      lightgbm::lgb.train(
        params = parameters,
        data = dtrain,
        nrounds = as.integer(cfg$modeling$lightgbm_max_rounds),
        valids = list(validation = dvalid),
        early_stopping_rounds = as.integer(cfg$modeling$lightgbm_early_stopping_rounds),
        verbose = -1
      ),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      errors[fold] <- conditionMessage(fit)
      metrics[[fold]] <- data.frame(R2 = NA_real_, RMSE = NA_real_, MAE = NA_real_, N = length(item$y_validation))
      next
    }
    best_iterations[fold] <- fit$best_iter %||% as.integer(cfg$modeling$lightgbm_max_rounds)
    prediction <- as.numeric(stats::predict(fit, as.matrix(item$x_validation), num_iteration = best_iterations[fold]))
    metrics[[fold]] <- regression_metrics(item$y_validation, prediction)
  }
  metrics <- do.call(rbind, metrics)
  metrics$Fold <- seq_len(nrow(metrics))
  metrics$Model <- "LightGBM"
  metrics$Error <- errors
  metrics$Best_Iteration <- best_iterations
  valid <- is.finite(metrics$R2) & is.finite(metrics$RMSE) & is.finite(metrics$MAE)
  summary <- data.frame(
    Model = "LightGBM",
    Mean_R2 = if (any(valid)) mean(metrics$R2[valid]) else NA_real_,
    SD_R2 = if (sum(valid) > 1L) stats::sd(metrics$R2[valid]) else NA_real_,
    Mean_RMSE = if (any(valid)) mean(metrics$RMSE[valid]) else NA_real_,
    SD_RMSE = if (sum(valid) > 1L) stats::sd(metrics$RMSE[valid]) else NA_real_,
    Mean_MAE = if (any(valid)) mean(metrics$MAE[valid]) else NA_real_,
    SD_MAE = if (sum(valid) > 1L) stats::sd(metrics$MAE[valid]) else NA_real_,
    Successful_Folds = sum(valid),
    row.names = NULL
  )
  list(summary = summary, fold_metrics = metrics, best_iterations = best_iterations)
}

tune_lightgbm <- function(fold_cache, cfg) {
  require_packages("lightgbm")
  trials <- as.integer(cfg$modeling$lightgbm_trials)
  evaluations <- vector("list", trials)
  parameters <- vector("list", trials)
  for (i in seq_len(trials)) {
    cat("LightGBM trial", i, "of", trials, "\n")
    parameters[[i]] <- sample_lightgbm_parameters(as.integer(cfg$modeling$seed) + i)
    evaluations[[i]] <- evaluate_lightgbm_parameter(parameters[[i]], fold_cache, cfg)
  }
  tuning <- do.call(rbind, lapply(seq_len(trials), function(i) {
    cbind(
      Candidate = i,
      Parameters = paste(paste(names(parameters[[i]]), unlist(parameters[[i]]), sep = "="), collapse = "; "),
      evaluations[[i]]$summary[, setdiff(names(evaluations[[i]]$summary), "Model"), drop = FALSE]
    )
  }))
  eligible <- which(is.finite(tuning$Mean_R2) & tuning$Successful_Folds == length(fold_cache))
  if (!length(eligible)) stop("No valid LightGBM tuning candidate.", call. = FALSE)
  best <- eligible[order(-tuning$Mean_R2[eligible], tuning$Mean_RMSE[eligible], tuning$Mean_MAE[eligible])][1]
  best_rounds <- as.integer(round(stats::median(evaluations[[best]]$best_iterations[evaluations[[best]]$best_iterations > 0])))
  list(
    model_name = "LightGBM",
    best_tune = data.frame(
      Parameters = tuning$Parameters[best],
      nrounds = best_rounds,
      row.names = NULL
    ),
    best_parameters = parameters[[best]],
    best_nrounds = best_rounds,
    summary = evaluations[[best]]$summary,
    fold_metrics = evaluations[[best]]$fold_metrics,
    tuning_results = tuning
  )
}

fit_final_model <- function(model_result, x, y, cfg) {
  model_name <- model_result$model_name
  if (identical(model_name, "LightGBM")) {
    dataset <- lightgbm::lgb.Dataset(as.matrix(x), label = y)
    return(lightgbm::lgb.train(
      params = model_result$best_parameters,
      data = dataset,
      nrounds = as.integer(model_result$best_nrounds),
      verbose = -1
    ))
  }
  methods <- c(
    Random_Forest = "rf",
    XGBoost = "xgbTree",
    GBDT = "gbm",
    SVM_RBF = "svmRadial",
    Linear_Regression = "lm",
    Elastic_Net = "glmnet",
    MARS = "earth",
    KNN = "knn",
    GPR_Radial = "gaussprRadial"
  )
  extra <- switch(
    model_name,
    Random_Forest = list(ntree = as.integer(cfg$modeling$rf_final_trees), importance = TRUE),
    XGBoost = list(verbosity = 0, nthread = 1, lambda = 1, alpha = 0.5),
    GBDT = list(verbose = FALSE, distribution = "gaussian"),
    list()
  )
  fit_caret_once(methods[[model_name]], model_result$best_tune, x, y, extra)
}

compute_grouped_permutation_importance <- function(
    model,
    model_name,
    x,
    y,
    groups,
    repeats,
    bootstrap_repeats,
    seed) {
  groups <- lapply(groups, intersect, y = names(x))
  groups <- groups[vapply(groups, length, integer(1)) > 0L]
  baseline_prediction <- predict_fitted_model(model, model_name, x)
  baseline_rmse <- regression_metrics(y, baseline_prediction)$RMSE
  set.seed(seed)
  delta_matrix <- matrix(NA_real_, nrow = repeats, ncol = length(groups), dimnames = list(NULL, names(groups)))
  for (g in seq_along(groups)) {
    columns <- groups[[g]]
    for (r in seq_len(repeats)) {
      permuted <- x
      order_index <- sample.int(nrow(permuted))
      permuted[columns] <- permuted[order_index, columns, drop = FALSE]
      prediction <- predict_fitted_model(model, model_name, permuted)
      delta_matrix[r, g] <- regression_metrics(y, prediction)$RMSE - baseline_rmse
    }
  }
  mean_delta <- colMeans(delta_matrix, na.rm = TRUE)
  positive_delta <- pmax(mean_delta, 0)
  relative <- if (sum(positive_delta) > 0) 100 * positive_delta / sum(positive_delta) else rep(NA_real_, length(positive_delta))

  bootstrap_relative <- matrix(
    NA_real_,
    nrow = bootstrap_repeats,
    ncol = length(groups),
    dimnames = list(NULL, names(groups))
  )
  set.seed(seed + 1000L)
  for (b in seq_len(bootstrap_repeats)) {
    rows <- sample.int(nrow(x), size = nrow(x), replace = TRUE)
    xb <- x[rows, , drop = FALSE]
    yb <- y[rows]
    base <- regression_metrics(yb, predict_fitted_model(model, model_name, xb))$RMSE
    deltas <- numeric(length(groups))
    for (g in seq_along(groups)) {
      permuted <- xb
      permutation <- sample.int(nrow(xb))
      columns <- groups[[g]]
      permuted[columns] <- permuted[permutation, columns, drop = FALSE]
      deltas[g] <- max(0, regression_metrics(yb, predict_fitted_model(model, model_name, permuted))$RMSE - base)
    }
    if (sum(deltas) > 0) bootstrap_relative[b, ] <- 100 * deltas / sum(deltas)
  }
  lower <- apply(bootstrap_relative, 2, stats::quantile, probs = 0.025, na.rm = TRUE, names = FALSE)
  upper <- apply(bootstrap_relative, 2, stats::quantile, probs = 0.975, na.rm = TRUE, names = FALSE)
  data.frame(
    Predictor = names(groups),
    Baseline_RMSE = baseline_rmse,
    Mean_Delta_RMSE = unname(mean_delta),
    SD_Delta_RMSE = apply(delta_matrix, 2, stats::sd, na.rm = TRUE),
    Relative_Contribution_Percent = unname(relative),
    Bootstrap_95CI_Lower = unname(lower),
    Bootstrap_95CI_Upper = unname(upper),
    Permutation_Repeats = repeats,
    Bootstrap_Repeats = bootstrap_repeats,
    row.names = NULL
  )[order(-relative), ]
}

compute_grouped_shap <- function(
    model,
    model_name,
    training_x,
    test_x,
    test_original_predictors,
    groups,
    cfg) {
  require_packages("fastshap")
  set.seed(as.integer(cfg$modeling$seed) + 2000L)
  background_n <- min(nrow(training_x), as.integer(cfg$modeling$shap_background_sample_size))
  explain_n <- min(nrow(test_x), as.integer(cfg$modeling$shap_explain_sample_size))
  background_index <- sample.int(nrow(training_x), background_n)
  explain_index <- sample.int(nrow(test_x), explain_n)
  background <- training_x[background_index, , drop = FALSE]
  explain <- test_x[explain_index, , drop = FALSE]
  wrapper <- function(object, newdata) predict_fitted_model(object, model_name, newdata)
  shap <- fastshap::explain(
    object = model,
    X = as.data.frame(background, check.names = FALSE),
    pred_wrapper = wrapper,
    newdata = as.data.frame(explain, check.names = FALSE),
    nsim = as.integer(cfg$modeling$shap_nsim),
    adjust = TRUE,
    seed = as.integer(cfg$modeling$seed) + 2001L
  )
  shap <- as.matrix(shap)
  groups <- lapply(groups, intersect, y = colnames(shap))
  groups <- groups[vapply(groups, length, integer(1)) > 0L]
  grouped <- sapply(groups, function(columns) rowSums(shap[, columns, drop = FALSE]))
  if (is.null(dim(grouped))) grouped <- matrix(grouped, ncol = 1L, dimnames = list(NULL, names(groups)))
  importance <- data.frame(
    Predictor = colnames(grouped),
    Mean_Absolute_SHAP = colMeans(abs(grouped)),
    Mean_SHAP = colMeans(grouped),
    Explained_Test_Rows = nrow(grouped),
    Background_Training_Rows = nrow(background),
    SHAP_Simulations = as.integer(cfg$modeling$shap_nsim),
    row.names = NULL
  )
  importance <- importance[order(-importance$Mean_Absolute_SHAP), ]
  long <- do.call(rbind, lapply(colnames(grouped), function(predictor) {
    data.frame(
      Test_Row = explain_index,
      Predictor = predictor,
      Predictor_Value = as.character(test_original_predictors[[predictor]][explain_index]),
      SHAP_Value = grouped[, predictor],
      row.names = NULL,
      check.names = FALSE
    )
  }))
  list(
    encoded_shap = as.data.frame(shap, check.names = FALSE),
    grouped_shap = as.data.frame(grouped, check.names = FALSE),
    importance = importance,
    long = long,
    explain_index = explain_index,
    background_index = background_index
  )
}

run_model_search <- function(fold_cache, full_processed, cfg) {
  results <- list()
  failures <- list()
  requested <- cfg$modeling$models
  add_result <- function(name, expression) {
    cat("\n=====", name, "=====\n")
    value <- tryCatch(force(expression), error = function(e) e)
    if (inherits(value, "error")) {
      failures[[name]] <<- conditionMessage(value)
      cat("Skipped/failed:", failures[[name]], "\n")
    } else {
      results[[name]] <<- value
    }
  }

  if ("Random_Forest" %in% requested) add_result("Random_Forest", tune_random_forest(fold_cache, full_processed$x, cfg))
  if ("XGBoost" %in% requested) add_result("XGBoost", tune_xgboost(fold_cache, cfg))
  if ("LightGBM" %in% requested) add_result("LightGBM", tune_lightgbm(fold_cache, cfg))

  specifications <- list(
    GBDT = list(method = "gbm", grid = caret_generated_grid("gbm", full_processed$x, full_processed$y, 30L), extra = list(verbose = FALSE, distribution = "gaussian")),
    SVM_RBF = list(method = "svmRadial", grid = caret_generated_grid("svmRadial", full_processed$x, full_processed$y, 20L), extra = list()),
    Linear_Regression = list(method = "lm", grid = data.frame(intercept = TRUE), extra = list()),
    Elastic_Net = list(method = "glmnet", grid = expand.grid(alpha = seq(0, 1, by = 0.25), lambda = 10^seq(-3, 1, length.out = 20)), extra = list()),
    MARS = list(method = "earth", grid = expand.grid(nprune = seq(5L, 60L, by = 5L), degree = c(1L, 2L, 3L)), extra = list()),
    KNN = list(method = "knn", grid = caret_generated_grid("knn", full_processed$x, full_processed$y, 15L), extra = list()),
    GPR_Radial = list(method = "gaussprRadial", grid = caret_generated_grid("gaussprRadial", full_processed$x, full_processed$y, 10L), extra = list())
  )
  for (name in intersect(names(specifications), requested)) {
    specification <- specifications[[name]]
    add_result(
      name,
      evaluate_caret_grid(name, specification$method, specification$grid, fold_cache, specification$extra)
    )
  }
  list(results = results, failures = failures)
}

main <- function() {
  project_root <- script_root()
  source(file.path(project_root, "config", "analysis_config.R"), local = FALSE)
  source(file.path(project_root, "R", "preprocessing_helpers.R"), local = FALSE)
  require_packages(c(
    "readxl", "openxlsx", "mice", "caret", "randomForest", "glmnet",
    "earth", "kknn", "gbm", "kernlab", "ggplot2", "rBayesianOptimization"
  ))

  input_path <- resolve_project_path(CONFIG$input_file, project_root)
  output_root <- resolve_project_path(CONFIG$output_dir, project_root)
  output_dir <- file.path(output_root, "02_machine_learning")
  plot_dir <- file.path(output_dir, "plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  data <- read_analysis_data(input_path, CONFIG$input_sheet)
  data <- coerce_analysis_types(data, CONFIG)
  data <- exclude_application_cases(data, CONFIG)
  removed_cases <- attr(data, "application_cases_removed") %||% 0L
  assert_columns(
    data,
    unique(c(
      LDH_PREDICTORS, CONFIG$target_var, CONFIG$article_id_var,
      CONFIG$author_var, CONFIG$material_var
    )),
    "machine-learning input"
  )
  data <- data[is.finite(data[[CONFIG$target_var]]), , drop = FALSE]
  rownames(data) <- NULL

  split <- make_article_split(data, CONFIG)
  training <- data[split$train, , drop = FALSE]
  test <- data[split$test, , drop = FALSE]
  if (length(intersect(unique(training[[CONFIG$article_id_var]]), unique(test[[CONFIG$article_id_var]])))) {
    stop("Article leakage detected between training and test sets.", call. = FALSE)
  }

  validation_folds <- make_balanced_article_folds(training, CONFIG)
  fold_cache <- build_fold_cache(training, validation_folds, CONFIG)
  full_preprocessor <- fit_analysis_preprocessor(training, CONFIG)
  full_processed <- list(x = full_preprocessor$training_x, y = training[[CONFIG$target_var]])

  search <- run_model_search(fold_cache, full_processed, CONFIG)
  if (!length(search$results)) stop("All candidate models failed.", call. = FALSE)
  comparison <- do.call(rbind, lapply(search$results, function(result) result$summary))
  comparison <- comparison[order(-comparison$Mean_R2, comparison$Mean_RMSE, comparison$Mean_MAE), ]
  comparison$Rank <- seq_len(nrow(comparison))
  comparison <- comparison[, c("Rank", setdiff(names(comparison), "Rank"))]
  best_name <- comparison$Model[1]
  best_result <- search$results[[best_name]]
  cat("\nSelected by mean grouped-CV R2:", best_name, "\n")

  test_processed <- apply_analysis_preprocessor(full_preprocessor, test, CONFIG)
  set.seed(as.integer(CONFIG$modeling$seed) + 5000L)
  final_model <- fit_final_model(best_result, full_processed$x, full_processed$y, CONFIG)
  train_prediction <- predict_fitted_model(final_model, best_name, full_processed$x)
  test_prediction <- predict_fitted_model(final_model, best_name, test_processed$x)
  train_metrics <- cbind(Dataset = "Training", regression_metrics(full_processed$y, train_prediction))
  test_metrics <- cbind(Dataset = "Independent_Test", regression_metrics(test[[CONFIG$target_var]], test_prediction))
  final_metrics <- rbind(train_metrics, test_metrics)

  permutation <- compute_grouped_permutation_importance(
    final_model,
    best_name,
    test_processed$x,
    test[[CONFIG$target_var]],
    full_preprocessor$feature_map,
    repeats = as.integer(CONFIG$modeling$permutation_repeats),
    bootstrap_repeats = as.integer(CONFIG$modeling$importance_bootstrap_repeats),
    seed = as.integer(CONFIG$modeling$seed)
  )

  shap <- tryCatch(
    compute_grouped_shap(
      final_model,
      best_name,
      full_processed$x,
      test_processed$x,
      test_processed$imputed_predictors,
      full_preprocessor$feature_map,
      CONFIG
    ),
    error = function(e) {
      warning("SHAP analysis was not completed: ", conditionMessage(e))
      NULL
    }
  )

  fold_metrics <- rbind_fill(lapply(search$results, function(x) x$fold_metrics))
  tuning_results <- do.call(rbind, lapply(names(search$results), function(name) {
    table <- search$results[[name]]$tuning_results
    data.frame(Model = name, Parameters = if (nrow(table)) parameter_text(table) else character(0), row.names = NULL)
  }))
  failures <- if (length(search$failures)) {
    data.frame(Model = names(search$failures), Reason = unlist(search$failures), row.names = NULL)
  } else data.frame(Model = character(0), Reason = character(0))

  split_audit <- rbind(
    data.frame(Article_ID = split$training_articles, Partition = "Training", row.names = NULL),
    data.frame(Article_ID = split$test_articles, Partition = "Independent_Test", row.names = NULL)
  )
  fold_audit <- do.call(rbind, lapply(seq_along(validation_folds), function(fold) {
    data.frame(
      Article_ID = sort(unique(as.character(training[[CONFIG$article_id_var]][validation_folds[[fold]]]))),
      CV_Fold = fold,
      row.names = NULL
    )
  }))
  feature_audit <- data.frame(
    Feature = c(full_preprocessor$encoded_features, full_preprocessor$removed_nzv),
    Status = c(
      rep("Retained", length(full_preprocessor$encoded_features)),
      rep("Removed_Near_Zero_Variance", length(full_preprocessor$removed_nzv))
    ),
    row.names = NULL
  )
  predictions <- rbind(
    data.frame(Dataset = "Training", Observed = full_processed$y, Predicted = train_prediction),
    data.frame(Dataset = "Independent_Test", Observed = test[[CONFIG$target_var]], Predicted = test_prediction)
  )
  audit <- data.frame(
    Item = c(
      "Metal", "Target column", "Rows after case exclusion", "Application cases removed",
      "Training rows", "Test rows", "Training articles", "Test articles", "CV folds",
      "Model selection criterion", "Test-set use", "Permutation metric", "SHAP data",
      "Interpretation scope"
    ),
    Value = c(
      CONFIG$metal, CONFIG$target_var, nrow(data), removed_cases,
      nrow(training), nrow(test), length(split$training_articles), length(split$test_articles),
      CONFIG$modeling$cv_folds, "Highest mean article-grouped CV R2; RMSE/MAE complementary",
      "Evaluated once after model selection; also used for final permutation importance and SHAP",
      "Grouped normalized Delta RMSE with percentile bootstrap 95% CI; no p-value stars",
      "Held-out test observations; one-hot SHAP values summed to original predictors",
      "Predictive attribution only; not causal attribution"
    ),
    row.names = NULL
  )

  sheets <- list(
    CV_Model_Comparison = comparison,
    CV_Fold_Metrics = fold_metrics,
    Tuning_Audit = tuning_results,
    Failed_Models = failures,
    Final_Model_Metrics = final_metrics,
    Predictions = predictions,
    Permutation_Importance = permutation,
    Train_Test_Articles = split_audit,
    CV_Article_Folds = fold_audit,
    Feature_Audit = feature_audit,
    Analysis_Audit = audit
  )
  if (!is.null(shap)) {
    sheets$SHAP_Importance <- shap$importance
    sheets$SHAP_Grouped_Long <- shap$long
    sheets$SHAP_Grouped_Values <- shap$grouped_shap
  }
  output_xlsx <- file.path(output_dir, "machine_learning_results.xlsx")
  openxlsx::write.xlsx(sheets, output_xlsx, overwrite = TRUE)

  bundle <- list(
    metal = CONFIG$metal,
    target_column = CONFIG$target_var,
    best_model_name = best_name,
    best_tuning = best_result$best_tune,
    final_model = final_model,
    preprocessor = full_preprocessor,
    training_articles = split$training_articles,
    test_articles = split$test_articles,
    cv_article_folds = fold_audit,
    cv_comparison = comparison,
    independent_test_metrics = test_metrics,
    config = CONFIG,
    interpretation_note = "Permutation importance and SHAP are predictive attributions, not causal effects."
  )
  saveRDS(bundle, file.path(output_dir, "best_model_bundle.rds"))
  writeLines(session_information(), file.path(output_dir, "sessionInfo.txt"))

  p_cv <- ggplot2::ggplot(comparison, ggplot2::aes(x = stats::reorder(Model, Mean_R2), y = Mean_R2)) +
    ggplot2::geom_col(fill = "#4C78A8", width = 0.72) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Mean_R2 - SD_R2, ymax = Mean_R2 + SD_R2), width = 0.18) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Article-grouped CV R2 (mean +/- SD)") +
    ggplot2::theme_classic(base_size = 12)
  ggplot2::ggsave(file.path(plot_dir, "cv_model_comparison.pdf"), p_cv, width = 7.5, height = 5.5)
  ggplot2::ggsave(file.path(plot_dir, "cv_model_comparison.png"), p_cv, width = 7.5, height = 5.5, dpi = 300)

  test_plot <- predictions[predictions$Dataset == "Independent_Test", ]
  p_test <- ggplot2::ggplot(test_plot, ggplot2::aes(Observed, Predicted)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey45") +
    ggplot2::geom_point(color = "#D64F4F", alpha = 0.75, size = 2) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Observed IE", y = "Predicted IE", title = paste("Independent test:", best_name)) +
    ggplot2::theme_classic(base_size = 12)
  ggplot2::ggsave(file.path(plot_dir, "independent_test_predictions.pdf"), p_test, width = 5.5, height = 5.5)
  ggplot2::ggsave(file.path(plot_dir, "independent_test_predictions.png"), p_test, width = 5.5, height = 5.5, dpi = 300)

  p_perm <- ggplot2::ggplot(
    permutation,
    ggplot2::aes(x = stats::reorder(Predictor, Relative_Contribution_Percent), y = Relative_Contribution_Percent)
  ) +
    ggplot2::geom_col(fill = "#59A14F", width = 0.72) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = Bootstrap_95CI_Lower, ymax = Bootstrap_95CI_Upper),
      width = 0.18
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Relative contribution (%) with bootstrap 95% CI") +
    ggplot2::theme_classic(base_size = 12)
  ggplot2::ggsave(file.path(plot_dir, "permutation_importance.pdf"), p_perm, width = 7.5, height = 6.5)
  ggplot2::ggsave(file.path(plot_dir, "permutation_importance.png"), p_perm, width = 7.5, height = 6.5, dpi = 300)

  if (!is.null(shap)) {
    ordered <- shap$importance$Predictor
    shap$long$Predictor <- factor(shap$long$Predictor, levels = rev(ordered))
    p_shap <- ggplot2::ggplot(shap$long, ggplot2::aes(x = SHAP_Value, y = Predictor)) +
      ggplot2::geom_point(alpha = 0.45, size = 1.4, position = ggplot2::position_jitter(height = 0.18, width = 0)) +
      ggplot2::geom_vline(xintercept = 0, color = "grey55", linetype = 2) +
      ggplot2::labs(x = "Grouped SHAP value", y = NULL) +
      ggplot2::theme_classic(base_size = 12)
    ggplot2::ggsave(file.path(plot_dir, "global_shap.pdf"), p_shap, width = 8, height = 6.5)
    ggplot2::ggsave(file.path(plot_dir, "global_shap.png"), p_shap, width = 8, height = 6.5, dpi = 300)
  }

  cat("\nMachine-learning analysis completed.\n")
  cat("Best model:", best_name, "\n")
  cat("Independent test R2:", test_metrics$R2, "\n")
  cat("Output:", output_xlsx, "\n")
  invisible(bundle)
}

if (sys.nframe() == 0L) main()
