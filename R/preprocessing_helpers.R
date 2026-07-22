# Shared preprocessing utilities for the LDH immobilization analyses.
# The central rule is that every fitted quantity is learned from training data
# only and is then applied unchanged to validation/test data.

LDH_PREDICTORS <- c(
  "Synthesis_Method",
  "Intercalated_Ions",
  "Composite_Material_Type",
  "Divalent_Cation",
  "Trivalent_Cation",
  "Divalent_Trivalent",
  "Interlayer_Anion_Type",
  "Specific_Surface_Area",
  "Pore_Volume",
  "Average_Pore_Diameter",
  "Interlayer_Spacing",
  "Material_Concentration",
  "Initial_Concentration",
  "Initial_pH",
  "Experimental_Temperature",
  "Competing_Ions",
  "Reaction_Time",
  "Soil_Moisture"
)

LDH_CATEGORICAL_PREDICTORS <- c(
  "Synthesis_Method",
  "Intercalated_Ions",
  "Composite_Material_Type",
  "Divalent_Cation",
  "Trivalent_Cation",
  "Interlayer_Anion_Type",
  "Competing_Ions"
)

LDH_NUMERIC_PREDICTORS <- setdiff(LDH_PREDICTORS, LDH_CATEGORICAL_PREDICTORS)

`%||%` <- function(x, y) if (is.null(x)) y else x

resolve_project_path <- function(path, project_root) {
  if (grepl("^(?:[A-Za-z]:[\\\\/]|/)", path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(project_root, path), winslash = "/", mustWork = FALSE)
}

require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing required R packages: ", paste(missing, collapse = ", "),
      ". Install them before running this script.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_columns <- function(data, columns, context = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      context, " is missing required columns: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

read_analysis_data <- function(path, sheet = "Sheet1") {
  if (!file.exists(path)) stop("Input file does not exist: ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    require_packages("readxl")
    return(as.data.frame(readxl::read_excel(path, sheet = sheet), check.names = FALSE))
  }
  if (ext == "csv") return(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE))
  if (ext == "tsv") return(utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE))
  stop("Unsupported input type: .", ext, call. = FALSE)
}

normalize_missing_tokens <- function(data) {
  missing_tokens <- c("", "na", "n/a", "nan", "null", "none", "missing", "-")
  for (nm in names(data)) {
    if (is.factor(data[[nm]])) data[[nm]] <- as.character(data[[nm]])
    if (is.character(data[[nm]])) {
      x <- trimws(data[[nm]])
      x[tolower(x) %in% missing_tokens] <- NA_character_
      data[[nm]] <- x
    }
  }
  data
}

coerce_analysis_types <- function(data, cfg) {
  data <- normalize_missing_tokens(data)
  present_numeric <- intersect(LDH_NUMERIC_PREDICTORS, names(data))
  for (nm in present_numeric) {
    data[[nm]] <- suppressWarnings(as.numeric(as.character(data[[nm]])))
  }
  if (cfg$target_var %in% names(data)) {
    data[[cfg$target_var]] <- suppressWarnings(as.numeric(as.character(data[[cfg$target_var]])))
  }
  present_cat <- intersect(
    unique(c(LDH_CATEGORICAL_PREDICTORS, cfg$author_var, cfg$material_var)),
    names(data)
  )
  for (nm in present_cat) data[[nm]] <- as.character(data[[nm]])
  data
}

mode_value <- function(x, fallback = "Unknown") {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(fallback)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

default_categorical_value <- function(variable) {
  defaults <- c(
    Material = "Unknown",
    Synthesis_Method = "Unknown",
    Divalent_Cation = "Mg",
    Trivalent_Cation = "Al"
  )
  if (variable %in% names(defaults)) unname(defaults[[variable]]) else "Unknown"
}

default_numeric_value <- function(variable) {
  defaults <- c(
    Initial_pH = 7,
    Experimental_Temperature = 25,
    Divalent_Trivalent = 2
  )
  if (variable %in% names(defaults)) unname(defaults[[variable]]) else 0
}

exclude_application_cases <- function(data, cfg) {
  if (isTRUE(cfg$input_already_excludes_application_cases)) {
    attr(data, "application_cases_removed") <- 0L
    return(data)
  }

  remove <- rep(FALSE, nrow(data))
  ids <- as.character(cfg$application_case_article_ids %||% character(0))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids)) {
    assert_columns(data, cfg$article_id_var, "application-case exclusion")
    remove <- remove | as.character(data[[cfg$article_id_var]]) %in% ids
  }

  flag_col <- cfg$application_case_flag_column %||% ""
  if (nzchar(flag_col) && flag_col %in% names(data)) {
    flag_values <- tolower(as.character(cfg$application_case_flag_values))
    remove <- remove | tolower(trimws(as.character(data[[flag_col]]))) %in% flag_values
  }

  if (!length(ids) && !(nzchar(flag_col) && flag_col %in% names(data))) {
    stop(
      "The input is declared to contain application cases, but no usable case IDs or flag column were supplied.",
      call. = FALSE
    )
  }
  out <- data[!remove, , drop = FALSE]
  attr(out, "application_cases_removed") <- sum(remove)
  rownames(out) <- NULL
  out
}

missing_summary <- function(data) {
  counts <- vapply(data, function(x) sum(is.na(x)), integer(1))
  data.frame(
    Variable = names(counts),
    Missing_Count = unname(counts),
    Missing_Percent = if (nrow(data)) 100 * unname(counts) / nrow(data) else NA_real_,
    Data_Type = vapply(data, function(x) class(x)[1], character(1)),
    row.names = NULL,
    check.names = FALSE
  )
}

group_key <- function(data, groups) {
  if (!length(groups)) return(rep("__ALL__", nrow(data)))
  do.call(paste, c(lapply(data[groups], function(x) ifelse(is.na(x), "__NA__", as.character(x))), sep = "\u241f"))
}

fit_group_statistic <- function(data, target, groups) {
  key <- group_key(data, groups)
  values <- data[[target]]
  split_values <- split(values, key, drop = TRUE)
  statistic <- vapply(split_values, function(z) {
    z <- z[is.finite(z)]
    if (!length(z)) return(NA_real_)
    med <- stats::median(z)
    if (is.finite(med)) med else mean(z)
  }, numeric(1))
  statistic <- statistic[is.finite(statistic)]
  list(target = target, groups = groups, statistic = statistic)
}

apply_group_statistic <- function(data, specification) {
  target <- specification$target
  if (!target %in% names(data)) return(data)
  miss <- is.na(data[[target]]) | !is.finite(data[[target]])
  if (!any(miss) || !length(specification$statistic)) return(data)
  key <- group_key(data, specification$groups)
  replacement <- unname(specification$statistic[key])
  usable <- miss & is.finite(replacement)
  data[[target]][usable] <- replacement[usable]
  data
}

apply_categorical_imputation <- function(data, modes) {
  for (nm in intersect(names(modes), names(data))) {
    x <- as.character(data[[nm]])
    miss <- is.na(x) | !nzchar(trimws(x))
    x[miss] <- modes[[nm]]
    data[[nm]] <- x
  }
  data
}

safe_mice_complete <- function(numeric_data, imputation_cfg) {
  if (!anyNA(numeric_data)) {
    return(list(data = numeric_data, mids = NULL, used_mice = FALSE, warning = NULL))
  }
  require_packages("mice")

  method <- rep("pmm", ncol(numeric_data))
  names(method) <- names(numeric_data)
  predictor_matrix <- mice::make.predictorMatrix(numeric_data)
  diag(predictor_matrix) <- 0

  unusable <- vapply(numeric_data, function(x) {
    observed <- x[is.finite(x)]
    length(observed) < 2L || stats::sd(observed) == 0
  }, logical(1))
  method[unusable] <- ""
  predictor_matrix[, unusable] <- 0

  result <- tryCatch(
    {
      fit <- mice::mice(
        numeric_data,
        method = method,
        predictorMatrix = predictor_matrix,
        m = as.integer(imputation_cfg$mice_m),
        maxit = as.integer(imputation_cfg$mice_maxit),
        seed = as.integer(imputation_cfg$mice_seed),
        printFlag = FALSE
      )
      list(
        data = as.data.frame(mice::complete(fit, action = 1L), check.names = FALSE),
        mids = fit,
        used_mice = TRUE,
        warning = NULL
      )
    },
    error = function(e) list(
      data = numeric_data,
      mids = NULL,
      used_mice = FALSE,
      warning = conditionMessage(e)
    )
  )
  result
}

fit_ridge_pmm_model <- function(completed_numeric, target, donors = 5L) {
  others <- setdiff(names(completed_numeric), target)
  y <- as.numeric(completed_numeric[[target]])
  if (!length(others) || length(unique(y[is.finite(y)])) < 2L) {
    return(list(
      target = target,
      others = character(0),
      center = numeric(0),
      scale = numeric(0),
      coefficients = mean(y, na.rm = TRUE),
      donor_predictions = rep(mean(y, na.rm = TRUE), length(y)),
      donor_values = y,
      donors = as.integer(donors)
    ))
  }

  x <- as.matrix(completed_numeric[others])
  center <- colMeans(x)
  scale <- apply(x, 2, stats::sd)
  scale[!is.finite(scale) | scale == 0] <- 1
  xz <- sweep(sweep(x, 2, center, "-"), 2, scale, "/")
  design <- cbind(`(Intercept)` = 1, xz)
  penalty <- diag(c(0, rep(1e-8, ncol(xz))))
  coefficients <- tryCatch(
    solve(crossprod(design) + penalty, crossprod(design, y)),
    error = function(e) qr.solve(crossprod(design) + penalty, crossprod(design, y))
  )
  donor_predictions <- as.numeric(design %*% coefficients)
  list(
    target = target,
    others = others,
    center = center,
    scale = scale,
    coefficients = coefficients,
    donor_predictions = donor_predictions,
    donor_values = y,
    donors = as.integer(donors)
  )
}

predict_pmm_means <- function(model, new_numeric, global_values) {
  if (!length(model$others)) return(rep(as.numeric(model$coefficients), nrow(new_numeric)))
  x <- as.matrix(new_numeric[model$others])
  for (j in seq_along(model$others)) {
    bad <- !is.finite(x[, j])
    x[bad, j] <- global_values[[model$others[j]]]
  }
  xz <- sweep(sweep(x, 2, model$center, "-"), 2, model$scale, "/")
  as.numeric(cbind(`(Intercept)` = 1, xz) %*% model$coefficients)
}

apply_out_of_sample_pmm <- function(data, models, global_values, seed) {
  for (i in seq_along(models)) {
    model <- models[[i]]
    target <- model$target
    miss <- is.na(data[[target]]) | !is.finite(data[[target]])
    if (!any(miss)) next
    predicted <- predict_pmm_means(model, data, global_values)
    valid_donors <- which(is.finite(model$donor_predictions) & is.finite(model$donor_values))
    if (!length(valid_donors)) next
    set.seed(as.integer(seed) + i)
    for (row in which(miss)) {
      distance <- abs(model$donor_predictions[valid_donors] - predicted[row])
      nearest <- valid_donors[order(distance)[seq_len(min(model$donors, length(valid_donors)))]]
      selected <- sample(nearest, size = 1L)
      data[[target]][row] <- model$donor_values[selected]
    }
  }
  data
}

fit_hierarchical_imputer <- function(training_data, cfg) {
  assert_columns(training_data, LDH_PREDICTORS, "imputation training data")
  assert_columns(training_data, c(cfg$author_var, cfg$material_var), "imputation grouping data")
  training_data <- coerce_analysis_types(training_data, cfg)

  support_columns <- unique(c(LDH_PREDICTORS, cfg$author_var, cfg$material_var))
  support <- training_data[support_columns]
  categorical_support <- intersect(
    unique(c(LDH_CATEGORICAL_PREDICTORS, cfg$author_var, cfg$material_var)),
    names(support)
  )
  modes <- setNames(vector("list", length(categorical_support)), categorical_support)
  for (nm in categorical_support) {
    modes[[nm]] <- mode_value(support[[nm]], default_categorical_value(nm))
  }
  support <- apply_categorical_imputation(support, modes)

  group_specs <- list()
  physical <- c("Specific_Surface_Area", "Pore_Volume", "Average_Pore_Diameter", "Interlayer_Spacing")
  experimental <- c(
    "Material_Concentration", "Initial_Concentration", "Initial_pH",
    "Experimental_Temperature", "Reaction_Time", "Soil_Moisture"
  )
  for (nm in physical) {
    group_specs[[nm]] <- fit_group_statistic(support, nm, c(cfg$material_var, "Synthesis_Method"))
    support <- apply_group_statistic(support, group_specs[[nm]])
  }
  for (nm in experimental) {
    group_specs[[nm]] <- fit_group_statistic(support, nm, cfg$author_var)
    support <- apply_group_statistic(support, group_specs[[nm]])
  }
  group_specs[["Divalent_Trivalent"]] <- fit_group_statistic(
    support,
    "Divalent_Trivalent",
    c("Divalent_Cation", "Trivalent_Cation")
  )
  support <- apply_group_statistic(support, group_specs[["Divalent_Trivalent"]])

  numeric_before_mice <- as.data.frame(support[LDH_NUMERIC_PREDICTORS], check.names = FALSE)
  mice_result <- safe_mice_complete(numeric_before_mice, cfg$imputation)
  completed_numeric <- mice_result$data

  global_values <- vapply(LDH_NUMERIC_PREDICTORS, function(nm) {
    z <- completed_numeric[[nm]]
    value <- stats::median(z[is.finite(z)], na.rm = TRUE)
    if (is.finite(value)) value else default_numeric_value(nm)
  }, numeric(1))
  for (nm in LDH_NUMERIC_PREDICTORS) {
    bad <- is.na(completed_numeric[[nm]]) | !is.finite(completed_numeric[[nm]])
    completed_numeric[[nm]][bad] <- global_values[[nm]]
  }
  support[LDH_NUMERIC_PREDICTORS] <- completed_numeric

  pmm_models <- lapply(
    LDH_NUMERIC_PREDICTORS,
    function(nm) fit_ridge_pmm_model(
      completed_numeric,
      nm,
      donors = as.integer(cfg$imputation$pmm_donors)
    )
  )
  names(pmm_models) <- LDH_NUMERIC_PREDICTORS

  list(
    modes = modes,
    group_specs = group_specs,
    global_values = global_values,
    pmm_models = pmm_models,
    support_columns = support_columns,
    training_data = support,
    mice_fit = mice_result$mids,
    mice_used = mice_result$used_mice,
    mice_warning = mice_result$warning,
    seed = as.integer(cfg$imputation$mice_seed)
  )
}

apply_hierarchical_imputer <- function(imputer, new_data, cfg) {
  assert_columns(new_data, imputer$support_columns, "new imputation data")
  new_data <- coerce_analysis_types(new_data, cfg)
  support <- new_data[imputer$support_columns]
  support <- apply_categorical_imputation(support, imputer$modes)
  for (specification in imputer$group_specs) {
    support <- apply_group_statistic(support, specification)
  }
  numeric_block <- as.data.frame(support[LDH_NUMERIC_PREDICTORS], check.names = FALSE)
  numeric_block <- apply_out_of_sample_pmm(
    numeric_block,
    imputer$pmm_models,
    imputer$global_values,
    imputer$seed
  )
  for (nm in LDH_NUMERIC_PREDICTORS) {
    bad <- is.na(numeric_block[[nm]]) | !is.finite(numeric_block[[nm]])
    numeric_block[[nm]][bad] <- imputer$global_values[[nm]]
  }
  support[LDH_NUMERIC_PREDICTORS] <- numeric_block
  support
}

fit_rare_level_maps <- function(data, categorical, min_count, min_fraction) {
  maps <- list()
  for (nm in categorical) {
    x <- as.character(data[[nm]])
    threshold <- max(as.integer(min_count), ceiling(as.numeric(min_fraction) * length(x)))
    tab <- sort(table(x), decreasing = TRUE)
    kept <- names(tab)[tab >= threshold]
    if (!length(kept) && length(tab)) kept <- names(tab)[1]
    maps[[nm]] <- list(
      kept = kept,
      levels = unique(c(sort(kept), "Other")),
      threshold = threshold
    )
  }
  maps
}

apply_rare_level_maps <- function(data, maps) {
  for (nm in names(maps)) {
    x <- as.character(data[[nm]])
    x[!x %in% maps[[nm]]$kept] <- "Other"
    data[[nm]] <- factor(x, levels = maps[[nm]]$levels)
  }
  data
}

fit_analysis_preprocessor <- function(training_data, cfg) {
  require_packages("caret")
  imputer <- fit_hierarchical_imputer(training_data, cfg)
  model_data <- imputer$training_data[LDH_PREDICTORS]
  rare_maps <- fit_rare_level_maps(
    model_data,
    LDH_CATEGORICAL_PREDICTORS,
    cfg$modeling$rare_level_min_count,
    cfg$modeling$rare_level_min_fraction
  )
  model_data <- apply_rare_level_maps(model_data, rare_maps)

  model_terms <- stats::terms(stats::reformulate(LDH_PREDICTORS), data = model_data)
  matrix_raw <- stats::model.matrix(model_terms, data = model_data)
  assignment <- attr(matrix_raw, "assign")
  contrasts_used <- attr(matrix_raw, "contrasts")
  keep_no_intercept <- colnames(matrix_raw) != "(Intercept)"
  matrix_raw <- matrix_raw[, keep_no_intercept, drop = FALSE]
  assignment <- assignment[keep_no_intercept]

  nzv_metrics <- caret::nearZeroVar(matrix_raw, saveMetrics = TRUE)
  removed_nzv <- rownames(nzv_metrics)[nzv_metrics$nzv]
  kept_features <- setdiff(colnames(matrix_raw), removed_nzv)
  if (!length(kept_features)) stop("All encoded predictors were removed as near-zero variance.", call. = FALSE)
  matrix_raw <- matrix_raw[, kept_features, drop = FALSE]

  center <- colMeans(matrix_raw)
  scale <- apply(matrix_raw, 2, stats::sd)
  scale[!is.finite(scale) | scale == 0] <- 1
  matrix_scaled <- sweep(sweep(matrix_raw, 2, center, "-"), 2, scale, "/")

  term_labels <- attr(model_terms, "term.labels")
  feature_map <- setNames(vector("list", length(term_labels)), term_labels)
  for (i in seq_along(term_labels)) {
    feature_map[[i]] <- intersect(colnames(matrix_raw), colnames(stats::model.matrix(model_terms, data = model_data))[attr(stats::model.matrix(model_terms, data = model_data), "assign") == i])
  }
  feature_map <- feature_map[vapply(feature_map, length, integer(1)) > 0L]

  list(
    imputer = imputer,
    rare_maps = rare_maps,
    model_terms = model_terms,
    contrasts = contrasts_used,
    encoded_features = colnames(matrix_raw),
    removed_nzv = removed_nzv,
    center = center,
    scale = scale,
    feature_map = feature_map,
    training_x = as.data.frame(matrix_scaled, check.names = FALSE),
    training_imputed_predictors = model_data
  )
}

apply_analysis_preprocessor <- function(preprocessor, new_data, cfg) {
  support <- apply_hierarchical_imputer(preprocessor$imputer, new_data, cfg)
  model_data <- support[LDH_PREDICTORS]
  model_data <- apply_rare_level_maps(model_data, preprocessor$rare_maps)
  matrix_raw <- stats::model.matrix(
    preprocessor$model_terms,
    data = model_data,
    contrasts.arg = preprocessor$contrasts
  )
  if ("(Intercept)" %in% colnames(matrix_raw)) {
    matrix_raw <- matrix_raw[, colnames(matrix_raw) != "(Intercept)", drop = FALSE]
  }
  missing_features <- setdiff(preprocessor$encoded_features, colnames(matrix_raw))
  if (length(missing_features)) {
    zeros <- matrix(0, nrow = nrow(matrix_raw), ncol = length(missing_features), dimnames = list(NULL, missing_features))
    matrix_raw <- cbind(matrix_raw, zeros)
  }
  matrix_raw <- matrix_raw[, preprocessor$encoded_features, drop = FALSE]
  matrix_scaled <- sweep(sweep(matrix_raw, 2, preprocessor$center, "-"), 2, preprocessor$scale, "/")
  list(
    x = as.data.frame(matrix_scaled, check.names = FALSE),
    imputed_predictors = model_data,
    imputed_support = support
  )
}

impute_entire_analysis_dataset <- function(data, cfg) {
  imputer <- fit_hierarchical_imputer(data, cfg)
  imputed <- data
  imputed[imputer$support_columns] <- imputer$training_data
  list(data = imputed, imputer = imputer)
}

make_article_split <- function(data, cfg) {
  assert_columns(data, c(cfg$article_id_var, cfg$target_var), "article-grouped split")
  article <- as.character(data[[cfg$article_id_var]])
  if (anyNA(article) || any(!nzchar(article))) stop("Article ID contains missing/blank values.", call. = FALSE)
  articles <- unique(article)
  if (length(articles) < 3L) stop("At least three articles are required for a grouped train/test split.", call. = FALSE)
  set.seed(as.integer(cfg$modeling$seed))
  train_n <- min(length(articles) - 1L, max(1L, floor(cfg$modeling$train_ratio * length(articles))))
  training_articles <- sample(articles, size = train_n, replace = FALSE)
  list(
    train = which(article %in% training_articles),
    test = which(!article %in% training_articles),
    training_articles = sort(training_articles),
    test_articles = sort(setdiff(articles, training_articles))
  )
}

make_balanced_article_folds <- function(data, cfg) {
  article <- as.character(data[[cfg$article_id_var]])
  article_sizes <- sort(table(article), decreasing = TRUE)
  k <- as.integer(cfg$modeling$cv_folds)
  if (length(article_sizes) < k) {
    stop("The training set has fewer articles (", length(article_sizes), ") than cv_folds (", k, ").", call. = FALSE)
  }
  set.seed(as.integer(cfg$modeling$seed) + 1L)
  tie_noise <- stats::runif(length(article_sizes))
  ordered_articles <- names(article_sizes)[order(-as.numeric(article_sizes), tie_noise)]
  fold_load <- rep(0L, k)
  article_fold <- setNames(integer(length(ordered_articles)), ordered_articles)
  for (id in ordered_articles) {
    lightest <- which(fold_load == min(fold_load))
    selected <- sample(lightest, 1L)
    article_fold[[id]] <- selected
    fold_load[selected] <- fold_load[selected] + as.integer(article_sizes[[id]])
  }
  lapply(seq_len(k), function(fold) which(article_fold[article] == fold))
}

r2_sse <- function(observed, predicted) {
  observed <- as.numeric(observed)
  predicted <- as.numeric(predicted)
  ok <- is.finite(observed) & is.finite(predicted)
  observed <- observed[ok]
  predicted <- predicted[ok]
  if (length(observed) < 2L) return(NA_real_)
  denominator <- sum((observed - mean(observed))^2)
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  1 - sum((observed - predicted)^2) / denominator
}

regression_metrics <- function(observed, predicted) {
  ok <- is.finite(observed) & is.finite(predicted)
  observed <- as.numeric(observed[ok])
  predicted <- as.numeric(predicted[ok])
  data.frame(
    R2 = r2_sse(observed, predicted),
    RMSE = sqrt(mean((observed - predicted)^2)),
    MAE = mean(abs(observed - predicted)),
    N = length(observed),
    row.names = NULL
  )
}

session_information <- function() {
  paste(capture.output(utils::sessionInfo()), collapse = "\n")
}

