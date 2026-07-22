#!/usr/bin/env Rscript

# Favorable continuous domains (LOESS + IE-weighted KDE) and categorical
# enrichment. Parameters match Methods 2.5 and SI Text S4.

script_root <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    script <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
    return(normalizePath(file.path(dirname(script), ".."), mustWork = FALSE))
  }
  normalizePath(Sys.getenv("LDH_PROJECT_ROOT", unset = getwd()), mustWork = FALSE)
}

merge_intervals <- function(intervals) {
  intervals <- intervals[vapply(intervals, function(x) {
    length(x) == 2L && all(is.finite(x)) && x[1] >= 0 && x[1] <= x[2]
  }, logical(1))]
  if (!length(intervals)) return(list())
  matrix_intervals <- do.call(rbind, intervals)
  matrix_intervals <- matrix_intervals[order(matrix_intervals[, 1]), , drop = FALSE]
  merged <- list()
  current <- matrix_intervals[1, ]
  if (nrow(matrix_intervals) > 1L) {
    for (i in 2:nrow(matrix_intervals)) {
      if (matrix_intervals[i, 1] <= current[2]) {
        current[2] <- max(current[2], matrix_intervals[i, 2])
      } else {
        merged[[length(merged) + 1L]] <- as.numeric(current)
        current <- matrix_intervals[i, ]
      }
    }
  }
  merged[[length(merged) + 1L]] <- as.numeric(current)
  merged
}

format_intervals <- function(intervals, digits = 6L) {
  if (!length(intervals)) return(NA_character_)
  paste(
    vapply(intervals, function(x) {
      paste(format(round(x, digits), trim = TRUE, scientific = FALSE), collapse = "-")
    }, character(1)),
    collapse = "; "
  )
}

loess_domain <- function(x, y, span, peak_threshold) {
  data <- data.frame(x = as.numeric(x), y = as.numeric(y))
  data <- data[is.finite(data$x) & is.finite(data$y), , drop = FALSE]
  if (nrow(data) < 10L || length(unique(data$x)) < 4L) return(NULL)
  fit <- tryCatch(stats::loess(y ~ x, data = data, span = span), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  grid <- seq(min(data$x), max(data$x), length.out = 200L)
  prediction <- as.numeric(stats::predict(fit, newdata = data.frame(x = grid)))
  valid <- is.finite(grid) & is.finite(prediction)
  grid <- grid[valid]
  prediction <- prediction[valid]
  if (!length(prediction)) return(NULL)
  peak <- max(prediction)
  selected <- grid[prediction >= peak_threshold * peak]
  if (!length(selected)) return(NULL)
  list(
    peak_x = grid[which.max(prediction)],
    peak_ie = peak,
    lower = min(selected),
    upper = max(selected),
    curve = data.frame(x = grid, Predicted_IE = prediction)
  )
}

wkde_domain <- function(x, y, n, central_mass) {
  valid <- is.finite(x) & is.finite(y)
  x <- as.numeric(x[valid])
  weights <- pmax(0, as.numeric(y[valid]))
  if (length(x) < 2L || length(unique(x)) < 2L || sum(weights) <= 0) return(NULL)
  weights <- weights / sum(weights)
  estimate <- stats::density(x, weights = weights, n = as.integer(n), na.rm = TRUE)
  dx <- estimate$x[2] - estimate$x[1]
  cdf <- cumsum(estimate$y) * dx
  cdf <- cdf / max(cdf)
  alpha <- (1 - central_mass) / 2
  lower <- stats::approx(cdf, estimate$x, xout = alpha, ties = "ordered", rule = 2)$y
  upper <- stats::approx(cdf, estimate$x, xout = 1 - alpha, ties = "ordered", rule = 2)$y
  list(
    mode_x = estimate$x[which.max(estimate$y)],
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    curve = data.frame(x = estimate$x, Weighted_Density = estimate$y)
  )
}

final_continuous_domain <- function(x, y, loess_result, wkde_result) {
  named_intervals <- list()
  if (!is.null(loess_result)) named_intervals$LOESS <- c(loess_result$lower, loess_result$upper)
  if (!is.null(wkde_result)) named_intervals$WKDE <- c(wkde_result$lower, wkde_result$upper)
  merged <- merge_intervals(named_intervals)
  if (!length(merged)) return(NULL)

  in_union <- rep(FALSE, length(x))
  for (interval in merged) in_union <- in_union | (x >= interval[1] & x <= interval[2])
  valid <- is.finite(x) & is.finite(y) & in_union
  union_x <- x[valid]
  union_y <- y[valid]
  if (!length(union_y)) return(NULL)
  top_fraction <- if (length(union_y) < 30L) 0.40 else 0.25
  threshold <- as.numeric(stats::quantile(union_y, 1 - top_fraction, type = 7, na.rm = TRUE))
  selected <- union_y >= threshold
  top_x <- union_x[selected]
  top_y <- union_y[selected]
  top_intervals <- list()
  for (interval in merged) {
    values <- top_x[top_x >= interval[1] & top_x <= interval[2]]
    if (length(values)) top_intervals[[length(top_intervals) + 1L]] <- range(values)
  }
  list(
    union_intervals = merged,
    top_intervals = top_intervals,
    final_min = min(top_x),
    final_max = max(top_x),
    n_union = length(union_y),
    n_top = length(top_y),
    top_fraction = top_fraction,
    ie_threshold = threshold,
    ie_min = min(top_y),
    ie_q25 = as.numeric(stats::quantile(top_y, 0.25, type = 7)),
    ie_median = stats::median(top_y),
    ie_mean = mean(top_y),
    ie_q75 = as.numeric(stats::quantile(top_y, 0.75, type = 7)),
    ie_max = max(top_y),
    sources = paste(names(named_intervals), collapse = ",")
  )
}

analyze_continuous <- function(data, variable, target, cfg) {
  valid <- is.finite(data[[variable]]) & is.finite(data[[target]])
  x <- data[[variable]][valid]
  y <- data[[target]][valid]
  if (length(x) < as.integer(cfg$favorable_domains$min_numeric_n)) return(NULL)
  loess_result <- loess_domain(
    x,
    y,
    cfg$favorable_domains$loess_span,
    cfg$favorable_domains$loess_peak_threshold
  )
  wkde_result <- wkde_domain(
    x,
    y,
    cfg$favorable_domains$wkde_n,
    cfg$favorable_domains$wkde_central_mass
  )
  final <- final_continuous_domain(x, y, loess_result, wkde_result)
  if (is.null(final)) return(NULL)
  list(
    variable = variable,
    n = length(x),
    basic = c(Mean = mean(x), Median = stats::median(x), SD = stats::sd(x), Min = min(x), Max = max(x)),
    loess = loess_result,
    wkde = wkde_result,
    final = final
  )
}

analyze_categorical <- function(data, variable, target, cfg) {
  valid <- !is.na(data[[variable]]) & is.finite(data[[target]])
  pool <- data[valid, c(variable, target), drop = FALSE]
  n <- nrow(pool)
  if (n < as.integer(cfg$favorable_domains$min_categorical_n)) return(NULL)
  top_fraction <- if (n < 30L) 0.40 else 0.25
  threshold <- as.numeric(stats::quantile(pool[[target]], 1 - top_fraction, type = 7, na.rm = TRUE))
  high <- pool[pool[[target]] >= threshold, , drop = FALSE]
  levels <- unique(as.character(pool[[variable]]))
  table <- do.call(rbind, lapply(levels, function(level) {
    all_index <- as.character(pool[[variable]]) == level
    high_index <- as.character(high[[variable]]) == level
    p_all <- sum(all_index) / nrow(pool)
    p_high <- sum(high_index) / nrow(high)
    data.frame(
      Variable = variable,
      Category = level,
      N_All = sum(all_index),
      N_High_IE = sum(high_index),
      Proportion_All = p_all,
      Proportion_High_IE = p_high,
      Enrichment_Ratio = if (p_all > 0) p_high / p_all else NA_real_,
      Mean_IE_All = mean(pool[[target]][all_index]),
      Mean_IE_High = if (any(high_index)) mean(high[[target]][high_index]) else NA_real_,
      Enriched_ER_GT_1 = is.finite(p_high / p_all) && p_high / p_all > 1,
      row.names = NULL
    )
  }))
  table <- table[order(-table$Enrichment_Ratio), ]
  list(
    table = table,
    info = data.frame(
      Variable = variable,
      N_All = n,
      N_High_IE = nrow(high),
      Top_Fraction = top_fraction,
      IE_Threshold = threshold,
      row.names = NULL
    )
  )
}

main <- function() {
  project_root <- script_root()
  source(file.path(project_root, "config", "analysis_config.R"), local = FALSE)
  source(file.path(project_root, "R", "preprocessing_helpers.R"), local = FALSE)
  require_packages(c("readxl", "openxlsx", "ggplot2"))

  output_root <- resolve_project_path(CONFIG$output_dir, project_root)
  default_input <- file.path(output_root, "01_imputation", paste0(tolower(CONFIG$metal), "_imputed.xlsx"))
  configured_input <- CONFIG$favorable_domains$input_file
  input_path <- if (is.null(configured_input) || !nzchar(configured_input)) {
    default_input
  } else resolve_project_path(configured_input, project_root)
  output_dir <- file.path(output_root, "03_favorable_domains")
  plot_dir <- file.path(output_dir, "plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  data <- read_analysis_data(input_path, CONFIG$favorable_domains$input_sheet)
  data <- coerce_analysis_types(data, CONFIG)
  data <- exclude_application_cases(data, CONFIG)
  removed_cases <- attr(data, "application_cases_removed") %||% 0L
  assert_columns(data, c(LDH_PREDICTORS, CONFIG$target_var), "favorable-domain input")
  data <- data[is.finite(data[[CONFIG$target_var]]), , drop = FALSE]

  continuous <- lapply(LDH_NUMERIC_PREDICTORS, function(variable) {
    analyze_continuous(data, variable, CONFIG$target_var, CONFIG)
  })
  names(continuous) <- LDH_NUMERIC_PREDICTORS
  continuous <- continuous[!vapply(continuous, is.null, logical(1))]

  categorical <- lapply(LDH_CATEGORICAL_PREDICTORS, function(variable) {
    analyze_categorical(data, variable, CONFIG$target_var, CONFIG)
  })
  names(categorical) <- LDH_CATEGORICAL_PREDICTORS
  categorical <- categorical[!vapply(categorical, is.null, logical(1))]

  continuous_summary <- do.call(rbind, lapply(continuous, function(result) {
    final <- result$final
    data.frame(
      Variable = result$variable,
      N_Valid = result$n,
      LOESS_Peak_X = result$loess$peak_x %||% NA_real_,
      LOESS_Peak_IE = result$loess$peak_ie %||% NA_real_,
      LOESS_Lower = result$loess$lower %||% NA_real_,
      LOESS_Upper = result$loess$upper %||% NA_real_,
      WKDE_Mode_X = result$wkde$mode_x %||% NA_real_,
      WKDE_Lower = result$wkde$lower %||% NA_real_,
      WKDE_Upper = result$wkde$upper %||% NA_real_,
      Union_Intervals = format_intervals(final$union_intervals),
      Top_Intervals = format_intervals(final$top_intervals),
      Final_Optimal_Min = final$final_min,
      Final_Optimal_Max = final$final_max,
      N_Union = final$n_union,
      N_Top = final$n_top,
      Top_Fraction = final$top_fraction,
      IE_Threshold = final$ie_threshold,
      IE_Min = final$ie_min,
      IE_Q25 = final$ie_q25,
      IE_Median = final$ie_median,
      IE_Mean = final$ie_mean,
      IE_Q75 = final$ie_q75,
      IE_Max = final$ie_max,
      Sources = final$sources,
      row.names = NULL
    )
  }))
  categorical_table <- if (length(categorical)) do.call(rbind, lapply(categorical, `[[`, "table")) else data.frame()
  categorical_info <- if (length(categorical)) do.call(rbind, lapply(categorical, `[[`, "info")) else data.frame()

  audit <- data.frame(
    Item = c(
      "Metal", "Rows analyzed", "Application cases removed", "Continuous predictors",
      "Categorical predictors", "LOESS span", "LOESS peak fraction", "WKDE grid n",
      "WKDE central mass", "Top rule", "Response column"
    ),
    Value = c(
      CONFIG$metal, nrow(data), removed_cases, length(continuous), length(categorical),
      CONFIG$favorable_domains$loess_span, CONFIG$favorable_domains$loess_peak_threshold,
      CONFIG$favorable_domains$wkde_n, CONFIG$favorable_domains$wkde_central_mass,
      "Top 25% when N >= 30; Top 40% otherwise", CONFIG$target_var
    ),
    row.names = NULL
  )
  used_columns <- unique(c(
    intersect(c(CONFIG$article_id_var, CONFIG$author_var), names(data)),
    CONFIG$target_var,
    LDH_PREDICTORS
  ))
  output_xlsx <- file.path(output_dir, "favorable_domain_results.xlsx")
  openxlsx::write.xlsx(
    list(
      Continuous_Domains = continuous_summary,
      Categorical_Enrichment = categorical_table,
      Categorical_Info = categorical_info,
      Analysis_Audit = audit,
      Data_Used = data[used_columns]
    ),
    output_xlsx,
    overwrite = TRUE
  )

  for (variable in names(continuous)) {
    result <- continuous[[variable]]
    raw <- data[is.finite(data[[variable]]) & is.finite(data[[CONFIG$target_var]]), , drop = FALSE]
    if (!is.null(result$loess)) {
      curve <- result$loess$curve
      plot <- ggplot2::ggplot(raw, ggplot2::aes(x = .data[[variable]], y = .data[[CONFIG$target_var]])) +
        ggplot2::geom_point(color = "grey55", alpha = 0.25, size = 1.2) +
        ggplot2::geom_line(data = curve, ggplot2::aes(x = x, y = Predicted_IE), color = "#D62728", linewidth = 1) +
        ggplot2::geom_vline(xintercept = c(result$final$final_min, result$final$final_max), linetype = 2) +
        ggplot2::labs(x = variable, y = "IE") +
        ggplot2::theme_classic(base_size = 12)
      ggplot2::ggsave(file.path(plot_dir, paste0(variable, "_LOESS.pdf")), plot, width = 6.5, height = 5)
      ggplot2::ggsave(file.path(plot_dir, paste0(variable, "_LOESS.png")), plot, width = 6.5, height = 5, dpi = 300)
    }
    if (!is.null(result$wkde)) {
      curve <- result$wkde$curve
      plot <- ggplot2::ggplot(curve, ggplot2::aes(x = x, y = Weighted_Density)) +
        ggplot2::geom_area(fill = "#F28E2B", alpha = 0.28) +
        ggplot2::geom_line(color = "#F28E2B", linewidth = 1) +
        ggplot2::geom_vline(xintercept = c(result$final$final_min, result$final$final_max), linetype = 2) +
        ggplot2::labs(x = variable, y = "IE-weighted density") +
        ggplot2::theme_classic(base_size = 12)
      ggplot2::ggsave(file.path(plot_dir, paste0(variable, "_WKDE.pdf")), plot, width = 6.5, height = 5)
      ggplot2::ggsave(file.path(plot_dir, paste0(variable, "_WKDE.png")), plot, width = 6.5, height = 5, dpi = 300)
    }
  }

  for (variable in names(categorical)) {
    plot_data <- categorical[[variable]]$table
    plot_data$Category <- factor(plot_data$Category, levels = plot_data$Category[order(plot_data$Enrichment_Ratio)])
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Category, y = Enrichment_Ratio)) +
      ggplot2::geom_col(fill = "#4E79A7", width = 0.72) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Enrichment ratio") +
      ggplot2::theme_classic(base_size = 12)
    height <- max(4.5, 0.32 * nrow(plot_data) + 1.5)
    ggplot2::ggsave(file.path(plot_dir, paste0(variable, "_enrichment.pdf")), plot, width = 7, height = height)
    ggplot2::ggsave(file.path(plot_dir, paste0(variable, "_enrichment.png")), plot, width = 7, height = height, dpi = 300)
  }

  saveRDS(list(continuous = continuous, categorical = categorical, config = CONFIG), file.path(output_dir, "favorable_domain_objects.rds"))
  writeLines(session_information(), file.path(output_dir, "sessionInfo.txt"))
  cat("Favorable-domain analysis completed.\n")
  cat("Output:", output_xlsx, "\n")
}

if (sys.nframe() == 0L) main()

