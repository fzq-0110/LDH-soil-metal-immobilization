#!/usr/bin/env Rscript

# Supplementary Boruta feature-relevance sensitivity analysis corresponding to
# SI Figures S3-S5. This does not replace predictive model selection.

script_root <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    script <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
    return(normalizePath(file.path(dirname(script), ".."), mustWork = FALSE))
  }
  normalizePath(Sys.getenv("LDH_PROJECT_ROOT", unset = getwd()), mustWork = FALSE)
}

main <- function() {
  project_root <- script_root()
  source(file.path(project_root, "config", "analysis_config.R"), local = FALSE)
  source(file.path(project_root, "R", "preprocessing_helpers.R"), local = FALSE)
  require_packages(c("readxl", "openxlsx", "Boruta"))

  output_root <- resolve_project_path(CONFIG$output_dir, project_root)
  default_input <- file.path(output_root, "01_imputation", paste0(tolower(CONFIG$metal), "_imputed.xlsx"))
  configured_input <- CONFIG$boruta$input_file
  input_path <- if (is.null(configured_input) || !nzchar(configured_input)) {
    default_input
  } else resolve_project_path(configured_input, project_root)
  output_dir <- file.path(output_root, "04_boruta_sensitivity")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  data <- read_analysis_data(input_path, CONFIG$boruta$input_sheet)
  data <- coerce_analysis_types(data, CONFIG)
  data <- exclude_application_cases(data, CONFIG)
  removed_cases <- attr(data, "application_cases_removed") %||% 0L
  assert_columns(data, c(LDH_PREDICTORS, CONFIG$target_var), "Boruta input")
  data <- data[is.finite(data[[CONFIG$target_var]]), , drop = FALSE]

  x <- data[LDH_PREDICTORS]
  for (variable in LDH_CATEGORICAL_PREDICTORS) x[[variable]] <- factor(x[[variable]])
  complete <- stats::complete.cases(x) & is.finite(data[[CONFIG$target_var]])
  x <- x[complete, , drop = FALSE]
  y <- data[[CONFIG$target_var]][complete]
  if (!nrow(x)) stop("No complete observations remain for Boruta.", call. = FALSE)

  set.seed(as.integer(CONFIG$boruta$seed))
  fit <- Boruta::Boruta(
    x = x,
    y = y,
    maxRuns = as.integer(CONFIG$boruta$max_runs),
    pValue = CONFIG$boruta$p_value,
    mcAdj = isTRUE(CONFIG$boruta$mc_adj),
    doTrace = 1L,
    holdHistory = TRUE
  )
  statistics <- Boruta::attStats(fit)
  statistics$Predictor <- rownames(statistics)
  rownames(statistics) <- NULL
  statistics <- statistics[, c("Predictor", setdiff(names(statistics), "Predictor"))]
  statistics <- statistics[order(statistics$decision, -statistics$medianImp), ]

  importance_history <- as.data.frame(fit$ImpHistory, check.names = FALSE)
  importance_history$Iteration <- seq_len(nrow(importance_history))
  audit <- data.frame(
    Item = c(
      "Metal", "Rows analyzed", "Application cases removed", "Predictor count",
      "Response column", "Seed", "maxRuns", "pValue", "mcAdj",
      "Role in study"
    ),
    Value = c(
      CONFIG$metal, nrow(x), removed_cases, ncol(x), CONFIG$target_var,
      CONFIG$boruta$seed, CONFIG$boruta$max_runs, CONFIG$boruta$p_value,
      CONFIG$boruta$mc_adj, "Feature-relevance sensitivity analysis only"
    ),
    row.names = NULL
  )

  output_xlsx <- file.path(output_dir, "boruta_results.xlsx")
  openxlsx::write.xlsx(
    list(
      Attribute_Statistics = statistics,
      Importance_History = importance_history,
      Analysis_Audit = audit
    ),
    output_xlsx,
    overwrite = TRUE
  )
  saveRDS(fit, file.path(output_dir, "boruta_model.rds"))

  grDevices::pdf(file.path(output_dir, "boruta_importance.pdf"), width = 11, height = 7)
  graphics::plot(fit, las = 2, xlab = "", main = paste(CONFIG$metal, "Boruta sensitivity analysis"))
  grDevices::dev.off()
  grDevices::png(file.path(output_dir, "boruta_importance.png"), width = 3300, height = 2100, res = 300)
  graphics::plot(fit, las = 2, xlab = "", main = paste(CONFIG$metal, "Boruta sensitivity analysis"))
  grDevices::dev.off()
  writeLines(session_information(), file.path(output_dir, "sessionInfo.txt"))

  cat("Boruta sensitivity analysis completed.\n")
  cat("Output:", output_xlsx, "\n")
}

if (sys.nframe() == 0L) main()

