#!/usr/bin/env Rscript

# Standalone hierarchical imputation for the full metal-specific analysis set.
# This output is intended for favorable-domain and Boruta analyses. The machine-
# learning script deliberately starts from the raw incomplete data and repeats
# imputation inside each training fold to prevent leakage.

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
  require_packages(c("readxl", "openxlsx", "mice"))

  input_path <- resolve_project_path(CONFIG$input_file, project_root)
  output_root <- resolve_project_path(CONFIG$output_dir, project_root)
  output_dir <- file.path(output_root, "01_imputation")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  raw <- read_analysis_data(input_path, CONFIG$input_sheet)
  raw <- coerce_analysis_types(raw, CONFIG)
  raw <- exclude_application_cases(raw, CONFIG)
  removed_cases <- attr(raw, "application_cases_removed") %||% 0L
  assert_columns(
    raw,
    unique(c(LDH_PREDICTORS, CONFIG$target_var, CONFIG$author_var, CONFIG$material_var)),
    "imputation input"
  )

  before <- missing_summary(raw)
  result <- impute_entire_analysis_dataset(raw, CONFIG)
  imputed <- result$data
  after <- missing_summary(imputed)

  validation <- do.call(rbind, lapply(LDH_NUMERIC_PREDICTORS, function(variable) {
    original <- raw[[variable]]
    completed <- imputed[[variable]]
    if (!anyNA(original)) return(NULL)
    data.frame(
      Variable = variable,
      Missing_Count = sum(is.na(original)),
      Original_Mean = mean(original, na.rm = TRUE),
      Imputed_Mean = mean(completed, na.rm = TRUE),
      Original_Median = stats::median(original, na.rm = TRUE),
      Imputed_Median = stats::median(completed, na.rm = TRUE),
      Original_SD = stats::sd(original, na.rm = TRUE),
      Imputed_SD = stats::sd(completed, na.rm = TRUE),
      row.names = NULL
    )
  }))
  if (is.null(validation)) validation <- data.frame(Note = "No numeric predictor required imputation")

  audit <- data.frame(
    Item = c(
      "Metal", "Rows retained", "Application cases removed", "MICE m",
      "MICE maxit", "MICE seed", "MICE used", "MICE warning",
      "Response imputed", "Intended downstream use"
    ),
    Value = c(
      CONFIG$metal,
      nrow(imputed),
      removed_cases,
      CONFIG$imputation$mice_m,
      CONFIG$imputation$mice_maxit,
      CONFIG$imputation$mice_seed,
      result$imputer$mice_used,
      result$imputer$mice_warning %||% "None",
      "No",
      "Favorable-domain and Boruta analyses; not ML input"
    ),
    row.names = NULL
  )

  notes <- data.frame(
    Step = c(
      "Categorical predictors",
      "Material properties",
      "Experimental conditions",
      "Divalent/Trivalent ratio",
      "Remaining numeric values",
      "Fallback"
    ),
    Implementation = c(
      "Training-set mode or predefined default",
      "Material x Synthesis_Method group median (mean fallback)",
      "First Author group median (mean fallback)",
      "Divalent_Cation x Trivalent_Cation group median (mean fallback)",
      "MICE predictive mean matching; m=5, maxit=10; first completed dataset",
      "Training-set global median or predefined numeric default if MICE cannot supply a value"
    ),
    row.names = NULL
  )

  output_xlsx <- file.path(output_dir, paste0(tolower(CONFIG$metal), "_imputed.xlsx"))
  openxlsx::write.xlsx(
    list(
      Imputed_Data = imputed,
      Missing_Before = before,
      Missing_After = after,
      Numeric_Validation = validation,
      Imputation_Audit = audit,
      Imputation_Notes = notes
    ),
    output_xlsx,
    overwrite = TRUE
  )
  saveRDS(result$imputer, file.path(output_dir, "hierarchical_imputer.rds"))
  writeLines(session_information(), file.path(output_dir, "sessionInfo.txt"))

  cat("Imputation completed.\n")
  cat("Rows:", nrow(imputed), "\n")
  cat("Remaining missing predictor values:", sum(is.na(imputed[LDH_PREDICTORS])), "\n")
  cat("Output:", output_xlsx, "\n")
  invisible(result)
}

if (sys.nframe() == 0L) main()

