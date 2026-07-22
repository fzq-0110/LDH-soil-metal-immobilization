# Central configuration for one metal-specific analysis run.
# Edit this file, then run the scripts from the repository root.

CONFIG <- list(
  metal = "As",
  input_file = "data/working/as_soil.xlsx",
  output_dir = "results/as",

  # The manuscript calls this outcome immobilization efficiency (IE), while
  # the current spreadsheets and legacy code use the column name IE.
  target_var = "IE",
  article_id_var = "Article_ID",
  author_var = "First Author",
  material_var = "Material",

  # Set to FALSE only when the input still contains the 21 application cases.
  # In that situation, provide either their Article IDs or a valid flag column.
  input_already_excludes_application_cases = TRUE,
  application_case_article_ids = character(0),
  application_case_flag_column = "Application_Case",
  application_case_flag_values = c("1", "true", "yes", "application-case", "case"),

  # Manuscript/SI parameters retained from the original scripts.
  imputation = list(
    mice_m = 5L,
    mice_maxit = 10L,
    mice_seed = 123L,
    pmm_donors = 5L
  ),
  modeling = list(
    seed = 42L,
    train_ratio = 0.80,
    cv_folds = 10L,
    rare_level_min_count = 5L,
    rare_level_min_fraction = 0.01,
    models = c(
      "Random_Forest", "XGBoost", "LightGBM", "GBDT", "SVM_RBF",
      "Linear_Regression", "Elastic_Net", "MARS", "KNN", "GPR_Radial"
    ),
    bayesian_init_points = 10L,
    bayesian_n_iter = 30L,
    bayesian_acq = "ucb",
    bayesian_kappa = 2.576,
    rf_tuning_trees = 200L,
    rf_final_trees = 500L,
    lightgbm_trials = 30L,
    lightgbm_max_rounds = 2000L,
    lightgbm_early_stopping_rounds = 100L,
    permutation_repeats = 10L,
    importance_bootstrap_repeats = 1000L,
    shap_explain_sample_size = 100L,
    shap_background_sample_size = 100L,
    shap_nsim = 50L
  ),
  favorable_domains = list(
    input_file = NULL,
    input_sheet = "Imputed_Data",
    loess_span = 0.75,
    loess_peak_threshold = 0.75,
    wkde_n = 512L,
    wkde_central_mass = 0.85,
    min_numeric_n = 10L,
    min_categorical_n = 5L
  ),
  boruta = list(
    input_file = NULL,
    input_sheet = "Imputed_Data",
    seed = 2024L,
    max_runs = 200L,
    p_value = 0.01,
    mc_adj = TRUE
  )
)
