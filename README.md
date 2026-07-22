# LDH Soil Metal Immobilization Analysis

Data and R code for article-grouped machine-learning analysis of layered double hydroxide (LDH) immobilization performance for As, Cd, and Pb in soils.

## Overview

This repository contains the curated database and R workflow used to evaluate LDH-mediated immobilization of As, Cd, and Pb and to identify dominant predictors and favorable predictor domains.

The workflow includes:

- hierarchical imputation of missing predictor values;
- article-grouped training/test partitioning;
- article-grouped 10-fold cross-validation;
- comparison and tuning of multiple machine-learning algorithms;
- independent-test evaluation;
- permutation importance and SHAP interpretation;
- favorable-domain identification using LOESS and weighted kernel density estimation;
- Boruta feature-ranking sensitivity analysis.

All analyses are performed separately for As, Cd, and Pb.

## Repository structure

```text
LDH-immobilization-analysis/
├── config/
│   └── analysis_config.R
├── data/
│   ├── input/
│   │   └── LDH_curated_database_1821.xlsx
│   └── working/
│       └── .gitkeep
├── R/
│   └── preprocessing_helpers.R
├── results/
│   └── .gitkeep
├── scripts/
│   ├── 00_install_packages.R
│   ├── 01_imputation.R
│   ├── 02_machine_learning.R
│   ├── 03_favorable_domains.R
│   └── 04_boruta_sensitivity.R
├── .gitignore
└── README.md
```

The `data/working/` and `results/` directories are retained using `.gitkeep`. Their generated contents are excluded from version control by `.gitignore`.

## Input dataset

The file:

```text
data/input/LDH_curated_database_1821.xlsx
```

contains 1,821 observations compiled from 79 publications after literature screening, data harmonization, quality control, and predefined outlier treatment.

This file is therefore a **curated input dataset**, rather than unprocessed primary raw data.

Missing values in the predictor variables are intentionally retained. Do not manually impute, standardize, normalize, or one-hot encode the dataset before running the workflow.

The response variable is stored in the `IE` column and represents the immobilization efficiency referred to as IE in the associated manuscript. The response variable is not imputed.

## Required variables

### Grouping and identification variables

The following columns are required:

- `Article_ID`: publication-level grouping variable;
- `First Author`: first author of the source publication;
- `Material`: LDH material identifier;
- `SYDX`: experimental medium classification;
- `HMs`: target metal or metal-combination classification;
- `IE`: immobilization efficiency used as the model response.

All observations extracted from the same publication must have the same `Article_ID`.

`Article_ID` may therefore occur in multiple rows. It must not be replaced by a row-specific sequence, and it must not contain missing or blank values.

### Predictor variables

The workflow uses the following 18 predictors:

1. `Synthesis_Method`
2. `Intercalated_Ions`
3. `Composite_Material_Type`
4. `Divalent_Cation`
5. `Trivalent_Cation`
6. `Divalent_Trivalent`
7. `Interlayer_Anion_Type`
8. `Specific_Surface_Area`
9. `Pore_Volume`
10. `Average_Pore_Diameter`
11. `Interlayer_Spacing`
12. `Material_Concentration`
13. `Initial_Concentration`
14. `Initial_pH`
15. `Experimental_Temperature`
16. `Competing_Ions`
17. `Reaction_Time`
18. `Soil_Moisture`

Do not change these column names unless the corresponding names are also updated in the configuration and helper files.

## Preparing the metal-specific soil datasets

The public repository provides one complete curated database. Before running the analyses, users should manually prepare separate soil datasets for As, Cd, and Pb.

Create the following files:

```text
data/working/as_soil.xlsx
data/working/cd_soil.xlsx
data/working/pb_soil.xlsx
```

### Filtering procedure

Starting from `LDH_curated_database_1821.xlsx`:

1. Use the `SYDX` column to retain only observations classified as soil.
2. Exclude solution experiments and soil-water-extract experiments.
3. Use the `HMs` column to select one target metal by exact matching:
   - retain only `As` for `as_soil.xlsx`;
   - retain only `Cd` for `cd_soil.xlsx`;
   - retain only `Pb` for `pb_soil.xlsx`.
4. Do not use partial-text matching. For example, selecting all cells containing “As” could incorrectly retain `As-Cd` or `As-Cd-Pb`.
5. Exclude mixed-metal records, including those labelled as `As-Cd`, `Cd-Pb`, or `As-Cd-Pb`. These records correspond to the real co-contamination application cases and are not used for model development.
6. Retain `Article_ID`, `First Author`, `Material`, `IE`, and all 18 predictors.
7. Retain the original missing predictor values.
8. Save each dataset as an `.xlsx` workbook with the data in a worksheet named `Sheet1`.

Strictly selecting `HMs == "As"`, `HMs == "Cd"`, or `HMs == "Pb"` will automatically exclude mixed-metal labels.

The metal-specific files are working files and are intentionally excluded from the public repository.

## Configuration

Before running the workflow for each metal, open:

```text
config/analysis_config.R
```

and edit the metal name, input file, and output directory.

### As configuration

```r
metal = "As",
input_file = "data/working/as_soil.xlsx",
input_sheet = "Sheet1",
output_dir = "results/as",

target_var = "IE",
article_id_var = "Article_ID",
```

### Cd configuration

```r
metal = "Cd",
input_file = "data/working/cd_soil.xlsx",
input_sheet = "Sheet1",
output_dir = "results/cd",

target_var = "IE",
article_id_var = "Article_ID",
```

### Pb configuration

```r
metal = "Pb",
input_file = "data/working/pb_soil.xlsx",
input_sheet = "Sheet1",
output_dir = "results/pb",

target_var = "IE",
article_id_var = "Article_ID",
```

Because the metal-specific working files already exclude the application-case records, retain:

```r
input_already_excludes_application_cases = TRUE
```

The principal configuration is summarized below.

| Metal | Input file | Output directory |
|---|---|---|
| As | `data/working/as_soil.xlsx` | `results/as` |
| Cd | `data/working/cd_soil.xlsx` | `results/cd` |
| Pb | `data/working/pb_soil.xlsx` | `results/pb` |

## Software setup

A recent version of R is required.

The required packages can be installed by running the following command once from the repository root:

```r
source("scripts/00_install_packages.R")
```

The installation script checks for missing packages and installs them when necessary.

Some packages, particularly `lightgbm`, may require additional compilation tools depending on the operating system and R installation.

## Running the workflow

Run the scripts from the repository root so that all relative file paths can be resolved correctly.

For each target metal, first update `config/analysis_config.R`, and then run the four analysis scripts in the following order.

### Command-line method

```bash
Rscript scripts/01_imputation.R
Rscript scripts/02_machine_learning.R
Rscript scripts/03_favorable_domains.R
Rscript scripts/04_boruta_sensitivity.R
```

### RStudio method

Set the working directory to the repository root and run:

```r
source("scripts/01_imputation.R")
main()

source("scripts/02_machine_learning.R")
main()

source("scripts/03_favorable_domains.R")
main()

source("scripts/04_boruta_sensitivity.R")
main()
```

The scripts define their analysis inside a `main()` function. When using RStudio and `source()`, run `main()` after sourcing each script.

Complete all four scripts for one metal before changing the configuration to the next metal.

## Analysis workflow

### 1. Full-dataset imputation

`scripts/01_imputation.R` reads the incomplete metal-specific soil dataset and performs hierarchical imputation of the predictor variables.

The full imputation procedure includes:

- categorical imputation using modes or predefined defaults;
- material-property imputation using material and synthesis-method group summaries;
- experimental-condition imputation using publication-level summaries;
- imputation of the divalent/trivalent ratio using cation-combination summaries;
- MICE predictive mean matching for remaining numerical missing values;
- fallback rules for any values that cannot be completed by MICE.

The default MICE settings are:

```text
m = 5
maxit = 10
seed = 123
```

The first completed MICE dataset is exported.

The response variable `IE` is never imputed.

The principal output is:

```text
results/<metal>/01_imputation/<metal>_imputed.xlsx
```

The completed data are stored in the `Imputed_Data` worksheet.

This fully imputed dataset is used only by the favorable-domain and Boruta analyses. It is not used as the input for machine-learning model selection or test evaluation.

### 2. Article-grouped machine learning

`scripts/02_machine_learning.R` starts from the incomplete metal-specific working dataset rather than the fully imputed file.

The machine-learning procedure includes:

- an 80%/20% training/test split at the `Article_ID` level;
- article-grouped 10-fold cross-validation within the training partition;
- fold-specific preprocessing and imputation;
- candidate-model tuning and comparison;
- final-model fitting using the complete training partition;
- one-time evaluation using the independent test partition;
- grouped permutation importance;
- global SHAP analysis.

All observations from the same publication are assigned exclusively to either the training or test partition. Similarly, observations from the same publication cannot occur in both the analysis and assessment portions of a cross-validation fold.

Within every cross-validation fold, the following operations are fitted using only the analysis-fold data and then applied unchanged to the corresponding assessment fold:

- hierarchical imputation;
- categorical mode estimation;
- rare-level handling;
- MICE-based completion;
- one-hot encoding;
- near-zero-variance filtering;
- centering and scaling.

After model selection, the same preprocessing sequence is refitted using the complete training partition and applied to the independent test partition.

This design prevents information from validation or test articles from entering preprocessing or model selection.

### Candidate models

The candidate-model pool includes:

- Random Forest;
- LightGBM;
- XGBoost;
- radial-basis-function Support Vector Machine;
- Linear Regression;
- Elastic Net;
- Multivariate Adaptive Regression Splines;
- Gradient Boosting Decision Trees;
- k-Nearest Neighbors;
- Gaussian Process Regression.

Models are ranked primarily by mean cross-validated R². RMSE and MAE are used as complementary performance metrics.

The independent test set is not used to select the best-performing model.

### Permutation importance

Permutation importance is evaluated using the fitted final model and held-out test observations.

For categorical predictors represented by multiple one-hot columns, all columns originating from the same predictor are permuted together.

Importance is defined as the increase in RMSE relative to the unpermuted baseline and is reported as normalized ΔRMSE.

The default settings are:

```text
Point-estimate permutation repeats: 10
Bootstrap replicates: 1,000
Uncertainty interval: 95% bootstrap confidence interval
```

No permutation-derived significance tests or star annotations are calculated.

### SHAP analysis

Global SHAP values are calculated for the selected final model using held-out test observations.

The default settings are:

```text
Maximum explained test observations: 100
Maximum training-background observations: 100
Monte Carlo simulations: 50
```

SHAP values from one-hot columns belonging to the same original categorical predictor are summed before global aggregation.

Permutation importance and SHAP values are interpreted as predictive attributions, not as evidence of causal relationships, particularly when predictors are correlated.

### 3. Favorable-domain analysis

`scripts/03_favorable_domains.R` automatically reads the `Imputed_Data` worksheet generated by `01_imputation.R`.

For continuous predictors, favorable domains are identified using:

- LOESS response curves;
- regions with predicted IE values of at least 75% of the fitted peak;
- IE-weighted kernel density estimation;
- a central 85% weighted-density interval;
- the top 25% of observations when the relevant sample size is at least 30;
- the top 40% of observations when the relevant sample size is below 30.

For categorical predictors, the script summarizes category-specific performance and identifies categories enriched among high-performing observations.

### 4. Boruta sensitivity analysis

`scripts/04_boruta_sensitivity.R` automatically reads the `Imputed_Data` worksheet generated by `01_imputation.R`.

Boruta is used only as a feature-ranking sensitivity analysis and does not replace the machine-learning model-selection procedure.

The default Boruta settings are:

```text
seed = 2024
maxRuns = 200
pValue = 0.01
mcAdj = TRUE
```

## Output files

All generated results are written under:

```text
results/<metal>/
```

The main output directories are:

```text
results/<metal>/01_imputation/
results/<metal>/02_machine_learning/
results/<metal>/03_favorable_domains/
results/<metal>/04_boruta_sensitivity/
```

Principal output files include:

```text
01_imputation/<metal>_imputed.xlsx
01_imputation/hierarchical_imputer.rds

02_machine_learning/machine_learning_results.xlsx
02_machine_learning/best_model_bundle.rds
02_machine_learning/plots/

03_favorable_domains/favorable_domain_results.xlsx
03_favorable_domains/favorable_domain_objects.rds
03_favorable_domains/plots/

04_boruta_sensitivity/boruta_results.xlsx
04_boruta_sensitivity/boruta_model.rds
04_boruta_sensitivity/boruta_importance.pdf
04_boruta_sensitivity/boruta_importance.png
```

Each analysis also exports `sessionInfo.txt` to record the R environment and package versions used in the run.

The machine-learning output records:

- training and test article assignments;
- cross-validation fold assignments;
- model-tuning results;
- cross-validated model comparisons;
- independent-test performance;
- observed and predicted values;
- grouped permutation importance;
- SHAP results;
- preprocessing and analysis audit information.

Generated outputs are excluded from GitHub by `.gitignore`.

## Reproducibility notes

For reproducible use:

1. Start from `LDH_curated_database_1821.xlsx`.
2. Select soil observations and one target metal using exact category matching.
3. Do not include mixed-metal application cases in model development.
4. Do not manually fill missing predictor values.
5. Preserve the publication-level `Article_ID` assignments.
6. Run all scripts from the repository root.
7. Run the scripts in the specified order.
8. Use the same configuration settings and random seeds.
9. Do not use the fully imputed spreadsheet as the machine-learning input.
10. Retain the generated `sessionInfo.txt` files when documenting a completed analysis.

## Citation

Please cite the associated manuscript when using this dataset or workflow.

Citation information will be updated after publication.