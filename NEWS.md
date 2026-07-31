# bregr 1.5.0

- Added `br_seed()`, `br_seed_screen()`, `br_seed_select()`, and `br_seed_model()`
  implementing the SEED (Selection of Essential prognostic genes from Expression Data)
  pipeline for identifying cancer prognosis target genes without control samples (#57).
  Based on Yang et al. (2025) *Cancer Letters*, DOI: 10.1016/j.canlet.2025.217960.
  The four-step pipeline includes:
  - Step 1 (`br_seed_screen`): Univariate screening via Cox/logistic regression or
    Spearman correlation against one or more clinical indicators, with p-value
    threshold filtering and significance intersection across indicators.
  - Step 2 (optional enrichment): GO/KEGG pathway enrichment via clusterProfiler
    to refine gene sets (gated by `enrich` argument).
  - Step 3 (`br_seed_select`): Sequential gene selection through Lasso
    regularization (glmnet), multivariate regression, and stepwise selection
    with reproducibility via `seed`.
  - Step 4 (`br_seed_model`): Risk score model construction with coefficient
    extraction and optional z-score expression scaling.
  All functions return S3 list objects with cli-based print methods, reuse the
  existing `br_pipeline()` engine for batch regression, and support mirai
  parallelization via `n_workers`.
- Added `br_get_model_stats()` returning a tidy data.frame of per-model summary
  statistics (N, events, C-index, AIC, LR test p, PH test p for Cox; N, AIC,
  deviance for GLM; N, R-squared for LM), eliminating the need for manual
  supplementary model fitting to obtain diagnostic metrics (#68).
- Added AIC to Cox model diagnostics in `br_diagnose()`.
- Systematically support non-standard variable names: `FGFR3::TACC3`, `EGFR-AS1`,
  `1p`/`2q`, `gene name`, R reserved words (`if`, `TRUE`, `NA`), and
  backtick-quoted user input in `x`, `x2`, and `y` variables (#69). Refactored
  `repair_names()` with `remove_backticks()` for robust quoting, and applied
  quoting to response variables in `br_set_y()`.
- Added `biocViews` field to DESCRIPTION for Bioconductor Suggests compatibility.

# bregr 1.4.0

- Added `br_compare_models()` and `br_show_forest_comparison()` (#54).
- Replaced the deprecated package `qs` with `qs2`.
- Updated `br_show_fitted_line()` and `br_show_fitted_line_2d()` documentation
  for compatibility with **visreg** 3.0, which removes the `gg` argument and
  renames `line.par`, `fill.par`, `points.par` to `line`, `fill`, `points` (#65).

# bregr 1.3.2

- Checked with `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))` and fixed reported check issues.

# bregr 1.3.1

- Fixed typo in class construction.

# bregr 1.3.0

- Updated paper citation.
- Updated tests in `br_pipeline()` to reduce running time.
- Fixed CRAN check issue in `br_show_coxph_diagnostics()`.

# bregr 1.2.0

**Enhancements & New Features:**

- Introduced `br_show_forest_circle()`for circular forest plots.
- Added diagnostic visualization for Cox PH models via `br_show_coxph_diagnostics()`.
- Implemented `br_show_nomogram()`for clinical prediction modeling.
- Added `dry_run`option to `br_pipeline()`for pipeline validation.

**Fixes & Improvements:**

- Resolved interaction term display and factor scaling in `br_show_nomogram()`.

# bregr 1.1.0

* Improved factor handling and warnings in polar plot and risk network functions.
* Added `br_show_residuals()` to show residuals vs fitted plot for regression models.
* Updated `br_set_x()` and `br_set_x2()` to properly handle different input
types, including valid column names (in R), invalid column names, and
model formula terms.
* Supported error handling when model construction failed.
* Added `br_get_model_names()` and `br_rename_models()` to get and set model names.
* Soft-deprecated `run_parallel` in `br_run()`.
* Soft-deprecated `br_get_model()` by merging it with `br_get_models()`.
* Introduced global options "bregr.save_model" and "bregr.path", configurable via `options()`.
* Enabled cross-platform parallel computation.
* Addressed lifecycle deprecation warnings in functions from **ggplot2**.
* Set the default `exponentiate` option appropriately in `br_run()`.
* Expanded `br_avail_methods_use_exp()` to include `c("poisson", "quasipoisson")`.
* Added a `log_first` option to `br_show_forest()`.
* Enhanced compatibility with multiple models from **broom.helpers**.
* Implemented the use of `:` for interaction term combinations throughout the package.
* Created several new vignettes to introduce the capabilities of **bregr**.
* Fixed a bug where dropping multiple columns failed in `br_show_forest()`.
* Resolved an issue where `br_get_model()` couldn't properly process multiple string inputs for the `idx` parameter.

# bregr 1.0.0

* The first experimental version submitted to CRAN.

