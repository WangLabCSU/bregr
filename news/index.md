# Changelog

## bregr 1.5.0

- Added
  [`br_seed()`](https://wanglabcsu.github.io/bregr/reference/br_seed.md),
  [`br_seed_screen()`](https://wanglabcsu.github.io/bregr/reference/br_seed_screen.md),
  [`br_seed_select()`](https://wanglabcsu.github.io/bregr/reference/br_seed_select.md),
  and
  [`br_seed_model()`](https://wanglabcsu.github.io/bregr/reference/br_seed_model.md)
  implementing the SEED (Selection of Essential prognostic genes from
  Expression Data) pipeline for identifying cancer prognosis target
  genes without control samples
  ([\#57](https://github.com/WangLabCSU/bregr/issues/57)). Based on Yang
  et al. (2025) *Cancer Letters*, DOI: 10.1016/j.canlet.2025.217960. The
  four-step pipeline includes:
  - Step 1 (`br_seed_screen`): Univariate screening via Cox/logistic
    regression or Spearman correlation against one or more clinical
    indicators, with p-value threshold filtering and significance
    intersection across indicators.
  - Step 2 (optional enrichment): GO/KEGG pathway enrichment via
    clusterProfiler to refine gene sets (gated by `enrich` argument).
  - Step 3 (`br_seed_select`): Sequential gene selection through Lasso
    regularization (glmnet), multivariate regression, and stepwise
    selection with reproducibility via `seed`.
  - Step 4 (`br_seed_model`): Risk score model construction with
    coefficient extraction and optional z-score expression scaling. All
    functions return S3 list objects with cli-based print methods, reuse
    the existing
    [`br_pipeline()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
    engine for batch regression, and support mirai parallelization via
    `n_workers`.

## bregr 1.4.0

CRAN release: 2026-01-18

- Added
  [`br_compare_models()`](https://wanglabcsu.github.io/bregr/reference/br_compare_models.md)
  and
  [`br_show_forest_comparison()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_comparison.md)
  ([\#54](https://github.com/WangLabCSU/bregr/issues/54)).
- Replaced the deprecated package `qs` with `qs2`.
- Updated
  [`br_show_fitted_line()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line.md)
  and
  [`br_show_fitted_line_2d()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line_2d.md)
  documentation for compatibility with **visreg** 3.0, which removes the
  `gg` argument and renames `line.par`, `fill.par`, `points.par` to
  `line`, `fill`, `points`
  ([\#65](https://github.com/WangLabCSU/bregr/issues/65)).

## bregr 1.3.2

- Checked with
  `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))` and
  fixed reported check issues.

## bregr 1.3.1

CRAN release: 2025-12-17

- Fixed typo in class construction.

## bregr 1.3.0

CRAN release: 2025-09-22

- Updated paper citation.
- Updated tests in
  [`br_pipeline()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
  to reduce running time.
- Fixed CRAN check issue in
  [`br_show_coxph_diagnostics()`](https://wanglabcsu.github.io/bregr/reference/br_show_coxph_diagnostics.md).

## bregr 1.2.0

CRAN release: 2025-08-19

**Enhancements & New Features:**

- Introduced
  [`br_show_forest_circle()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_circle.md)for
  circular forest plots.
- Added diagnostic visualization for Cox PH models via
  [`br_show_coxph_diagnostics()`](https://wanglabcsu.github.io/bregr/reference/br_show_coxph_diagnostics.md).
- Implemented
  [`br_show_nomogram()`](https://wanglabcsu.github.io/bregr/reference/br_show_nomogram.md)for
  clinical prediction modeling.
- Added `dry_run`option to
  [`br_pipeline()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)for
  pipeline validation.

**Fixes & Improvements:**

- Resolved interaction term display and factor scaling in
  [`br_show_nomogram()`](https://wanglabcsu.github.io/bregr/reference/br_show_nomogram.md).

## bregr 1.1.0

- Improved factor handling and warnings in polar plot and risk network
  functions.
- Added
  [`br_show_residuals()`](https://wanglabcsu.github.io/bregr/reference/br_show_residuals.md)
  to show residuals vs fitted plot for regression models.
- Updated
  [`br_set_x()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
  and
  [`br_set_x2()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
  to properly handle different input types, including valid column names
  (in R), invalid column names, and model formula terms.
- Supported error handling when model construction failed.
- Added
  [`br_get_model_names()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  and
  [`br_rename_models()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  to get and set model names.
- Soft-deprecated `run_parallel` in
  [`br_run()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md).
- Soft-deprecated
  [`br_get_model()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  by merging it with
  [`br_get_models()`](https://wanglabcsu.github.io/bregr/reference/accessors.md).
- Introduced global options “bregr.save_model” and “bregr.path”,
  configurable via [`options()`](https://rdrr.io/r/base/options.html).
- Enabled cross-platform parallel computation.
- Addressed lifecycle deprecation warnings in functions from
  **ggplot2**.
- Set the default `exponentiate` option appropriately in
  [`br_run()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md).
- Expanded
  [`br_avail_methods_use_exp()`](https://wanglabcsu.github.io/bregr/reference/avails.md)
  to include `c("poisson", "quasipoisson")`.
- Added a `log_first` option to
  [`br_show_forest()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest.md).
- Enhanced compatibility with multiple models from **broom.helpers**.
- Implemented the use of `:` for interaction term combinations
  throughout the package.
- Created several new vignettes to introduce the capabilities of
  **bregr**.
- Fixed a bug where dropping multiple columns failed in
  [`br_show_forest()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest.md).
- Resolved an issue where
  [`br_get_model()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  couldn’t properly process multiple string inputs for the `idx`
  parameter.

## bregr 1.0.0

CRAN release: 2025-06-28

- The first experimental version submitted to CRAN.
