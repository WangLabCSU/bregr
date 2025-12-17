# Changelog

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
