# bregr (development version)

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

