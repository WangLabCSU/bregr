# bregr (development version)

* Added logging option ("bregr.log_n") for modeling runner with bregr.log_n
* Added `br_show_residuals()` to show residuals vs fitted plot for regression models.
* Updated `br_set_x()` and `br_set_x2()` to properly handle different input
types, including valid column names (in R), invalid column names, and
model formula terms.
* Supported error handling when model construction failed.
* Added `br_get_model_names()` and `br_rename_models()` to get and set model names.
* Soft-deprecated `br_get_model()` by merging it with `br_get_models()`.
* Introduced global options "bregr.save_model" and "bregr.path", configurable via `options()`.
* Enabled cross-platform parallel computation through integration with **furrr**, **future.apply**, or **parallel**, and introduced global option "bregr.parallel_engine" (default is `TRUE` for `run_parallel > 1`). "parallel" is the default and recommended engine, as we observed bug and performance issues when using other engines in RStudio.
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

