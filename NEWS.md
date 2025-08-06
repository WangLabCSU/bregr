# bregr (development version)

* Added global options "bregr.save_model" and "bregr.path", users can
  set them with `options()`.
* Supported parallel computation on all platforms with **furrr**.
* Handled lifecyle deprecated issues in functions from **ggplot2**.
* Properly set default `exponentiate` option in `br_run()`.
* Added `c("poisson", "quasipoisson")` to `br_avail_methods_use_exp()`.
* Added `log_first` option to `br_show_forest()`.
* Supported many models from **broom.helpers**.
* Used `:` to combine interaction term system-wide.
* Added multiple vignettes for introducing **bregr**.
* Fixed the issue where dropping multiple columns failed in `br_show_forest()`.
* Resolved the problem where `br_get_model()` couldn't correctly process multiple strings as input for the `idx` parameter.

# bregr 1.0.0

* The first experimental version submitted to CRAN.

