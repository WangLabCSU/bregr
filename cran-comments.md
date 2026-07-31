* Checked with `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))` and fixed reported check issues.
* Replaced `qs` with `qs2`.
* Added `br_compare_models()` and `br_show_forest_comparison()`.
* Added SEED gene-selection pipeline: `br_seed()`, `br_seed_screen()`, `br_seed_select()`, `br_seed_model()` (#57).
* Added `glmnet` and `clusterProfiler` to Suggests for optional SEED steps (Lasso, enrichment).
