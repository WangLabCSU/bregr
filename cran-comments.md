* Checked with `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))` and fixed reported check issues.
* Replaced `qs` with `qs2`.
* Added `br_compare_models()` and `br_show_forest_comparison()`.
* Added SEED gene-selection pipeline: `br_seed()`, `br_seed_screen()`, `br_seed_select()`, `br_seed_model()` (#57).
* Added `glmnet` and `clusterProfiler` to Suggests for optional SEED steps.
* Added `biocViews` field to DESCRIPTION for Bioconductor Suggests compatibility.
* Added `br_get_model_stats()` for tidy per-model summary statistics (#68).
* Added AIC to Cox model diagnostics in `br_diagnose()`.
* Systematically support non-standard variable names with special characters (#69).
