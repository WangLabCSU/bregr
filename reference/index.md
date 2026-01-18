# Package index

## Class

Class definition and printing.

- [`breg()`](https://wanglabcsu.github.io/bregr/reference/breg.md)
  **\[stable\]** : Creates a new breg-class object
- [`print.breg`](https://wanglabcsu.github.io/bregr/reference/print.breg.md)
  **\[stable\]** : Print method for breg object
- [`print(`*`<breg_comparison>`*`)`](https://wanglabcsu.github.io/bregr/reference/print.breg_comparison.md)
  : Print method for breg_comparison object

## Workflow

Core workflow and pipeline wrapper.

- [`br_pipeline()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
  [`br_set_y()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
  [`br_set_x()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
  [`br_set_x2()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
  [`br_set_model()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
  [`br_run()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md)
  **\[stable\]** : Modeling and analysis pipeline
- [`br_compare_models()`](https://wanglabcsu.github.io/bregr/reference/br_compare_models.md)
  **\[experimental\]** : Compare univariate and multivariate models

## Accessor

Attributes and data accessors from \[breg\] objects.

- [`br_get_data()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_y()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_x()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_n_x()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_x2()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_n_x2()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_group_by()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_config()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_models()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_model()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_model_names()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_rename_models()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  [`br_get_results()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  **\[stable\]** :

  Accessor functions for `breg` objects

## Visualization

Visualize results using forest plots and more.

- [`br_show_forest()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest.md)
  **\[stable\]** : Show a forest plot for regression results

- [`br_show_forest_circle()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_circle.md)
  **\[experimental\]** : Show a circular forest plot for regression
  results

- [`br_show_risk_network()`](https://wanglabcsu.github.io/bregr/reference/br_show_risk_network.md)
  **\[stable\]** : Show connected risk network plot

- [`br_show_forest_ggstats()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstats.md)
  **\[stable\]** :

  Show a forest plot with `ggstats` interface

- [`br_show_forest_ggstatsplot()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstatsplot.md)
  **\[stable\]** :

  Show a forest plot with `ggstatsplot` interface

- [`br_show_forest_comparison()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_comparison.md)
  **\[experimental\]** : Show forest plot for model comparison

## Model Diagnostics and Use

Inspect and visualize models for diagnostic purposes, and enable
effective model utilization.

- [`br_diagnose()`](https://wanglabcsu.github.io/bregr/reference/br_diagnose.md)
  **\[experimental\]** : Diagnose regression models

- [`br_predict()`](https://wanglabcsu.github.io/bregr/reference/br_predict.md)
  **\[experimental\]** : Predict method for breg objects

- [`br_show_survival_curves()`](https://wanglabcsu.github.io/bregr/reference/br_show_survival_curves.md)
  **\[experimental\]** : Show survival curves based on model scores

- [`br_show_fitted_line()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line.md)
  **\[stable\]** :

  Show fitted regression line with `visreg` interface

- [`br_show_fitted_line_2d()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line_2d.md)
  **\[stable\]** :

  Show 2d fitted regression line with `visreg` interface

- [`br_show_residuals()`](https://wanglabcsu.github.io/bregr/reference/br_show_residuals.md)
  **\[experimental\]** : Show residuals vs fitted plot for regression
  models

- [`br_show_coxph_diagnostics()`](https://wanglabcsu.github.io/bregr/reference/br_show_coxph_diagnostics.md)
  **\[experimental\]** : Show Cox proportional hazards model diagnostic
  plots

- [`br_show_nomogram()`](https://wanglabcsu.github.io/bregr/reference/br_show_nomogram.md)
  **\[experimental\]** : Show nomogram for regression models

## Table

Output results in table of different formats.

- [`br_show_table()`](https://wanglabcsu.github.io/bregr/reference/br_show_table.md)
  **\[stable\]** : Show model tidy results in table format

- [`br_show_table_gt()`](https://wanglabcsu.github.io/bregr/reference/br_show_table_gt.md)
  **\[stable\]** :

  Show regression models with `gtsummary` interface

## Utility

Helpers for analysis and visualization.

- [`br_avail_methods()`](https://wanglabcsu.github.io/bregr/reference/avails.md)
  [`br_avail_methods_use_exp()`](https://wanglabcsu.github.io/bregr/reference/avails.md)
  [`br_avail_method_config()`](https://wanglabcsu.github.io/bregr/reference/avails.md)
  **\[stable\]** : Package availability
- [`polar_init()`](https://wanglabcsu.github.io/bregr/reference/polar_init.md)
  **\[stable\]** : Init a dot plot in polar system
- [`polar_connect()`](https://wanglabcsu.github.io/bregr/reference/polar_connect.md)
  **\[stable\]** : Connects dots
