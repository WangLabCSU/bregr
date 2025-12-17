# Show residuals vs fitted plot for regression models

**\[experimental\]**

This function creates residual plots to diagnose model fit. It can
display:

- Residuals vs fitted values plots for individual models

- Multiple residual plots when multiple models are selected

- Customizable plot appearance through ggplot2

## Usage

``` r
br_show_residuals(breg, idx = NULL, plot_type = "fitted")
```

## Arguments

- breg:

  A regression object with results (must pass
  `assert_breg_obj_with_results()`).

- idx:

  Index or names (focal variables) of the model(s). If `NULL` (default),
  all models are included. If length-1, shows residuals for a single
  model. If length \> 1, shows faceted plots for multiple models.

- plot_type:

  Character string specifying the type of residual plot. Options:
  "fitted" (residuals vs fitted values, default), "qq" (Q-Q plot),
  "scale_location" (scale-location plot).

## Value

A ggplot object

## See also

Other br_show:
[`br_show_coxph_diagnostics()`](https://wanglabcsu.github.io/bregr/reference/br_show_coxph_diagnostics.md),
[`br_show_fitted_line()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line.md),
[`br_show_fitted_line_2d()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line_2d.md),
[`br_show_forest()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest.md),
[`br_show_forest_circle()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_circle.md),
[`br_show_forest_ggstats()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstats.md),
[`br_show_forest_ggstatsplot()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstatsplot.md),
[`br_show_nomogram()`](https://wanglabcsu.github.io/bregr/reference/br_show_nomogram.md),
[`br_show_risk_network()`](https://wanglabcsu.github.io/bregr/reference/br_show_risk_network.md),
[`br_show_survival_curves()`](https://wanglabcsu.github.io/bregr/reference/br_show_survival_curves.md),
[`br_show_table()`](https://wanglabcsu.github.io/bregr/reference/br_show_table.md),
[`br_show_table_gt()`](https://wanglabcsu.github.io/bregr/reference/br_show_table_gt.md)

## Examples

``` r
m <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)

# Single model residual plot
br_show_residuals(m, idx = 1)
#> `geom_smooth()` using formula = 'y ~ x'


# Multiple models
br_show_residuals(m, idx = c(1, 2))
#> `geom_smooth()` using formula = 'y ~ x'


# All models
br_show_residuals(m)
#> `geom_smooth()` using formula = 'y ~ x'

```
