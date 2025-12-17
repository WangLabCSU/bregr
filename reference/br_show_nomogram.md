# Show nomogram for regression models

**\[experimental\]**

Creates a nomogram (graphical calculator) for regression models,
particularly useful for Cox proportional hazards models. A nomogram
allows visual calculation of predicted outcomes by assigning points to
variable values and summing them to get total points that correspond to
predicted probabilities.

## Usage

``` r
br_show_nomogram(
  breg,
  idx = NULL,
  time_points = c(12, 24, 36),
  fun_at = NULL,
  point_range = c(0, 100),
  title = NULL,
  subtitle = NULL
)
```

## Arguments

- breg:

  A `breg` object with fitted regression models.

- idx:

  Index or name of the model to use for the nomogram. If NULL, uses the
  first model.

- time_points:

  For Cox models, time points at which to show survival probabilities.
  Default is c(12, 24, 36) representing months.

- fun_at:

  For non-survival models, the function values at which to show
  predictions.

- point_range:

  Range of points to use in the nomogram scale. Default is c(0, 100).

- title:

  Plot title. If NULL, generates automatic title.

- subtitle:

  Plot subtitle.

## Value

A ggplot2 object showing the nomogram.

## See also

Other br_show:
[`br_show_coxph_diagnostics()`](https://wanglabcsu.github.io/bregr/reference/br_show_coxph_diagnostics.md),
[`br_show_fitted_line()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line.md),
[`br_show_fitted_line_2d()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line_2d.md),
[`br_show_forest()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest.md),
[`br_show_forest_circle()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_circle.md),
[`br_show_forest_ggstats()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstats.md),
[`br_show_forest_ggstatsplot()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstatsplot.md),
[`br_show_residuals()`](https://wanglabcsu.github.io/bregr/reference/br_show_residuals.md),
[`br_show_risk_network()`](https://wanglabcsu.github.io/bregr/reference/br_show_risk_network.md),
[`br_show_survival_curves()`](https://wanglabcsu.github.io/bregr/reference/br_show_survival_curves.md),
[`br_show_table()`](https://wanglabcsu.github.io/bregr/reference/br_show_table.md),
[`br_show_table_gt()`](https://wanglabcsu.github.io/bregr/reference/br_show_table_gt.md)

## Examples

``` r
# \donttest{
# Cox regression nomogram

lung <- survival::lung |> dplyr::filter(ph.ecog != 3)
lung$ph.ecog <- factor(lung$ph.ecog)
mds <- br_pipeline(
  lung,
  y = c("time", "status"),
  x = c("age", "ph.ecog"),
  x2 = "sex",
  method = "coxph"
)
#> exponentiate estimates of model(s) constructed from coxph method at default
p <- br_show_nomogram(mds)
#> `idx` not set, use the first model
#> Cox model: intercept term present but no intercept coefficient (as expected for
#> semi-parametric models)
p



# Linear regression nomogram
mds_lm <- br_pipeline(
  mtcars,
  y = "mpg",
  x = c("hp", "wt"),
  x2 = "vs",
  method = "gaussian"
)
p2 <- br_show_nomogram(mds_lm, fun_at = c(15, 20, 25, 30))
#> `idx` not set, use the first model
p2

# }
```
