# Show survival curves based on model scores

**\[experimental\]**

Generate survival curves by grouping observations based on model
prediction scores. This function is specifically designed for Cox
regression models and creates survival curves comparing different risk
groups.

## Usage

``` r
br_show_survival_curves(
  breg,
  idx = NULL,
  n_groups = 3,
  group_labels = NULL,
  title = NULL,
  subtitle = NULL
)
```

## Arguments

- breg:

  A `breg` object with fitted Cox regression models.

- idx:

  Index or name of the model to use for prediction. If NULL, uses the
  first model.

- n_groups:

  Number of groups to create based on score quantiles. Default is 3.

- group_labels:

  Custom labels for the groups. If NULL, uses "Low", "Medium", "High"
  for 3 groups or "Q1", "Q2", etc. for other numbers.

- title:

  Plot title. If NULL, generates automatic title.

- subtitle:

  Plot subtitle.

## Value

A ggplot2 object showing survival curves.

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
[`br_show_residuals()`](https://wanglabcsu.github.io/bregr/reference/br_show_residuals.md),
[`br_show_risk_network()`](https://wanglabcsu.github.io/bregr/reference/br_show_risk_network.md),
[`br_show_table()`](https://wanglabcsu.github.io/bregr/reference/br_show_table.md),
[`br_show_table_gt()`](https://wanglabcsu.github.io/bregr/reference/br_show_table_gt.md)

## Examples

``` r
# \donttest{
# Cox regression example with survival curves
if (requireNamespace("survival", quietly = TRUE)) {
  lung <- survival::lung |> dplyr::filter(ph.ecog != 3)
  mds <- br_pipeline(
    lung,
    y = c("time", "status"),
    x = c("age", "ph.ecog"),
    x2 = "sex",
    method = "coxph"
  )
  p <- br_show_survival_curves(mds)
  print(p)
}
#> exponentiate estimates of model(s) constructed from coxph method at default
#> `idx` not set, use the first model

# }
```
