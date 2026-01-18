# Show connected risk network plot

**\[stable\]**

## Usage

``` r
br_show_risk_network(breg, ...)
```

## Arguments

- breg:

  A regression object with results (must pass
  `assert_breg_obj_with_results()`).

- ...:

  Arguments passing to
  [`br_get_results()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  for subsetting data table.

## Value

A plot

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
[`br_show_survival_curves()`](https://wanglabcsu.github.io/bregr/reference/br_show_survival_curves.md),
[`br_show_table()`](https://wanglabcsu.github.io/bregr/reference/br_show_table.md),
[`br_show_table_gt()`](https://wanglabcsu.github.io/bregr/reference/br_show_table_gt.md)

Other risk_network:
[`polar_connect()`](https://wanglabcsu.github.io/bregr/reference/polar_connect.md),
[`polar_init()`](https://wanglabcsu.github.io/bregr/reference/polar_init.md)

## Examples

``` r
lung <- survival::lung
# Cox-PH regression
mod_surv <- br_pipeline(
  data = lung,
  y = c("time", "status"),
  x = c("age", "ph.ecog", "ph.karno"),
  x2 = c("factor(sex)"),
  method = "coxph"
)
#> exponentiate estimates of model(s) constructed from coxph method at default

if (requireNamespace("ggnewscale")) {
  p <- br_show_risk_network(mod_surv)
  p
}
#> Loading required namespace: ggnewscale
#> please note only continuous focal terms analyzed and visualized

```
