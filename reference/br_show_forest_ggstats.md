# Show a forest plot with `ggstats` interface

**\[stable\]**

Provides an interface to visualize the model results with
[**ggstats**](https://github.com/larmarange/ggstats/) package.

## Usage

``` r
br_show_forest_ggstats(breg, idx = NULL, ...)
```

## Arguments

- breg:

  A regression object with results (must pass
  `assert_breg_obj_with_results()`).

- idx:

  Index or names (focal variables) of the model(s).

- ...:

  Arguments passing to
  [`ggstats::ggcoef_table()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html)
  or
  [`ggstats::ggcoef_compare()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html)
  excepts `model`.

## Value

A plot

## See also

Other br_show:
[`br_show_coxph_diagnostics()`](https://wanglabcsu.github.io/bregr/reference/br_show_coxph_diagnostics.md),
[`br_show_fitted_line()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line.md),
[`br_show_fitted_line_2d()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line_2d.md),
[`br_show_forest()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest.md),
[`br_show_forest_circle()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_circle.md),
[`br_show_forest_ggstatsplot()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstatsplot.md),
[`br_show_nomogram()`](https://wanglabcsu.github.io/bregr/reference/br_show_nomogram.md),
[`br_show_residuals()`](https://wanglabcsu.github.io/bregr/reference/br_show_residuals.md),
[`br_show_risk_network()`](https://wanglabcsu.github.io/bregr/reference/br_show_risk_network.md),
[`br_show_survival_curves()`](https://wanglabcsu.github.io/bregr/reference/br_show_survival_curves.md),
[`br_show_table()`](https://wanglabcsu.github.io/bregr/reference/br_show_table.md),
[`br_show_table_gt()`](https://wanglabcsu.github.io/bregr/reference/br_show_table_gt.md)

## Examples

``` r
if (rlang::is_installed("ggstats")) {
  m <- br_pipeline(mtcars,
    y = "mpg",
    x = colnames(mtcars)[2:4],
    x2 = "vs",
    method = "gaussian"
  )
  br_show_forest_ggstats(m)
}

```
