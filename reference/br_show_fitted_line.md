# Show fitted regression line with `visreg` interface

**\[stable\]**

Provides an interface to visualize the model results with
[**visreg**](https://github.com/larmarange/ggstats/) package, to show
how a predictor variable `x` affects an outcome `y`.

## Usage

``` r
br_show_fitted_line(breg, idx = 1, ...)
```

## Arguments

- breg:

  A regression object with results (must pass
  `assert_breg_obj_with_results()`).

- idx:

  Length-1 vector. Index or name (focal variable) of the model. This is
  different from `idx` in
  [br_show_forest_ggstats](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstats.md),
  only one model is supported to visualized here, so only length-1
  vector is supported as `idx`.

- ...:

  Arguments passing to
  [`visreg::visreg()`](https://pbreheny.github.io/visreg/reference/visreg.html)
  excepts `fit` and `data`.

## Value

A plot

## See also

Other br_show:
[`br_show_coxph_diagnostics()`](https://wanglabcsu.github.io/bregr/reference/br_show_coxph_diagnostics.md),
[`br_show_fitted_line_2d()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line_2d.md),
[`br_show_forest()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest.md),
[`br_show_forest_circle()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_circle.md),
[`br_show_forest_ggstats()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstats.md),
[`br_show_forest_ggstatsplot()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest_ggstatsplot.md),
[`br_show_nomogram()`](https://wanglabcsu.github.io/bregr/reference/br_show_nomogram.md),
[`br_show_residuals()`](https://wanglabcsu.github.io/bregr/reference/br_show_residuals.md),
[`br_show_risk_network()`](https://wanglabcsu.github.io/bregr/reference/br_show_risk_network.md),
[`br_show_survival_curves()`](https://wanglabcsu.github.io/bregr/reference/br_show_survival_curves.md),
[`br_show_table()`](https://wanglabcsu.github.io/bregr/reference/br_show_table.md),
[`br_show_table_gt()`](https://wanglabcsu.github.io/bregr/reference/br_show_table_gt.md)

## Examples

``` r
if (rlang::is_installed("visreg")) {
  m <- br_pipeline(mtcars,
    y = "mpg",
    x = colnames(mtcars)[2:4],
    x2 = "vs",
    method = "gaussian"
  )

  if (interactive()) {
    br_show_fitted_line(m)
  }
  br_show_fitted_line(m, xvar = "cyl")
}
#> subset model list with idx: 1
#> model call: stats::glm(formula = mpg ~ cyl + vs, family = stats::gaussian, data
#> = data)

```
