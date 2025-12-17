# Show model tidy results in table format

**\[stable\]**

## Usage

``` r
br_show_table(
  breg,
  ...,
  args_table_format = list(),
  export = FALSE,
  args_table_export = list()
)
```

## Arguments

- breg:

  A regression object with results (must pass
  `assert_breg_obj_with_results()`).

- ...:

  Arguments passing to
  [`br_get_results()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
  for subsetting table.

- args_table_format:

  A list of arguments passing to
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html).

- export:

  Logical. If `TRUE`, show table for export purpose, e.g., present the
  table in Markdown or HTML format.

- args_table_export:

  A list of arguments passing to
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).
  Only works when `export` is `TRUE`.

## Value

A table

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
[`br_show_survival_curves()`](https://wanglabcsu.github.io/bregr/reference/br_show_survival_curves.md),
[`br_show_table_gt()`](https://wanglabcsu.github.io/bregr/reference/br_show_table_gt.md)

## Examples

``` r
m <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)

br_show_table(m)
#>   Focal_variable term estimate std.error statistic p.value       conf.int
#> 1            cyl  cyl    -3.09      0.56     -5.54  < .001 [-4.18, -2.00]
#> 2            cyl   vs    -0.94      1.98     -0.47  0.638  [-4.81,  2.94]
#> 3           disp disp    -0.04  6.72e-03     -5.49  < .001 [-0.05, -0.02]
#> 4           disp   vs     1.50      1.65      0.91  0.373  [-1.74,  4.73]
#> 5             hp   hp    -0.05      0.01     -3.77  < .001 [-0.08, -0.03]
#> 6             hp   vs     2.58      1.97      1.31  0.201  [-1.28,  6.44]
br_show_table(m, export = TRUE)
#> Focal_variable | term | estimate | std.error | statistic | p.value |       conf.int
#> -----------------------------------------------------------------------------------
#> cyl            |  cyl |    -3.09 |      0.56 |     -5.54 |  < .001 | [-4.18, -2.00]
#> cyl            |   vs |    -0.94 |      1.98 |     -0.47 |  0.638  | [-4.81,  2.94]
#> disp           | disp |    -0.04 |  6.72e-03 |     -5.49 |  < .001 | [-0.05, -0.02]
#> disp           |   vs |     1.50 |      1.65 |      0.91 |  0.373  | [-1.74,  4.73]
#> hp             |   hp |    -0.05 |      0.01 |     -3.77 |  < .001 | [-0.08, -0.03]
#> hp             |   vs |     2.58 |      1.97 |      1.31 |  0.201  | [-1.28,  6.44]
if (interactive()) {
  br_show_table(m, export = TRUE, args_table_export = list(format = "html"))
}
```
