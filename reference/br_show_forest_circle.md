# Show a circular forest plot for regression results

**\[experimental\]**

This function creates a circular (polar) forest plot from regression
results, providing an alternative visualization to the traditional
linear forest plot. The function uses the same input as
[`br_show_forest()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest.md)
but displays the results in a circular format using
[`ggplot2::coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html).

## Usage

``` r
br_show_forest_circle(
  breg,
  rm_controls = FALSE,
  style = c("points", "bars"),
  ref_line = TRUE,
  sort_by = c("none", "estimate", "estimate_desc", "pvalue", "variable"),
  subset = NULL,
  log_first = FALSE
)
```

## Arguments

- breg:

  A regression object with results.

- rm_controls:

  If `TRUE`, remove control terms.

- style:

  Character string specifying the style of circular forest plot. Options
  are:

  - `"points"` (default): Display point estimates with error bars in
    circular format

  - `"bars"`: Display as bars with points overlaid

- ref_line:

  Logical or numeric. If `TRUE`, shows reference circle at default value
  (1 for exponentiated estimates, 0 for regular estimates). If numeric,
  shows reference circle at specified value. If `FALSE`, no reference
  circle is shown.

- sort_by:

  Character string specifying how to sort the variables. Options are:

  - `"none"` (default): No sorting, use original order

  - `"estimate"`: Sort by effect estimate (ascending)

  - `"estimate_desc"`: Sort by effect estimate (descending)

  - `"pvalue"`: Sort by p-value (ascending, most significant first)

  - `"variable"`: Sort alphabetically by variable name

- subset:

  Expression for subsetting the results data (`br_get_results(breg)`).

- log_first:

  Log transformed the estimates and their confident intervals.

## Value

A ggplot object

## References

Implementation of circular forest plot
`https://mp.weixin.qq.com/s/PBKcsEFGrDSQJp6ZmUgfHA`

## See also

Other br_show:
[`br_show_coxph_diagnostics()`](https://wanglabcsu.github.io/bregr/reference/br_show_coxph_diagnostics.md),
[`br_show_fitted_line()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line.md),
[`br_show_fitted_line_2d()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line_2d.md),
[`br_show_forest()`](https://wanglabcsu.github.io/bregr/reference/br_show_forest.md),
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
m <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)
br_show_forest_circle(m)

br_show_forest_circle(m, style = "bars")

br_show_forest_circle(m, sort_by = "estimate")

br_show_forest_circle(m, ref_line = FALSE)

br_show_forest_circle(m, ref_line = 0.5)
```
