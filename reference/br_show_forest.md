# Show a forest plot for regression results

**\[stable\]**

This function takes regression results and formats them into a forest
plot display. It handles:

- Formatting of estimates, CIs and p-values

- Automatic x-axis limits calculation

- Cleaning of redundant group/focal variable labels

- Custom subsetting and column dropping The function uses
  [`forestploter::forest()`](https://rdrr.io/pkg/forestploter/man/forest.html)
  internally for the actual plotting.

## Usage

``` r
br_show_forest(
  breg,
  clean = TRUE,
  rm_controls = FALSE,
  ...,
  subset = NULL,
  drop = NULL,
  tab_headers = NULL,
  log_first = FALSE
)
```

## Arguments

- breg:

  A regression object with results (must pass
  `assert_breg_obj_with_results()`).

- clean:

  Logical indicating whether to clean/condense redundant group/focal
  variable labels. If `TRUE`, remove "Group" or "Focal" variable column
  when the values in the result table are the same (before performing
  `subset` and `drop`), and reduce repeat values in column "Group",
  "Focal", and "Variable".

- rm_controls:

  If `TRUE`, remove control terms.

- ...:

  Additional arguments passed to
  [`forestploter::forest()`](https://rdrr.io/pkg/forestploter/man/forest.html),
  run
  [`vignette("forestploter-post", "forestploter")`](https://cran.rstudio.com/web/packages/forestploter/vignettes/forestploter-post.html)
  to see more plot options. For example, use `ticks_at` to specify
  custom ticks, generally a vector of 4-5 elements.

- subset:

  Expression for subsetting the results data (`br_get_results(breg)`).

- drop:

  Column indices to drop from the display table.

- tab_headers:

  Character vector of custom column headers (must match number of
  displayed columns).

- log_first:

  Log transformed the estimates and their confident intervals. For only
  log scaled axis of the forest, use `x_trans = "log"`.

## Value

A plot

## See also

Other br_show:
[`br_show_coxph_diagnostics()`](https://wanglabcsu.github.io/bregr/reference/br_show_coxph_diagnostics.md),
[`br_show_fitted_line()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line.md),
[`br_show_fitted_line_2d()`](https://wanglabcsu.github.io/bregr/reference/br_show_fitted_line_2d.md),
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
m <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)
br_show_forest(m)

br_show_forest(m, clean = TRUE, drop = 3)

br_show_forest(m, clean = FALSE)
```
