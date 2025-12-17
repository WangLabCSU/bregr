# Show Cox proportional hazards model diagnostic plots

**\[experimental\]**

Creates diagnostic plots specifically for Cox regression models. Focuses
on Schoenfeld residuals plots to assess proportional hazards assumption
and other Cox-specific diagnostics. Inspired by
[survminer::ggcoxzph](https://search.r-project.org/CRAN/refmans/survminer/html/ggcoxzph.html)
with enhanced visualization and computation optimizations to work in
**bregr**.

## Usage

``` r
br_show_coxph_diagnostics(
  breg,
  idx = 1,
  type = "schoenfeld",
  resid = TRUE,
  se = TRUE,
  point_col = "red",
  point_size = 1,
  point_alpha = 0.6,
  ...
)
```

## Arguments

- breg:

  A regression object with results (must pass
  `assert_breg_obj_with_results()`).

- idx:

  Index or name (focal variable) of the Cox model to plot. Must be a
  single model.

- type:

  Type of Cox diagnostic plot. Options: "schoenfeld" (default for
  Schoenfeld residuals), "martingale" (martingale residuals), "deviance"
  (deviance residuals).

- resid:

  Logical, if TRUE the residuals are included on the plot along with
  smooth fit.

- se:

  Logical, if TRUE confidence bands at two standard errors will be
  added.

- point_col:

  Color for residual points.

- point_size:

  Size for residual points.

- point_alpha:

  Alpha (transparency) for residual points.

- ...:

  Additional arguments passed to
  [survival::cox.zph](https://rdrr.io/pkg/survival/man/cox.zph.html).

## Value

A ggplot2 object or list of plots.

## See also

Other br_show:
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
[`br_show_table()`](https://wanglabcsu.github.io/bregr/reference/br_show_table.md),
[`br_show_table_gt()`](https://wanglabcsu.github.io/bregr/reference/br_show_table_gt.md)

## Examples

``` r
# Create Cox models
mds <- br_pipeline(
  survival::lung,
  y = c("time", "status"),
  x = colnames(survival::lung)[6:10],
  x2 = c("age", "sex"),
  method = "coxph"
)
#> exponentiate estimates of model(s) constructed from coxph method at default

# Show Cox diagnostic plots
p1 <- br_show_coxph_diagnostics(mds, idx = 1)
p1
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'

p2 <- br_show_coxph_diagnostics(mds, type = "martingale")
p2
#> `geom_smooth()` using formula = 'y ~ x'

```
