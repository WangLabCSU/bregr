# Show regression models with `gtsummary` interface

**\[stable\]**

Provides an interface to visualize the model results with
[**gtsummary**](https://github.com/ddsjoberg/gtsummary/) package in
table format. check
<https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#customize-output>
to see possible output customization.

## Usage

``` r
br_show_table_gt(breg, idx = NULL, ..., tab_spanner = NULL)
```

## Arguments

- breg:

  A regression object with results (must pass
  `assert_breg_obj_with_results()`).

- idx:

  Index or names (focal variables) of the model(s).

- ...:

  Arguments passing to
  [`gtsummary::tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
  excepts `x`.

- tab_spanner:

  (`character`)  
  Character vector specifying the spanning headers. Must be the same
  length as `tbls`. The strings are interpreted with
  [`gt::md`](https://gt.rstudio.com/reference/md.html). Must be same
  length as `tbls` argument. Default is `NULL`, and places a default
  spanning header. If `FALSE`, no header will be placed.

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
[`br_show_table()`](https://wanglabcsu.github.io/bregr/reference/br_show_table.md)

## Examples

``` r
if (rlang::is_installed("gtsummary")) {
  m <- br_pipeline(mtcars,
    y = "mpg",
    x = colnames(mtcars)[2:4],
    x2 = "vs",
    method = "gaussian"
  )
  br_show_table_gt(m)
}


  


Characteristic
```

**cyl**

**disp**

**hp**

**Beta**

**95% CI**

**p-value**

**Beta**

**95% CI**

**p-value**

**Beta**

**95% CI**

**p-value**

cyl

-3.1

-4.2, -2.0

\<0.001

  

  

  

  

  

  

vs

-0.94

-4.8, 2.9

0.6

1.5

-1.7, 4.7

0.4

2.6

-1.3, 6.4

0.2

disp

  

  

  

-0.04

-0.05, -0.02

\<0.001

  

  

  

hp

  

  

  

  

  

  

-0.05

-0.08, -0.03

\<0.001

Abbreviation: CI = Confidence Interval
