# Show forest plot for model comparison

**\[experimental\]**

Creates a forest plot comparing univariate and multivariate model
results side by side. Each variable shows estimates from both modeling
approaches.

## Usage

``` r
br_show_forest_comparison(comparison, ..., xlim = NULL, rm_controls = TRUE)
```

## Arguments

- comparison:

  A `breg_comparison` object from
  [`br_compare_models()`](https://wanglabcsu.github.io/bregr/reference/br_compare_models.md).

- ...:

  Additional arguments passed to
  [`forestploter::forest()`](https://rdrr.io/pkg/forestploter/man/forest.html).

- xlim:

  Numeric vector of length 2 specifying x-axis limits.

- rm_controls:

  If `TRUE`, show only focal variables (default).

## Value

A forest plot object.

## See also

Other br_compare:
[`br_compare_models()`](https://wanglabcsu.github.io/bregr/reference/br_compare_models.md)

## Examples

``` r
lung <- survival::lung |>
  dplyr::filter(ph.ecog != 3)
lung$ph.ecog <- factor(lung$ph.ecog)

comparison <- br_compare_models(
  lung,
  y = c("time", "status"),
  x = c("ph.ecog", "ph.karno", "pat.karno"),
  x2 = c("age", "sex"),
  method = "coxph"
)
#> Building univariate models...
#> exponentiate estimates of model(s) constructed from coxph method at default
#> Building multivariate model...
#> exponentiate estimates of model(s) constructed from coxph method at default

br_show_forest_comparison(comparison)
```
