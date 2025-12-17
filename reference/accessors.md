# Accessor functions for `breg` objects

**\[stable\]**

These functions provide access to components of `breg` objects, serving
as counterparts to the `br_set_*()` functions. Some functions include
additional arguments for extended functionality.

## Usage

``` r
br_get_data(obj)

br_get_y(obj)

br_get_x(obj)

br_get_n_x(obj)

br_get_x2(obj)

br_get_n_x2(obj)

br_get_group_by(obj)

br_get_config(obj)

br_get_models(obj, idx = NULL, auto_drop = TRUE)

br_get_model(obj, idx)

br_get_model_names(obj)

br_rename_models(obj, new_names)

br_get_results(obj, tidy = FALSE, ...)
```

## Arguments

- obj:

  A `breg` object.

- idx:

  Index or names (focal variables) of the model(s) to return. Default
  returns all.

- auto_drop:

  If `TRUE`, automatically drop the list if only one model is selected.

- new_names:

  Character vector to replace existing model names.

- tidy:

  If `TRUE` return tidy (compact) results, otherwise return
  comprehensive results. The tidy results are obtained from
  [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
  while comprehensive results are obtained from
  [`broom.helpers::tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.html).
  The results can be configured when run with
  [`br_run()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md).

- ...:

  Subset operations passing to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  to filter results.

## Value

Output depends on the function called:

- `br_get_data()` returns a `data.frame`.

- `br_get_y()`, `br_get_x()`, `br_get_x2()` return modeling terms.

- `br_get_n_x()` and `br_get_n_x2()` return the length of terms `x` and
  `x2`.

- `br_get_group_by()` returns variable(s) for group analysis.

- `br_get_config()` returns modeling method and extra arguments.

- `br_get_models()` returns all or a subset of constructed models.

- `br_get_model()` returns a subset of constructed models.

- `br_get_model_names()` returns all model names.

- `br_rename_models()` returns a renamed object.

- `br_get_results()` returns modeling result `data.frame`.

## See also

[pipeline](https://wanglabcsu.github.io/bregr/reference/pipeline.md) for
building `breg` objects.

Other accessors:
[`br_diagnose()`](https://wanglabcsu.github.io/bregr/reference/br_diagnose.md),
[`br_predict()`](https://wanglabcsu.github.io/bregr/reference/br_predict.md)

## Examples

``` r
m <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)
br_get_data(m)
#> # A tibble: 32 × 12
#>    .row_names    mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Mazda RX4    21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2 Mazda RX4 …  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3 Datsun 710   22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4 Hornet 4 D…  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5 Hornet Spo…  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6 Valiant      18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7 Duster 360   14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8 Merc 240D    24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9 Merc 230     22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10 Merc 280     19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
br_get_y(m)
#> [1] "mpg"
br_get_x(m)
#> [1] "cyl"  "disp" "hp"  
br_get_n_x(m)
#> [1] 3
br_get_x2(m)
#> [1] "vs"
br_get_n_x2(m)
#> [1] 1
br_get_group_by(m)
#> NULL
br_get_config(m)
#> $method
#> [1] "gaussian"
#> 
#> $extra
#> [1] ""
#> 
br_get_models(m)
#> $cyl
#> 
#> Call:  stats::glm(formula = mpg ~ cyl + vs, family = stats::gaussian, 
#>     data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl           vs  
#>     39.6250      -3.0907      -0.9391  
#> 
#> Degrees of Freedom: 31 Total (i.e. Null);  29 Residual
#> Null Deviance:       1126 
#> Residual Deviance: 306   AIC: 171.1
#> 
#> $disp
#> 
#> Call:  stats::glm(formula = mpg ~ disp + vs, family = stats::gaussian, 
#>     data = data)
#> 
#> Coefficients:
#> (Intercept)         disp           vs  
#>     27.9493      -0.0369       1.4950  
#> 
#> Degrees of Freedom: 31 Total (i.e. Null);  29 Residual
#> Null Deviance:       1126 
#> Residual Deviance: 308.4     AIC: 171.3
#> 
#> $hp
#> 
#> Call:  stats::glm(formula = mpg ~ hp + vs, family = stats::gaussian, 
#>     data = data)
#> 
#> Coefficients:
#> (Intercept)           hp           vs  
#>    26.96300     -0.05453      2.57622  
#> 
#> Degrees of Freedom: 31 Total (i.e. Null);  29 Residual
#> Null Deviance:       1126 
#> Residual Deviance: 422.7     AIC: 181.4
#> 
br_get_models(m, 1)
#> 
#> Call:  stats::glm(formula = mpg ~ cyl + vs, family = stats::gaussian, 
#>     data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl           vs  
#>     39.6250      -3.0907      -0.9391  
#> 
#> Degrees of Freedom: 31 Total (i.e. Null);  29 Residual
#> Null Deviance:       1126 
#> Residual Deviance: 306   AIC: 171.1
br_get_n_x2(m)
#> [1] 1
br_get_results(m)
#> # A tibble: 6 × 18
#>   Focal_variable term  variable var_label var_class var_type   var_nlevels
#>   <chr>          <chr> <chr>    <chr>     <chr>     <chr>            <int>
#> 1 cyl            cyl   cyl      cyl       numeric   continuous          NA
#> 2 cyl            vs    vs       vs        numeric   continuous          NA
#> 3 disp           disp  disp     disp      numeric   continuous          NA
#> 4 disp           vs    vs       vs        numeric   continuous          NA
#> 5 hp             hp    hp       hp        numeric   continuous          NA
#> 6 hp             vs    vs       vs        numeric   continuous          NA
#> # ℹ 11 more variables: contrasts <chr>, contrasts_type <chr>,
#> #   reference_row <lgl>, label <chr>, n_obs <dbl>, estimate <dbl>,
#> #   std.error <dbl>, statistic <dbl>, p.value <dbl>, conf.low <dbl>,
#> #   conf.high <dbl>
br_get_results(m, tidy = TRUE)
#> # A tibble: 6 × 8
#>   Focal_variable term  estimate std.error statistic   p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 cyl            cyl    -3.09     0.558      -5.54    5.70e-6  -4.18     -2.00  
#> 2 cyl            vs     -0.939    1.98       -0.475   6.38e-1  -4.81      2.94  
#> 3 disp           disp   -0.0369   0.00672    -5.49    6.43e-6  -0.0501   -0.0237
#> 4 disp           vs      1.50     1.65        0.905   3.73e-1  -1.74      4.73  
#> 5 hp             hp     -0.0545   0.0145     -3.77    7.52e-4  -0.0829   -0.0262
#> 6 hp             vs      2.58     1.97        1.31    2.01e-1  -1.28      6.44  
br_get_results(m, tidy = TRUE, term == "cyl")
#> # A tibble: 1 × 8
#>   Focal_variable term  estimate std.error statistic   p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 cyl            cyl      -3.09     0.558     -5.54   5.70e-6    -4.18     -2.00
```
