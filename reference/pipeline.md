# Modeling and analysis pipeline

**\[stable\]**

Provides a set of functions for running batch regression analysis.
Combines data setup, model configuration, and execution steps into a
single workflow. Supports both GLM and Cox-PH models with options for
focal/control terms and parallel processing.

## Usage

``` r
br_pipeline(
  data,
  y,
  x,
  method,
  x2 = NULL,
  group_by = NULL,
  n_workers = 1L,
  run_parallel = lifecycle::deprecated(),
  dry_run = FALSE,
  model_args = list(),
  run_args = list(),
  filter_x = FALSE,
  filter_na_prop = 0.8,
  filter_sd_min = 1e-06,
  filter_var_min = 1e-06,
  filter_min_levels = 2
)

br_set_y(obj, y)

br_set_x(
  obj,
  ...,
  filter_x = FALSE,
  filter_na_prop = 0.8,
  filter_sd_min = 1e-06,
  filter_var_min = 1e-06,
  filter_min_levels = 2
)

br_set_x2(obj, ...)

br_set_model(obj, method, ...)

br_run(
  obj,
  ...,
  group_by = NULL,
  n_workers = 1L,
  run_parallel = lifecycle::deprecated()
)
```

## Arguments

- data:

  A `data.frame` containing all necessary variables for analysis. Column
  names should follow R's naming conventions.

- y:

  Character vector specifying dependent variables (response variables).
  For GLM models, this is typically a single character (e.g.,
  `"outcome"`). For Cox-PH models, it should be a length-2 vector in the
  format `c("time", "status")`.

- x:

  Character vector specifying focal independent terms (predictors).

- method:

  Method for model construction. A name or a list specifying custom
  model setting. A string representing a complex method setting is
  acceptable, e.g., 'quasi(variance = "mu", link = "log")'. Or a list
  with 4 elements, see
  [`br_avail_method_config()`](https://wanglabcsu.github.io/bregr/reference/avails.md)
  for examples.

- x2:

  Character vector specifying control independent terms (predictors,
  optional).

- group_by:

  A string specifying the group by column.

- n_workers, run_parallel:

  Integer, indicating integer number of workers to launch, default is
  `1L`. When `>1`, run modeling code in parallel in the background.

- dry_run:

  If `TRUE`, generates modeling descriptions without executing the run.
  Use this to inspect the construction first.

- model_args:

  A list of arguments passed to `br_set_model()`.

- run_args:

  A list of arguments passed to `br_run()`.

- filter_x:

  Logical, whether to enable pre-filtering of focal variables. Default
  is `FALSE`.

- filter_na_prop:

  Numeric, maximum proportion of NA values allowed for a variable.
  Default is `0.8`.

- filter_sd_min:

  Numeric, minimum standard deviation required for a variable. Default
  is `1e-6`.

- filter_var_min:

  Numeric, minimum variance required for a variable. Default is `1e-6`.

- filter_min_levels:

  Numeric, minimum number of unique levels required for categorical
  variables. Default is `2`.

- obj:

  An object of class `breg`.

- ...:

  Additional arguments depending on the called function.

  - `br_set_x()` for passing focal terms as characters.

  - `br_set_x2()` for passing control terms as characters.

  - `br_set_model()` for passing other configurations for modeling.

  - `br_run()` for passing other configurations for obtaining modeling
    results with
    [`broom.helpers::tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.html).
    e.g., The default value for `exponentiate` is `FALSE` (coefficients
    are not exponentiated). For logistic, and Cox-PH regressions models,
    `exponentiate` is set to `TRUE` at default.

## Value

An object of class `breg` with input values added to corresponding
slot(s). For `br_run()`, the returned object is a `breg` object with
results added to the slots `@results` and `@results_tidy`, note that
`@models` is updated to a list of constructed model object (See
[accessors](https://wanglabcsu.github.io/bregr/reference/accessors.md)).

## Details

Please note the difference between
[variables](https://easystats.github.io/insight/#variables) and
[terms](https://easystats.github.io/insight/#terms), e.g.,
`x + poly(x, 2)` has *one* variable `x`, but *two* terms `x` and
`poly(x, 2)`.

### Global options

**bregr** supported global options can be set with
[`options()`](https://rdrr.io/r/base/options.html). Currently they are
used in `br_run()`.

- `bregr.save_model`: If `TRUE`, saves models to local disk.

- `bregr.path`: A path for saving models, defaults to using a temporary
  directory.

## Functions

- `br_pipeline()`: All-in-one end to end wrapper to run the regression
  analysis in batch. Which could be splitted into the following steps

- `br_set_y()`: Set dependent variables for model construction.

- `br_set_x()`: Set focal terms for model construction.

- `br_set_x2()`: Set control terms for model construction (Optional in
  pipeline).

- `br_set_model()`: Set model configurations.

- `br_run()`: Run the regression analysis in batch.

## See also

[accessors](https://wanglabcsu.github.io/bregr/reference/accessors.md)
for accessing `breg` object properties.

## Examples

``` r
library(bregr)
# 1. Pipeline -------------------------
# 1.1. A single linear model ----------
m <- breg(mtcars) |> # set model data
  br_set_y("mpg") |> # set dependent variable
  br_set_x("qsec") |> # set focal variables
  br_set_model("gaussian") |> # set model
  br_run() # run analysis

# get model tidy result
br_get_results(m, tidy = TRUE)
#> # A tibble: 1 × 8
#>   Focal_variable term  estimate std.error statistic p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 qsec           qsec      1.41     0.559      2.53  0.0171    0.316      2.51
# or m@results_tidy

# compare with R's built-in function
lm(mpg ~ qsec, data = mtcars) |> summary()
#> 
#> Call:
#> lm(formula = mpg ~ qsec, data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -9.8760 -3.4539 -0.7203  2.2774 11.6491 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)  
#> (Intercept)  -5.1140    10.0295  -0.510   0.6139  
#> qsec          1.4121     0.5592   2.525   0.0171 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 5.564 on 30 degrees of freedom
#> Multiple R-squared:  0.1753, Adjusted R-squared:  0.1478 
#> F-statistic: 6.377 on 1 and 30 DF,  p-value: 0.01708
#> 
# 1.2. Batch linear model -------------
# control variables are injected in all constructed models
# focal variables are injected in constructed models one by one
m2 <- breg(mtcars) |>
  br_set_y("mpg") |>
  br_set_x(colnames(mtcars)[2:4]) |> # set focal variables
  br_set_x2("vs") |> # set control variables
  br_set_model("gaussian") |>
  br_run()

# \donttest{
# 1.3. Group by model -------------
m3 <- breg(mtcars) |>
  br_set_y("mpg") |>
  br_set_x("cyl") |>
  br_set_x2("wt") |> # set control variables
  br_set_model("gaussian") |>
  br_run(group_by = "am")

# 2. All-in-one pipeline wrapper ---
m4 <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)

# 3. Customized model -----------
dt <- data.frame(x = rnorm(100))
dt$y <- rpois(100, exp(1 + dt$x))

m5 <- breg(dt) |>
  br_set_y("y") |>
  br_set_x("x") |>
  br_set_model(method = 'quasi(variance = "mu", link = "log")') |>
  br_run()
#> Warning: nonstandard method `quasi(variance = "mu", link = "log")` passed to
#> `stats::glm()`, double-check if it's correct
# }
```
