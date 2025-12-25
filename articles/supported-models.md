# Supported Models

``` r
library(bregr)
#> Welcome to 'bregr' package!
#> =======================================================================
#> You are using bregr version 1.3.1.9000
#> 
#> Project home : https://github.com/WangLabCSU/bregr
#> Documentation: https://wanglabcsu.github.io/bregr/
#> Cite as      : https://doi.org/10.1002/mdr2.70028
#>   Wang, S., Peng, Y., Shu, C., Wang, C., Yang, Y., Zhao, Y., Cui, Y., Hu, D. and Zhou, J.-G. (2025),
#>   bregr: An R Package for Streamlined Batch Processing and Visualization of Biomedical Regression Models. Med Research.
#> =======================================================================
#> 
```

## Default Supported Models

By default, the **bregr** package supports regression models defined in
R’s builtin packages, including **stats** and **survival**. This
encompasses numerous common regression methodologies, such as
generalized linear regressions (linear, logistic) and Cox proportional
hazards regression. A comprehensive list of all supported model methods
can be retrieved using the
[`br_avail_methods()`](https://wanglabcsu.github.io/bregr/reference/avails.md)
function:

``` r
br_avail_methods()
#>  [1] "lm"               "coxph"            "survreg"          "clogit"          
#>  [5] "cch"              "binomial"         "gaussian"         "Gamma"           
#>  [9] "inverse.gaussian" "poisson"          "quasi"            "quasibinomial"   
#> [13] "quasipoisson"     "nls"              "aov"
```

For illustrative purposes, we will utilize the pipeline function instead
of a piped workflow to maintain simplicity.

### Linear Models

``` r
rv <- br_pipeline(
  data = mtcars,
  y = "mpg", x = c("cyl", "disp", "hp"), x2 = "am",
  method = "lm"
)
```

``` r
br_get_results(rv, tidy = TRUE)
#> # A tibble: 6 × 8
#>   Focal_variable term  estimate std.error statistic   p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 cyl            cyl    -2.50     0.361       -6.93   1.28e-7  -3.24     -1.76  
#> 2 cyl            am      2.57     1.29         1.99   5.64e-2  -0.0742    5.21  
#> 3 disp           disp   -0.0369   0.00578     -6.37   5.75e-7  -0.0487   -0.0250
#> 4 disp           am      1.83     1.44         1.28   2.12e-1  -1.10      4.77  
#> 5 hp             hp     -0.0589   0.00786     -7.50   2.92e-8  -0.0750   -0.0428
#> 6 hp             am      5.28     1.08         4.89   3.46e-5   3.07      7.48
```

To focus on the results of focal variables:

``` r
br_get_results(rv, tidy = TRUE) |>
  dplyr::filter(Focal_variable == term)
#> # A tibble: 3 × 8
#>   Focal_variable term  estimate std.error statistic   p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 cyl            cyl    -2.50     0.361       -6.93   1.28e-7  -3.24     -1.76  
#> 2 disp           disp   -0.0369   0.00578     -6.37   5.75e-7  -0.0487   -0.0250
#> 3 hp             hp     -0.0589   0.00786     -7.50   2.92e-8  -0.0750   -0.0428
```

This corresponds to the “gaussian” family defined within generalized
linear models.

``` r
br_pipeline(
  data = mtcars,
  y = "mpg", x = c("cyl", "disp", "hp"), x2 = "am",
  method = "gaussian"
) |>
  br_get_results(tidy = TRUE) |>
  dplyr::filter(Focal_variable == term)
#> # A tibble: 3 × 8
#>   Focal_variable term  estimate std.error statistic   p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 cyl            cyl    -2.50     0.361       -6.93   1.28e-7  -3.21     -1.79  
#> 2 disp           disp   -0.0369   0.00578     -6.37   5.75e-7  -0.0482   -0.0255
#> 3 hp             hp     -0.0589   0.00786     -7.50   2.92e-8  -0.0743   -0.0435
```

### Logistic Models

By modifying the variables (or terms) and method, the presented workflow
can be readily adapted to any other supported method with appropriate
data.

For a logistic regression model:

``` r
br_pipeline(
  data = mtcars,
  y = "vs", x = c("cyl", "disp", "hp"), x2 = "am",
  method = "binomial"
) |>
  suppressWarnings() |>
  br_get_results(tidy = TRUE) |>
  dplyr::filter(Focal_variable == term)
#> exponentiate estimates of model(s) constructed from binomial
#> method at default
#> # A tibble: 3 × 8
#>   Focal_variable term  estimate std.error statistic p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 cyl            cyl   7.97e-10 5771.      -0.00363  0.997    NA     1.60e+143
#> 2 disp           disp  6.07e- 1    0.535   -0.933    0.351    NA     8.31e-  1
#> 3 hp             hp    8.88e- 1    0.0524  -2.28     0.0229    0.764 9.53e-  1
```

### Cox-PH Models

``` r
br_pipeline(
  data = survival::lung,
  y = c("time", "status"),
  x = c("ph.ecog", "ph.karno", "pat.karno", "meal.cal"), x2 = c("age", "sex"),
  method = "coxph"
) |>
  suppressWarnings() |>
  br_get_results(tidy = TRUE) |>
  dplyr::filter(Focal_variable == term)
#> exponentiate estimates of model(s) constructed from coxph method
#> at default
#> # A tibble: 4 × 8
#>   Focal_variable term    estimate std.error statistic p.value conf.low conf.high
#>   <chr>          <chr>      <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 ph.ecog        ph.ecog    1.59   0.114        4.08  4.45e-5    1.27      1.99 
#> 2 ph.karno       ph.kar…    0.987  0.00588     -2.27  2.35e-2    0.975     0.998
#> 3 pat.karno      pat.ka…    0.981  0.00564     -3.38  7.22e-4    0.970     0.992
#> 4 meal.cal       meal.c…    1.000  0.000238    -0.563 5.74e-1    0.999     1.00
```

## Extended Supported Models

Fundamentally, **bregr** leverages `broom.helpers:::tidy_plus_plus()`
for model result processing. Consequently, any model supported by
**broom.helpers** should be compatible with **bregr**, provided the
corresponding dependent packages are installed.

``` r
knitr::kable(
  broom.helpers::supported_models
)
```

| model                                                                  | notes                                                                                                                                                                                                                                                                       |
|:-----------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `betareg::betareg()`                                                   | Use `tidy_parameters()` as `tidy_fun` with `component` argument to control with coefficients to return. [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) does not support the `exponentiate` argument for betareg models, use `tidy_parameters()` instead. |
| `biglm::bigglm()`                                                      |                                                                                                                                                                                                                                                                             |
| `brms::brm()`                                                          | `broom.mixed` package required                                                                                                                                                                                                                                              |
| `cmprsk::crr()`                                                        | Limited support. It is recommended to use `tidycmprsk::crr()` instead.                                                                                                                                                                                                      |
| `fixest::feglm()`                                                      | May fail with R \<= 4.0.                                                                                                                                                                                                                                                    |
| `fixest::femlm()`                                                      | May fail with R \<= 4.0.                                                                                                                                                                                                                                                    |
| `fixest::feNmlm()`                                                     | May fail with R \<= 4.0.                                                                                                                                                                                                                                                    |
| `fixest::feols()`                                                      | May fail with R \<= 4.0.                                                                                                                                                                                                                                                    |
| `gam::gam()`                                                           |                                                                                                                                                                                                                                                                             |
| `geepack::geeglm()`                                                    |                                                                                                                                                                                                                                                                             |
| `glmmTMB::glmmTMB()`                                                   | `broom.mixed` package required                                                                                                                                                                                                                                              |
| `glmtoolbox::glmgee()`                                                 |                                                                                                                                                                                                                                                                             |
| [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html)       | Limited support for categorical variables                                                                                                                                                                                                                                   |
| `lfe::felm()`                                                          |                                                                                                                                                                                                                                                                             |
| [`lme4::glmer.nb()`](https://rdrr.io/pkg/lme4/man/glmer.nb.html)       | `broom.mixed` package required                                                                                                                                                                                                                                              |
| [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html)             | `broom.mixed` package required                                                                                                                                                                                                                                              |
| [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)               | `broom.mixed` package required                                                                                                                                                                                                                                              |
| `logitr::logitr()`                                                     | Requires logitr \>= 0.8.0                                                                                                                                                                                                                                                   |
| [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html)           |                                                                                                                                                                                                                                                                             |
| [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html)               |                                                                                                                                                                                                                                                                             |
| [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html)                 | Use default tidier [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) for smooth terms only, or [`gtsummary::tidy_gam()`](https://www.danieldsjoberg.com/gtsummary/reference/custom_tidiers.html) to include parametric terms                                |
| `mice::mira`                                                           | Limited support. If `mod` is a `mira` object, use `tidy_fun = function(x, ...) {mice::pool(x) &#124;> mice::tidy(...)}`                                                                                                                                                     |
| `mmrm::mmrm()`                                                         |                                                                                                                                                                                                                                                                             |
| `multgee::nomLORgee()`                                                 | Use `tidy_multgee()` as `tidy_fun`.                                                                                                                                                                                                                                         |
| `multgee::ordLORgee()`                                                 | Use `tidy_multgee()` as `tidy_fun`.                                                                                                                                                                                                                                         |
| [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)       |                                                                                                                                                                                                                                                                             |
| `ordinal::clm()`                                                       | Limited support for models with nominal predictors.                                                                                                                                                                                                                         |
| `ordinal::clmm()`                                                      | Limited support for models with nominal predictors.                                                                                                                                                                                                                         |
| `parsnip::model_fit`                                                   | Supported as long as the type of model and the engine is supported.                                                                                                                                                                                                         |
| `plm::plm()`                                                           |                                                                                                                                                                                                                                                                             |
| `pscl::hurdle()`                                                       | Use `tidy_zeroinfl()` as `tidy_fun`.                                                                                                                                                                                                                                        |
| `pscl::zeroinfl()`                                                     | Use `tidy_zeroinfl()` as `tidy_fun`.                                                                                                                                                                                                                                        |
| [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html)           | If several quantiles are estimated, use `tidy_with_broom_or_parameters()` tidier, the default tidier used by `tidy_plus_plus()`.                                                                                                                                            |
| `rstanarm::stan_glm()`                                                 | `broom.mixed` package required                                                                                                                                                                                                                                              |
| [`stats::aov()`](https://rdrr.io/r/stats/aov.html)                     | Reference rows are not relevant for such models.                                                                                                                                                                                                                            |
| [`stats::glm()`](https://rdrr.io/r/stats/glm.html)                     |                                                                                                                                                                                                                                                                             |
| [`stats::lm()`](https://rdrr.io/r/stats/lm.html)                       |                                                                                                                                                                                                                                                                             |
| [`stats::nls()`](https://rdrr.io/r/stats/nls.html)                     | Limited support                                                                                                                                                                                                                                                             |
| `survey::svycoxph()`                                                   |                                                                                                                                                                                                                                                                             |
| `survey::svyglm()`                                                     |                                                                                                                                                                                                                                                                             |
| `survey::svyolr()`                                                     |                                                                                                                                                                                                                                                                             |
| [`survival::cch()`](https://rdrr.io/pkg/survival/man/cch.html)         | Experimental support.                                                                                                                                                                                                                                                       |
| [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)   |                                                                                                                                                                                                                                                                             |
| [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)     |                                                                                                                                                                                                                                                                             |
| [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html) |                                                                                                                                                                                                                                                                             |
| `svyVGAM::svy_vglm()`                                                  | Experimental support. It is recommended to use `tidy_svy_vglm()` as `tidy_fun`.                                                                                                                                                                                             |
| `tidycmprsk::crr()`                                                    |                                                                                                                                                                                                                                                                             |
| `VGAM::vgam()`                                                         | Experimental support. It is recommended to use `tidy_vgam()` as `tidy_fun`.                                                                                                                                                                                                 |
| `VGAM::vglm()`                                                         | Experimental support. It is recommended to use `tidy_vgam()` as `tidy_fun`.                                                                                                                                                                                                 |

In such instances, model methods must be configured using a list
comprising four elements.

For default supported model methods, configurations are prebuilt within
the package and can be inspected using:

``` r
br_avail_method_config("coxph")
#> $f_call
#> survival::coxph
#> 
#> $f_cnst_y
#> function (y) 
#> {
#>     glue::glue("survival::Surv({paste(y, collapse = ', ')})")
#> }
#> <bytecode: 0x555a83926400>
#> <environment: 0x555a786b2f28>
#> 
#> $args_method
#> NULL
#> 
#> $args_data
#> data = data
```

``` r
br_avail_method_config("binomial")
#> $f_call
#> stats::glm
#> 
#> $f_cnst_y
#> NULL
#> 
#> $args_method
#> family = stats::binomial
#> 
#> $args_data
#> data = data
```

- `f_call`: A string representing the caller used to construct the
  model.

- `f_cnst_y`: A function to construct the response term in string format
  within the model. For most models utilizing a single variable, set
  this to `NULL`; however, for models such as survival models that
  typically employ `Surv(time, status)` as the response term, the
  aforementioned function can be utilized to construct the term from
  user-specified `y`.

- `args_method`: A string indicating additional configuration for method
  definition. For generalized linear models (GLM), the method should be
  specified as a family. Generally, this is set to `NULL`.

- `args_data`: A string representing the data argument. If the model
  function does not use `data` as the data input (e.g., if `data2` is
  used instead), adjust it accordingly (e.g., set it to “data2 = data”).
  If not required, set it to “data = data”.

For example, to configure a linear mixed model:

``` r
if (requireNamespace("lme4")) {
  md_config <- list(
    f_call = "lme4::lmer",
    f_cnst_y = NULL,
    args_method = NULL,
    args_data = "data = data"
  )
}
#> Loading required namespace: lme4
```

Subsequently, utilize it in the `method` argument:

``` r
if (requireNamespace("lme4") && requireNamespace("merDeriv") && requireNamespace("broom.mixed")) {
  br_pipeline(
    data = lme4::sleepstudy,
    y = "Reaction",
    x = c("Days", "Subject"), x2 = "(Days | Subject)",
    method = md_config
  ) |>
    # br_get_results(tidy = TRUE)
    br_show_table(export = TRUE, args_table_export = list(format = "html"))
}
#> Loading required namespace: merDeriv
#> Loading required namespace: broom.mixed
#> Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
#> unable to evaluate scaled gradient
#> Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
#>   See ?lme4::convergence and ?lme4::troubleshooting.
```

| Focal_variable | effect   | group    | term                    | estimate | std.error | statistic | conf.int            |
|----------------|----------|----------|-------------------------|----------|-----------|-----------|---------------------|
| Days           | fixed    |          | Days                    | 10.47    | 1.55      | 6.77      | \[ 7.44, 13.50\]    |
| Days           | ran_pars | Subject  | sd\_\_(Intercept)       | 24.74    |           |           |                     |
| Days           | ran_pars | Subject  | cor\_\_(Intercept).Days | 0.07     |           |           |                     |
| Days           | ran_pars | Subject  | sd\_\_Days              | 5.92     |           |           |                     |
| Days           | ran_pars | Residual | sd\_\_Observation       | 25.59    |           |           |                     |
| Subject        | fixed    |          | Subject309              | -22.12   | 65.92     | -0.34     | \[-151.32, 107.08\] |
| Subject        | fixed    |          | Subject310              | -27.05   | 65.92     | -0.41     | \[-156.25, 102.15\] |
| Subject        | fixed    |          | Subject330              | 61.86    | 65.92     | 0.94      | \[ -67.34, 191.06\] |
| Subject        | fixed    |          | Subject331              | 55.95    | 65.92     | 0.85      | \[ -73.25, 185.15\] |
| Subject        | fixed    |          | Subject332              | 30.71    | 65.92     | 0.47      | \[ -98.50, 159.91\] |
| Subject        | fixed    |          | Subject333              | 41.84    | 65.92     | 0.63      | \[ -87.36, 171.04\] |
| Subject        | fixed    |          | Subject334              | 4.27     | 65.92     | 0.06      | \[-124.93, 133.47\] |
| Subject        | fixed    |          | Subject335              | 40.35    | 65.92     | 0.61      | \[ -88.85, 169.55\] |
| Subject        | fixed    |          | Subject337              | 48.30    | 65.92     | 0.73      | \[ -80.90, 177.50\] |
| Subject        | fixed    |          | Subject349              | -21.86   | 65.92     | -0.33     | \[-151.06, 107.34\] |
| Subject        | fixed    |          | Subject350              | -16.38   | 65.92     | -0.25     | \[-145.59, 112.82\] |
| Subject        | fixed    |          | Subject351              | 30.34    | 65.92     | 0.46      | \[ -98.87, 159.54\] |
| Subject        | fixed    |          | Subject352              | 39.33    | 65.92     | 0.60      | \[ -89.87, 168.54\] |
| Subject        | fixed    |          | Subject369              | 19.87    | 65.92     | 0.30      | \[-109.33, 149.07\] |
| Subject        | fixed    |          | Subject370              | -30.51   | 65.92     | -0.46     | \[-159.71, 98.69\]  |
| Subject        | fixed    |          | Subject371              | 20.42    | 65.92     | 0.31      | \[-108.78, 149.62\] |
| Subject        | fixed    |          | Subject372              | 31.99    | 65.92     | 0.49      | \[ -97.21, 161.19\] |
| Subject        | ran_pars | Subject  | sd\_\_(Intercept)       | 45.40    |           |           |                     |
| Subject        | ran_pars | Subject  | cor\_\_(Intercept).Days | 0.31     |           |           |                     |
| Subject        | ran_pars | Subject  | sd\_\_Days              | 11.93    |           |           |                     |
| Subject        | ran_pars | Residual | sd\_\_Observation       | 25.59    |           |           |                     |
