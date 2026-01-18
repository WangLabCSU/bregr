# Save Models to Local Disk

``` r
library(bregr)
#> Welcome to 'bregr' package!
#> =======================================================================
#> You are using bregr version 1.4.0
#> 
#> Project home : https://github.com/WangLabCSU/bregr
#> Documentation: https://wanglabcsu.github.io/bregr/
#> Cite as      : https://doi.org/10.1002/mdr2.70028
#>   Wang, S., Peng, Y., Shu, C., Wang, C., Yang, Y., Zhao, Y., Cui, Y., Hu, D. and Zhou, J.-G. (2025),
#>   bregr: An R Package for Streamlined Batch Processing and Visualization of Biomedical Regression Models. Med Research.
#> =======================================================================
#> 
```

Two global options have been introduced to control whether models are
saved as local files (`bregr.save_model`, default is `FALSE`) and where
they should be saved (`bregr.path`, default uses a temporary path).

``` r
options(bregr.save_model = TRUE)
# Set model save path if necessary
# options(bregr.path = "/model/to/path")

m <- breg(mtcars) |>
  br_set_y("mpg") |>
  br_set_x(colnames(mtcars)[2:4]) |>
  br_set_x2("vs") |>
  br_set_model("gaussian") |>
  br_run()
#> model save is enabled with result path /tmp/Rtmpja7GzH
#> This message is displayed once per session.

options(bregr.save_model = FALSE)
```

In summary, the models have been saved to a unique path. We can verify
this by examining the model objects:

``` r
m@models
#> $cyl
#> /tmp/Rtmpja7GzH/e5d0fdf2-f43f-11f0-8309-7ced8d2a83ef.qs
#> 
#> $disp
#> /tmp/Rtmpja7GzH/e60104b6-f43f-11f0-8309-7ced8d2a83ef.qs
#> 
#> $hp
#> /tmp/Rtmpja7GzH/e62decd8-f43f-11f0-8309-7ced8d2a83ef.qs
```

We can retrieve the saved models using the following commands:

``` r
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
br_get_models(m, c(1, 3))
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
br_get_models(m, "cyl")
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
br_get_models(m, c("cyl", "hp"))
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
```

The modeling results should remain consistent, regardless of whether
they are saved or not.

Run without saving models.

``` r
m2 <- breg(mtcars) |>
  br_set_y("mpg") |>
  br_set_x(colnames(mtcars)[2:4]) |>
  br_set_x2("vs") |>
  br_set_model("gaussian") |>
  br_run()
m2@models
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
```

``` r
all.equal(m, m2)
#> [1] TRUE
```
