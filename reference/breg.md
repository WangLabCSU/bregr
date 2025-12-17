# Creates a new breg-class object

**\[stable\]**

Constructs a breg-class object containing regression model
specifications and results.

## Usage

``` r
breg(
  data = NULL,
  y = NULL,
  x = NULL,
  x2 = NULL,
  group_by = NULL,
  config = NULL,
  models = list(),
  results = NULL,
  results_tidy = NULL
)
```

## Arguments

- data:

  A `data.frame` containing modeling data.

- y:

  Character vector of dependent variable names.

- x:

  Character vector of focal independent variable names.

- x2:

  Optional character vector of control variable names.

- group_by:

  Optional character vector specifying grouping column.

- config:

  List of model configuration parameters.

- models:

  List containing fitted model objects.

- results:

  A `data.frame` of model results from
  [`broom.helpers::tidy_plus_plus()`](https://larmarange.github.io/broom.helpers/reference/tidy_plus_plus.html).

- results_tidy:

  A `data.frame` of tidy model results from
  [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html).

## Value

A constructed `breg` object.

## Examples

``` r
obj <- breg()
obj
#> an object of <breg> class with slots:
#> • y (response variable):
#> • x (focal term):
#> • x2 (control term):
#> • group_by:
#> • data: <tibble[,0]>
#> • config: NULL
#> • models: <list: >
#> • results: <df[,0]> with colnames
#> • results_tidy: <df[,0]> with colnames
#> 
#> Focal term(s) are injected into the model one by one,
#> while control term(s) remain constant across all models in the batch.
print(obj, raw = TRUE)
#> <bregr::breg>
#>  @ data        : tibble [0 × 0] (S3: tbl_df/tbl/data.frame)
#>  Named list()
#>  @ y           : NULL
#>  @ x           : NULL
#>  @ x2          : NULL
#>  @ group_by    : NULL
#>  @ config      : NULL
#>  @ models      : list()
#>  @ results     :'data.frame':    0 obs. of  0 variables
#>  @ results_tidy:'data.frame':    0 obs. of  0 variables
#>  @ n_x         : int 0
#>  @ n_x2        : int 0
#> NULL
```
