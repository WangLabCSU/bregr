# Predict method for breg objects

**\[experimental\]**

Generate predictions from fitted models in a `breg` object. For Cox
regression models, returns linear predictors (log relative hazard). For
other models, returns predicted values.

## Usage

``` r
br_predict(obj, newdata = NULL, idx = NULL, type = NULL)
```

## Arguments

- obj:

  A `breg` object with fitted models.

- newdata:

  Optional data frame for predictions. If NULL, uses original data.

- idx:

  Model index, an integer or string.

- type:

  Type of prediction. For Cox models: "lp" (linear predictor, default)
  or "risk" (relative risk). For other models: "response" (default) or
  "link".

## Value

Typically, a numeric vector of predictions.

## See also

Other accessors:
[`accessors`](https://wanglabcsu.github.io/bregr/reference/accessors.md),
[`br_diagnose()`](https://wanglabcsu.github.io/bregr/reference/br_diagnose.md)

## Examples

``` r
# Cox regression example
if (requireNamespace("survival", quietly = TRUE)) {
  lung <- survival::lung |> dplyr::filter(ph.ecog != 3)
  mds <- br_pipeline(
    lung,
    y = c("time", "status"),
    x = c("age", "ph.ecog"),
    x2 = "sex",
    method = "coxph"
  )
  scores <- br_predict(mds)
  head(scores)
}
#> exponentiate estimates of model(s) constructed from coxph method at default
#> `idx` not set, use the first model
#> `type` is not specified, use lp for the model
#>          1          2          3          4          5          6 
#> 0.39429054 0.29301268 0.09045697 0.10733661 0.15797554 0.39429054 
```
