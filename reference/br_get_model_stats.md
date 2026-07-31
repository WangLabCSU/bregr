# Get model-level statistics in tidy format

**\[experimental\]**

Extracts per-model summary statistics (N, events, C-index, AIC, PH test,
etc.) and returns them as a tidy data.frame. This complements
[`br_get_results()`](https://wanglabcsu.github.io/bregr/reference/accessors.md)
which provides per-term estimates.

## Usage

``` r
br_get_model_stats(breg, idx = NULL)
```

## Arguments

- breg:

  A regression object with results.

- idx:

  Index or name (focal variable) of the model(s). If `NULL`, returns
  stats for all models.

## Value

A data.frame with one row per model. Columns depend on model type:

- Cox models: `model`, `n`, `events`, `c_index`, `aic`, `ph_test_p`,
  `lr_test_p`

- GLM models: `model`, `n`, `aic`, `deviance`, `df_residual`

- LM models: `model`, `n`, `r_squared`, `adj_r_squared`, `df_residual`

## See also

Other accessors:
[`accessors`](https://wanglabcsu.github.io/bregr/reference/accessors.md),
[`br_diagnose()`](https://wanglabcsu.github.io/bregr/reference/br_diagnose.md),
[`br_predict()`](https://wanglabcsu.github.io/bregr/reference/br_predict.md)

## Examples

``` r
m <- br_pipeline(survival::lung,
  y = c("time", "status"),
  x = colnames(survival::lung)[6:10],
  method = "coxph"
)
#> exponentiate estimates of model(s) constructed from coxph method at default
br_get_model_stats(m)
#>       model   n events   c_index      aic lr_test_p   ph_test_p
#> 1   ph.ecog 227    164 0.6044625 1473.393        NA 0.134113095
#> 2  ph.karno 227    164 0.5977865 1483.410        NA 0.008171405
#> 3 pat.karno 225    162 0.6072739 1457.029        NA 0.051965795
#> 4  meal.cal 181    134 0.5324953 1159.499        NA 0.043029792
#> 5   wt.loss 214    152 0.5250973 1362.734        NA 0.829888640
```
