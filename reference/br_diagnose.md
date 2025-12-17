# Diagnose regression models

**\[experimental\]**

Universal diagnostic function that performs appropriate diagnostics
based on the model type. For Cox models, tests proportional hazards
assumption using Schoenfeld residuals and provides comprehensive Cox
diagnostics. For other models, provides general diagnostic information.

## Usage

``` r
br_diagnose(breg, idx = NULL, transform = "km", ...)
```

## Arguments

- breg:

  A regression object with results (must pass
  `assert_breg_obj_with_results()`).

- idx:

  Index or name (focal variable) of the model(s) to diagnose. If `NULL`,
  diagnoses all models in the breg object.

- transform:

  Character string specifying how to transform time for Cox PH tests.
  Options are "km" (Kaplan-Meier), "rank", "identity", or a function.

- ...:

  Additional arguments passed to specific diagnostic functions.

## Value

A list containing diagnostic results for each model.

## See also

Other accessors:
[`accessors`](https://wanglabcsu.github.io/bregr/reference/accessors.md),
[`br_predict()`](https://wanglabcsu.github.io/bregr/reference/br_predict.md)

## Examples

``` r
# Create models
mds <- br_pipeline(
  survival::lung,
  y = c("time", "status"),
  x = colnames(survival::lung)[6:10],
  x2 = c("age", "sex"),
  method = "coxph"
)
#> exponentiate estimates of model(s) constructed from coxph method at default

# Diagnose models (includes PH testing for Cox models)
diagnostics <- br_diagnose(mds)
print(diagnostics)
#> 
#> ── Model Diagnostics Summary ───────────────────────────────────────────────────
#> 
#> ── Model: "ph.ecog" (coxph) ──
#> 
#> Sample size: 227
#> Events: 164
#> Log-likelihood: -729.23
#> Concordance: 12544, 7117, 126, 28, 0, 0.637, and 0.025
#> 
#> Proportional Hazards Test (Schoenfeld Residuals):
#> + ph.ecog: χ² = 2.054, df = 1, p = 0.152
#> + age: χ² = 0.188, df = 1, p = 0.665
#> + sex: χ² = 2.305, df = 1, p = 0.129
#> Global test: p = 0.216 - Assumption + SATISFIED
#> 
#> 
#> ── Model: "ph.karno" (coxph) ──
#> 
#> Sample size: 227
#> Events: 164
#> Log-likelihood: -735.078
#> Concordance: 12578, 7145, 65, 28, 0, 0.637, and 0.025
#> 
#> Proportional Hazards Test (Schoenfeld Residuals):
#> x ph.karno: χ² = 8.017, df = 1, p = 0.00463
#> + age: χ² = 0.478, df = 1, p = 0.489
#> + sex: χ² = 3.085, df = 1, p = 0.079
#> Global test: p = 0.0157 - Assumption x VIOLATED
#> 
#> 
#> ── Model: "pat.karno" (coxph) ──
#> 
#> Sample size: 225
#> Events: 162
#> Log-likelihood: -721.587
#> Concordance: 12343, 6957, 57, 26, 0, 0.639, and 0.025
#> 
#> Proportional Hazards Test (Schoenfeld Residuals):
#> x pat.karno: χ² = 4.226, df = 1, p = 0.0398
#> + age: χ² = 0.054, df = 1, p = 0.817
#> + sex: χ² = 2.752, df = 1, p = 0.0971
#> Global test: p = 0.0819 - Assumption + SATISFIED
#> 
#> 
#> ── Model: "meal.cal" (coxph) ──
#> 
#> Sample size: 181
#> Events: 134
#> Log-likelihood: -573.568
#> Concordance: 7761, 5080, 7, 17, 0, 0.604, and 0.029
#> 
#> Proportional Hazards Test (Schoenfeld Residuals):
#> x meal.cal: χ² = 4.65, df = 1, p = 0.031
#> + age: χ² = 0.622, df = 1, p = 0.43
#> + sex: χ² = 1.481, df = 1, p = 0.224
#> Global test: p = 0.0942 - Assumption + SATISFIED
#> 
#> 
#> ── Model: "wt.loss" (coxph) ──
#> 
#> Sample size: 214
#> Events: 152
#> Log-likelihood: -673.056
#> Concordance: 10531, 6672, 10, 22, 0, 0.612, and 0.027
#> 
#> Proportional Hazards Test (Schoenfeld Residuals):
#> + wt.loss: χ² = 0.014, df = 1, p = 0.904
#> + age: χ² = 0.508, df = 1, p = 0.476
#> + sex: χ² = 2.549, df = 1, p = 0.11
#> Global test: p = 0.391 - Assumption + SATISFIED
#> 
```
