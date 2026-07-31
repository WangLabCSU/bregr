# Run the full SEED gene selection pipeline

**\[experimental\]**

Implements the complete SEED (Selection of Essential prognostic genes
from Expression Data) pipeline for identifying cancer prognosis target
genes from RNA-seq data without control samples.

The pipeline runs four steps sequentially:

1.  **Preliminary screening**: Univariate regression
    (Cox/logistic/Spearman) per gene against clinical indicators

2.  **Enrichment analysis** (optional): Pathway enrichment to refine
    gene set

3.  **Gene selection**: Lasso -\> multivariate -\> stepwise regression

4.  **Model development**: Build risk score from final gene set

Based on Yang, H. et al. (2025) *Cancer Letters*, DOI:
10.1016/j.canlet.2025.217960.

## Usage

``` r
br_seed(
  data,
  y,
  y_type,
  genes = NULL,
  x2 = NULL,
  p_threshold = 0.05,
  n_workers = 1L,
  enrich = NULL,
  do_select = TRUE,
  lasso_lambda = c("lambda.min", "lambda.1se"),
  lasso_nfolds = 10,
  step_direction = c("both", "backward", "forward"),
  step_k = 2,
  risk_scale = c("none", "zscore"),
  seed = NULL,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing clinical variables and gene expression
  columns side by side. Gene columns must be numeric.

- y:

  Character vector specifying clinical indicators. For survival: a
  length-2 vector `c("time", "status")`. For binary/ordinal: a single
  column name. When screening against multiple indicators, pass a named
  list, e.g., `list(OS = c("time", "status"), ORR = "response")`.

- y_type:

  Character vector of indicator types: `"survival"`, `"binary"`, or
  `"ordinal"`. Must match `y` in length.

- genes:

  Character vector of gene column names. If `NULL`, all numeric columns
  not in `y` or `x2` are used.

- x2:

  Optional character vector of adjustment covariates included in all
  models.

- p_threshold:

  Numeric significance threshold for gene filtering (0 to 1). Default is
  `0.05`.

- n_workers:

  Integer number of parallel workers for batch regression. Passed to
  [`br_pipeline()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md).

- enrich:

  Optional enrichment method. One of:

  - `NULL` (default): skip enrichment

  - A string naming an `org.*.eg.db` package (e.g., `"org.Hs.eg.db"`)

  - A function taking a character vector of gene symbols and returning a
    character vector of enriched genes

- do_select:

  Logical. If `TRUE` (default), run gene selection step.

- lasso_lambda:

  Lambda selection criterion: `"lambda.min"` (default) or
  `"lambda.1se"`.

- lasso_nfolds:

  Number of folds for cross-validation in Lasso. Default 10.

- step_direction:

  Direction for stepwise selection: `"both"` (default), `"backward"`, or
  `"forward"`.

- step_k:

  The penalty multiplier for AIC in stepwise selection. Default 2
  (standard AIC).

- risk_scale:

  How to scale expression values before computing risk score. `"none"`
  (default) uses raw expression; `"zscore"` centers and scales.

- seed:

  Integer seed for reproducibility of Lasso cross-validation.

- ...:

  Additional arguments passed to
  [`br_pipeline()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md).

## Value

A list with class `breg_seed` containing:

- `screen`: `breg_seed_screen` object from Step 1

- `select`: `breg_seed_select` object from Step 3 (if
  `do_select = TRUE`)

- `model`: `breg_seed_model` object from Step 4

- `risk_score`: numeric vector of risk scores per sample

## See also

Other br_seed:
[`br_seed_model()`](https://wanglabcsu.github.io/bregr/reference/br_seed_model.md),
[`br_seed_screen()`](https://wanglabcsu.github.io/bregr/reference/br_seed_screen.md),
[`br_seed_select()`](https://wanglabcsu.github.io/bregr/reference/br_seed_select.md)

## Examples

``` r
# \donttest{
set.seed(123)
n <- 100
test_data <- data.frame(
  time = rexp(n, 0.1),
  status = sample(0:1, n, replace = TRUE),
  GENE1 = rnorm(n),
  GENE2 = rnorm(n, mean = 0.8 * (1:n) / n),
  GENE3 = rnorm(n, mean = -0.5 * (1:n) / n),
  GENE4 = rnorm(n),
  GENE5 = rnorm(n),
  age = rnorm(n, 60, 10)
)

if (rlang::is_installed("glmnet")) {
  res <- br_seed(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = c("GENE1", "GENE2", "GENE3", "GENE4", "GENE5"),
    x2 = "age",
    p_threshold = 0.3,
    seed = 42
  )
  print(res)
}
#> 
#> ── SEED Pipeline ───────────────────────────────────────────────────────────────
#> 
#> ── Step 1: Preliminary Screening ──
#> 
#> screening 5 genes against "survival" indicator
#> screening survival (survival)...
#> pre-filtering: no variables were filtered out
#> exponentiate estimates of model(s) constructed from coxph method at default
#> 2 significant genes found
#> Step 2: Enrichment Analysis (skipped)
#> 
#> ── Step 3: Gene Selection ──
#> 
#> Step A: Lasso regression (lambda.min) on 2 genes
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> Warning: Starting in glmnet 5.1, the default Cox tie-handling method will change from 'breslow' to 'efron' (matching survival::coxph). To silence this message and lock in the v5.0 default, pass cox.ties = 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties = 'efron'.
#> retained 2 genes from Lasso
#> Step B: Multivariate regression on 2 Lasso genes
#> exponentiate estimates of model(s) constructed from coxph method at default
#> retained 2 genes from multivariate regression
#> Step C: Stepwise regression (direction = both)
#> retained 1 genes from stepwise regression
#> ✔ 1 final genes from intersection of all stages
#> 
#> ── Step 4: Model Development ──
#> 
#> building final coxph model with 1 genes
#> exponentiate estimates of model(s) constructed from coxph method at default
#> $screen
#> $results
#> # A tibble: 5 × 9
#>   Focal_variable term  estimate std.error statistic p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 GENE1          GENE1    1.15      0.131    1.08    0.282     0.891      1.49
#> 2 GENE2          GENE2    0.987     0.140   -0.0898  0.928     0.750      1.30
#> 3 GENE3          GENE3    1.15      0.149    0.954   0.340     0.860      1.55
#> 4 GENE4          GENE4    1.34      0.156    1.87    0.0616    0.986      1.82
#> 5 GENE5          GENE5    1.11      0.142    0.760   0.447     0.843      1.47
#> # ℹ 1 more variable: indicator <chr>
#> 
#> $genes
#> [1] "GENE1" "GENE4"
#> 
#> $breg
#> an object of <breg> class with slots:
#> • y (response variable): time and status
#> • x (focal terms): GENE1, GENE2, GENE3, GENE4, and GENE5
#> • x2 (control term): age
#> • group_by:
#> • data: <tibble[,9]>
#> • config: <list: method = "coxph", extra = "">
#> • models: <list: GENE1 = <coxph>, GENE2 = <coxph>, GENE3 = <coxph>, GENE4 =
#> <coxph>, and  GENE5 = <coxph>>
#> • results: <brm.hlpr[,21]> with colnames Focal_variable, term, variable,
#> var_label, var_class, var_type, var_nlevels, contrasts, contrasts_type,
#> reference_row, label, n_obs, n_ind, n_event, exposure, estimate, std.error,
#> statistic, …, conf.low, and conf.high
#> • results_tidy: <tibble[,8]> with colnames Focal_variable, term, estimate,
#> std.error, statistic, p.value, conf.low, and conf.high
#> 
#> Focal term(s) are injected into the model one by one,
#> while control term(s) remain constant across all models in the batch.
#> 
#> $screen_results
#> $screen_results$survival
#> $screen_results$survival$breg
#> an object of <breg> class with slots:
#> • y (response variable): time and status
#> • x (focal terms): GENE1, GENE2, GENE3, GENE4, and GENE5
#> • x2 (control term): age
#> • group_by:
#> • data: <tibble[,9]>
#> • config: <list: method = "coxph", extra = "">
#> • models: <list: GENE1 = <coxph>, GENE2 = <coxph>, GENE3 = <coxph>, GENE4 =
#> <coxph>, and  GENE5 = <coxph>>
#> • results: <brm.hlpr[,21]> with colnames Focal_variable, term, variable,
#> var_label, var_class, var_type, var_nlevels, contrasts, contrasts_type,
#> reference_row, label, n_obs, n_ind, n_event, exposure, estimate, std.error,
#> statistic, …, conf.low, and conf.high
#> • results_tidy: <tibble[,8]> with colnames Focal_variable, term, estimate,
#> std.error, statistic, p.value, conf.low, and conf.high
#> 
#> Focal term(s) are injected into the model one by one,
#> while control term(s) remain constant across all models in the batch.
#> 
#> $screen_results$survival$genes
#> [1] "GENE1" "GENE4"
#> 
#> $screen_results$survival$results
#> # A tibble: 5 × 8
#>   Focal_variable term  estimate std.error statistic p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 GENE1          GENE1    1.15      0.131    1.08    0.282     0.891      1.49
#> 2 GENE2          GENE2    0.987     0.140   -0.0898  0.928     0.750      1.30
#> 3 GENE3          GENE3    1.15      0.149    0.954   0.340     0.860      1.55
#> 4 GENE4          GENE4    1.34      0.156    1.87    0.0616    0.986      1.82
#> 5 GENE5          GENE5    1.11      0.142    0.760   0.447     0.843      1.47
#> 
#> 
#> 
#> $p_threshold
#> [1] 0.3
#> 
#> $n_genes_in
#> [1] 5
#> 
#> $n_genes_out
#> [1] 2
#> 
#> attr(,"class")
#> [1] "breg_seed_screen" "list"            
#> 
#> $select
#> $lasso_genes
#> [1] "GENE1" "GENE4"
#> 
#> $multivariate_genes
#> [1] "GENE1" "GENE4"
#> 
#> $stepwise_genes
#> [1] "GENE4"
#> 
#> $final_genes
#> [1] "GENE4"
#> 
#> $lasso_model
#> 
#> Call:  glmnet::cv.glmnet(x = x_mat, y = y_surv, nfolds = nfolds, family = family,      alpha = 1, standardize = TRUE) 
#> 
#> Measure: Partial Likelihood Deviance 
#> 
#>      Lambda Index Measure     SE Nonzero
#> min 0.00287    42   4.765 0.3021       2
#> 1se 0.13025     1   4.794 0.3159       0
#> 
#> $multivariate_model
#> an object of <breg> class with slots:
#> • y (response variable): time and status
#> • x (focal terms): GENE1 and GENE4
#> • x2 (control term): age
#> • group_by:
#> • data: <tibble[,9]>
#> • config: <list: method = "coxph", extra = "">
#> • models: <list: GENE1 = <coxph>, GENE4 = <coxph>>
#> • results: <brm.hlpr[,21]> with colnames Focal_variable, term, variable,
#> var_label, var_class, var_type, var_nlevels, contrasts, contrasts_type,
#> reference_row, label, n_obs, n_ind, n_event, exposure, estimate, std.error,
#> statistic, …, conf.low, and conf.high
#> • results_tidy: <tibble[,8]> with colnames Focal_variable, term, estimate,
#> std.error, statistic, p.value, conf.low, and conf.high
#> 
#> Focal term(s) are injected into the model one by one,
#> while control term(s) remain constant across all models in the batch.
#> 
#> $stepwise_model
#> Call:
#> survival::coxph(formula = survival::Surv(time, status) ~ GENE4, 
#>     data = data)
#> 
#>         coef exp(coef) se(coef)     z      p
#> GENE4 0.2766    1.3187   0.1562 1.771 0.0765
#> 
#> Likelihood ratio test=3.31  on 1 df, p=0.06872
#> n= 100, number of events= 52 
#> 
#> $p_threshold
#> [1] 0.3
#> 
#> attr(,"class")
#> [1] "breg_seed_select" "list"            
#> 
#> $model
#> $model
#> an object of <breg> class with slots:
#> • y (response variable): time and status
#> • x (focal term): GENE4
#> • x2 (control term): age
#> • group_by:
#> • data: <tibble[,9]>
#> • config: <list: method = "coxph", extra = "">
#> • models: <list: GENE4 = <coxph>>
#> • results: <brm.hlpr[,21]> with colnames Focal_variable, term, variable,
#> var_label, var_class, var_type, var_nlevels, contrasts, contrasts_type,
#> reference_row, label, n_obs, n_ind, n_event, exposure, estimate, std.error,
#> statistic, …, conf.low, and conf.high
#> • results_tidy: <tibble[,8]> with colnames Focal_variable, term, estimate,
#> std.error, statistic, p.value, conf.low, and conf.high
#> 
#> Focal term(s) are injected into the model one by one,
#> while control term(s) remain constant across all models in the batch.
#> 
#> $coefficients
#> # A tibble: 1 × 2
#>   term  estimate
#>   <chr>    <dbl>
#> 1 GENE4     1.34
#> 
#> $risk_score
#>   [1]  0.95446631  1.45275214 -2.97974729  1.65486499 -1.66203120  0.60903595
#>   [7]  0.88375459 -0.26769639 -0.86394930  0.22140116  0.58767464  1.18293651
#>  [13] -2.74853006 -2.19147131  1.91562295  1.40166594  0.58294752  0.95778099
#>  [19]  1.22829869 -3.56356016  1.48690493 -0.64950493  0.30884660 -0.39528113
#>  [25]  1.16775262 -0.46668116  0.69439044 -0.52321301 -1.46348213  1.62046988
#>  [31]  0.99222787  2.30916591  0.08725543  1.50662581  2.64552006 -0.37696639
#>  [37] -1.77172215 -0.32054433 -0.28664824  0.20313352  2.29315251 -0.43677832
#>  [43]  0.49953517 -0.30491898  0.02738799  0.42059220  1.77877125  0.16247196
#>  [49]  0.95465246  1.04306467  1.22508236 -0.76924048  2.17875132 -0.51018476
#>  [55] -0.14166824  1.88033174  1.73306262 -1.45973856 -1.16923386 -1.81876619
#>  [61]  0.24353334  0.22075813  0.48762955  0.73946047 -0.80606674 -1.33078070
#>  [67]  1.37509075  1.00583607 -2.02110551 -0.12742334 -1.19987095 -2.77319057
#>  [73]  0.20104383 -0.10608188 -0.13039884  0.28947574  1.18181471  0.27534022
#>  [79] -0.82554301 -0.98405761 -0.17651289  0.41518085 -1.39236038 -0.24683006
#>  [85]  1.29538334 -0.14501083 -0.93533870 -0.36955120  1.49275926  0.73662969
#>  [91]  1.65618056  0.18628259  0.54944848 -0.74789648  0.81072431 -0.67809184
#>  [97] -1.90244927  0.17141070  2.60592223  1.07260024
#> 
#> $risk_score_formula
#> [1] "Risk Score = 1.3392 * GENE4"
#> 
#> $risk_scale
#> [1] "none"
#> 
#> $n_genes
#> [1] 1
#> 
#> $n_samples
#> [1] 100
#> 
#> attr(,"class")
#> [1] "breg_seed_model" "list"           
#> 
#> $risk_score
#>   [1]  0.95446631  1.45275214 -2.97974729  1.65486499 -1.66203120  0.60903595
#>   [7]  0.88375459 -0.26769639 -0.86394930  0.22140116  0.58767464  1.18293651
#>  [13] -2.74853006 -2.19147131  1.91562295  1.40166594  0.58294752  0.95778099
#>  [19]  1.22829869 -3.56356016  1.48690493 -0.64950493  0.30884660 -0.39528113
#>  [25]  1.16775262 -0.46668116  0.69439044 -0.52321301 -1.46348213  1.62046988
#>  [31]  0.99222787  2.30916591  0.08725543  1.50662581  2.64552006 -0.37696639
#>  [37] -1.77172215 -0.32054433 -0.28664824  0.20313352  2.29315251 -0.43677832
#>  [43]  0.49953517 -0.30491898  0.02738799  0.42059220  1.77877125  0.16247196
#>  [49]  0.95465246  1.04306467  1.22508236 -0.76924048  2.17875132 -0.51018476
#>  [55] -0.14166824  1.88033174  1.73306262 -1.45973856 -1.16923386 -1.81876619
#>  [61]  0.24353334  0.22075813  0.48762955  0.73946047 -0.80606674 -1.33078070
#>  [67]  1.37509075  1.00583607 -2.02110551 -0.12742334 -1.19987095 -2.77319057
#>  [73]  0.20104383 -0.10608188 -0.13039884  0.28947574  1.18181471  0.27534022
#>  [79] -0.82554301 -0.98405761 -0.17651289  0.41518085 -1.39236038 -0.24683006
#>  [85]  1.29538334 -0.14501083 -0.93533870 -0.36955120  1.49275926  0.73662969
#>  [91]  1.65618056  0.18628259  0.54944848 -0.74789648  0.81072431 -0.67809184
#>  [97] -1.90244927  0.17141070  2.60592223  1.07260024
#> 
#> $risk_score_formula
#> [1] "Risk Score = 1.3392 * GENE4"
#> 
#> attr(,"class")
#> [1] "breg_seed" "list"     
# }
```
