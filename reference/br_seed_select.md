# Select genes via Lasso -\> multivariate -\> stepwise regression

**\[experimental\]**

Step 3 of the SEED pipeline: refines the gene set through three
sequential stages — Lasso regularization, multivariate regression, and
stepwise selection. The final gene set is the intersection of genes
retained by all three methods.

## Usage

``` r
br_seed_select(
  data,
  y,
  y_type,
  genes,
  x2 = NULL,
  p_threshold = 0.05,
  lasso_lambda = c("lambda.min", "lambda.1se"),
  lasso_nfolds = 10,
  step_direction = c("both", "backward", "forward"),
  step_k = 2,
  seed = NULL
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

  Character: `"survival"` or `"binary"`. Ordinal indicators are not
  supported for selection/modeling steps.

- genes:

  Character vector of gene names (typically from
  [`br_seed_screen()`](https://wanglabcsu.github.io/bregr/reference/br_seed_screen.md)),
  or a `breg_seed_screen` object from which genes are extracted
  automatically.

- x2:

  Optional character vector of adjustment covariates included in all
  models.

- p_threshold:

  Numeric significance threshold for gene filtering (0 to 1). Default is
  `0.05`.

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

- seed:

  Integer seed for reproducibility of Lasso cross-validation.

## Value

A list with class `breg_seed_select` containing:

- `lasso_genes`, `multivariate_genes`, `stepwise_genes`: gene sets from
  each stage

- `final_genes`: intersection of all three gene sets

- `lasso_model`, `multivariate_model`, `stepwise_model`: fitted model
  objects

## See also

Other br_seed:
[`br_seed()`](https://wanglabcsu.github.io/bregr/reference/br_seed.md),
[`br_seed_model()`](https://wanglabcsu.github.io/bregr/reference/br_seed_model.md),
[`br_seed_screen()`](https://wanglabcsu.github.io/bregr/reference/br_seed_screen.md)

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
  sel <- br_seed_select(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = c("GENE1", "GENE2", "GENE3", "GENE4", "GENE5"),
    x2 = "age",
    seed = 42
  )
  print(sel)
}
#> Step A: Lasso regression (lambda.min) on 5 genes
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
#> retained 3 genes from Lasso
#> Step B: Multivariate regression on 3 Lasso genes
#> exponentiate estimates of model(s) constructed from coxph method at default
#> retained 0 genes from multivariate regression
#> Step C: Stepwise regression (direction = both)
#> Warning: fewer than 2 multivariate genes; using Lasso genes for stepwise model
#> retained 1 genes from stepwise regression
#> ✔ 1 final genes from intersection of all stages
#> $lasso_genes
#> [1] "GENE1" "GENE3" "GENE4"
#> 
#> $multivariate_genes
#> character(0)
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
#> min 0.03886    14   4.779 0.3078       3
#> 1se 0.13025     1   4.794 0.3159       0
#> 
#> $multivariate_model
#> an object of <breg> class with slots:
#> • y (response variable): time and status
#> • x (focal terms): GENE1, GENE3, and GENE4
#> • x2 (control term): age
#> • group_by:
#> • data: <tibble[,9]>
#> • config: <list: method = "coxph", extra = "">
#> • models: <list: GENE1 = <coxph>, GENE3 = <coxph>, GENE4 = <coxph>>
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
#> [1] 0.05
#> 
#> attr(,"class")
#> [1] "breg_seed_select" "list"            
# }
```
