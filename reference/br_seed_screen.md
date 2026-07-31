# Screen genes via univariate regression

**\[experimental\]**

Step 1 of the SEED pipeline: runs univariate regression for each gene
against one or more clinical indicators. Supports Cox regression
(survival), logistic regression (binary), and Spearman correlation
(ordinal). When multiple indicators are provided, genes significant
across *all* indicators are retained (intersection).

## Usage

``` r
br_seed_screen(
  data,
  y,
  y_type,
  genes = NULL,
  x2 = NULL,
  p_threshold = 0.05,
  n_workers = 1L,
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

- ...:

  Additional arguments passed to
  [`br_pipeline()`](https://wanglabcsu.github.io/bregr/reference/pipeline.md).

## Value

A list with class `breg_seed_screen` containing:

- `results`: data.frame of screening results (gene, indicator, p.value,
  etc.)

- `genes`: character vector of significant genes (intersection across
  indicators)

- `breg`: the underlying `breg` object (for regression-based screening)

- `n_genes_in`, `n_genes_out`: counts for progress reporting

## See also

Other br_seed:
[`br_seed()`](https://wanglabcsu.github.io/bregr/reference/br_seed.md),
[`br_seed_model()`](https://wanglabcsu.github.io/bregr/reference/br_seed_model.md),
[`br_seed_select()`](https://wanglabcsu.github.io/bregr/reference/br_seed_select.md)

## Examples

``` r
# \donttest{
set.seed(123)
n <- 100
# Create gene with real survival signal
test_data <- data.frame(
  time = rexp(n, 0.1),
  status = sample(0:1, n, replace = TRUE),
  GENE1 = rnorm(n),
  GENE2 = rnorm(n, mean = 0.8 * (1:n) / n),
  GENE3 = rnorm(n),
  age = rnorm(n, 60, 10)
)

# Single survival indicator; use relaxed p_threshold for example data
res <- br_seed_screen(test_data,
  y = c("time", "status"),
  y_type = "survival",
  genes = c("GENE1", "GENE2", "GENE3"),
  x2 = "age",
  p_threshold = 0.5
)
#> screening 3 genes against "survival" indicator
#> screening survival (survival)...
#> pre-filtering: no variables were filtered out
#> exponentiate estimates of model(s) constructed from coxph method at default
#> 2 significant genes found
print(res)
#> $results
#> # A tibble: 3 × 9
#>   Focal_variable term  estimate std.error statistic p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 GENE1          GENE1    1.14      0.135     0.965   0.335    0.874      1.48
#> 2 GENE2          GENE2    0.976     0.141    -0.170   0.865    0.740      1.29
#> 3 GENE3          GENE3    1.18      0.145     1.14    0.256    0.888      1.56
#> # ℹ 1 more variable: indicator <chr>
#> 
#> $genes
#> [1] "GENE1" "GENE3"
#> 
#> $breg
#> an object of <breg> class with slots:
#> • y (response variable): time and status
#> • x (focal terms): GENE1, GENE2, and GENE3
#> • x2 (control term): age
#> • group_by:
#> • data: <tibble[,7]>
#> • config: <list: method = "coxph", extra = "">
#> • models: <list: GENE1 = <coxph>, GENE2 = <coxph>, GENE3 = <coxph>>
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
#> • x (focal terms): GENE1, GENE2, and GENE3
#> • x2 (control term): age
#> • group_by:
#> • data: <tibble[,7]>
#> • config: <list: method = "coxph", extra = "">
#> • models: <list: GENE1 = <coxph>, GENE2 = <coxph>, GENE3 = <coxph>>
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
#> [1] "GENE1" "GENE3"
#> 
#> $screen_results$survival$results
#> # A tibble: 3 × 8
#>   Focal_variable term  estimate std.error statistic p.value conf.low conf.high
#>   <chr>          <chr>    <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 GENE1          GENE1    1.14      0.135     0.965   0.335    0.874      1.48
#> 2 GENE2          GENE2    0.976     0.141    -0.170   0.865    0.740      1.29
#> 3 GENE3          GENE3    1.18      0.145     1.14    0.256    0.888      1.56
#> 
#> 
#> 
#> $p_threshold
#> [1] 0.5
#> 
#> $n_genes_in
#> [1] 3
#> 
#> $n_genes_out
#> [1] 2
#> 
#> attr(,"class")
#> [1] "breg_seed_screen" "list"            
# }
```
