# Build a risk score model from selected genes

**\[experimental\]**

Step 4 of the SEED pipeline: builds a final regression model from the
selected gene set and computes a risk score for each sample. The risk
score is the linear predictor (gene expression x coefficient) summed
across genes.

## Usage

``` r
br_seed_model(
  data,
  y,
  y_type,
  genes,
  x2 = NULL,
  risk_scale = c("none", "zscore")
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

  Character vector of final gene names, or a `breg_seed_select` object
  from which `final_genes` are extracted.

- x2:

  Optional character vector of adjustment covariates included in all
  models.

- risk_scale:

  How to scale expression values before computing risk score. `"none"`
  (default) uses raw expression; `"zscore"` centers and scales.

## Value

A list with class `breg_seed_model` containing:

- `model`: `breg` object with the final model

- `coefficients`: data.frame of gene coefficients

- `risk_score`: numeric vector, one per sample

- `risk_score_formula`: character string representation of the risk
  formula

## See also

Other br_seed:
[`br_seed()`](https://wanglabcsu.github.io/bregr/reference/br_seed.md),
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
  age = rnorm(n, 60, 10)
)

mod <- br_seed_model(test_data,
  y = c("time", "status"),
  y_type = "survival",
  genes = c("GENE1", "GENE2", "GENE3"),
  x2 = "age"
)
#> building final coxph model with 3 genes
#> exponentiate estimates of model(s) constructed from coxph method at default
print(mod)
#> $model
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
#> $coefficients
#> # A tibble: 3 × 2
#>   term  estimate
#>   <chr>    <dbl>
#> 1 GENE1    1.14 
#> 2 GENE2    0.976
#> 3 GENE3    1.19 
#> 
#> $risk_score
#>   [1] -3.46594173  1.01638050  3.52851115 -2.52174213  0.78255788  0.36489951
#>   [7]  1.67382778 -2.75412163 -2.04441881 -0.81819262 -1.62496708 -1.20310434
#>  [13]  0.60676661 -1.18060013  1.76781938 -2.01738872 -0.43498255 -0.88799763
#>  [19]  0.39533287  0.02904805  2.22559234  0.19290092 -1.89377066  1.18536458
#>  [25]  0.49209874 -0.84927367 -1.01321625  6.23410605  2.79178649  1.65612171
#>  [31]  1.10317580 -3.33316240  1.29143674  0.02099067  3.52420744 -0.88664051
#>  [37]  0.14489888  2.11632029  0.66508756 -2.31275861 -0.89811391  0.67639914
#>  [43]  0.97784368 -2.54982166 -2.71461874  2.46979994  0.27381282 -2.41350819
#>  [49]  0.50429156  1.64153223  2.11820677  0.44156447  0.22416575  0.10774253
#>  [55]  0.71577178 -1.80800924  2.38503303 -2.46465272 -0.11241128  4.24333689
#>  [61]  1.69731662 -1.62158359 -2.06123426 -0.43411137  1.83919915 -0.51011895
#>  [67] -1.85054670 -0.31717630  0.02822226 -2.05630631 -3.14995267  0.56427469
#>  [73]  2.58105939  2.33552974  2.13154657  1.47629830  0.63924247 -1.17683610
#>  [79] -1.82088837 -0.40572131 -0.56642864 -1.38782666  1.02785873 -1.07978817
#>  [85]  1.85581701  2.39371955  0.26892290  3.13015165 -1.61601977  0.36564203
#>  [91]  2.16330286 -0.01541822  4.43030209 -2.68977137  3.16964584 -0.63437042
#>  [97]  0.07709310 -0.37493100 -0.07956849 -4.60235423
#> 
#> $risk_score_formula
#> [1] "Risk Score = 1.1391 * GENE1 + 0.9763 * GENE2 + 1.1916 * GENE3"
#> 
#> $risk_scale
#> [1] "none"
#> 
#> $n_genes
#> [1] 3
#> 
#> $n_samples
#> [1] 100
#> 
#> attr(,"class")
#> [1] "breg_seed_model" "list"           
# }
```
