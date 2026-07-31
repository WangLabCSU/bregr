# SEED Gene Selection Pipeline
#
# Implements the SEED (Selection of Essential prognostic genes from Expression
# Data) method for identifying cancer prognosis target genes without control
# samples. Based on Yang et al. (2025), Cancer Letters, DOI: 10.1016/j.canlet.2025.217960.
#
# The pipeline consists of 4 steps:
# 1. Preliminary screening: univariate regression (Cox/logistic/Spearman) per gene
# 2. Enrichment analysis: pathway enrichment (optional)
# 3. Gene selection: Lasso -> multivariate -> stepwise regression
# 4. Model development: build risk score from final gene set
# =====================

# ---- Helpers ----

# Extract significant genes from a breg object
# Returns sorted unique gene names with p.value < threshold
.seed_filter_genes <- function(breg, p_threshold = 0.05) {
  assert_breg_obj_with_results(breg)
  assert_number_decimal(p_threshold, min = 0, max = 1)

  res <- br_get_results(breg, tidy = TRUE)
  res <- res |>
    dplyr::filter(
      .data$Focal_variable == .data$term,
      .data$p.value < p_threshold
    )

  sig_genes <- unique(res$Focal_variable)
  if (length(sig_genes) == 0) {
    cli::cli_warn("no genes passed the p-value threshold {.val {p_threshold}}")
    return(character(0))
  }

  sort(sig_genes)
}

# Run Spearman correlation screening for ordinal clinical variables
# Returns a data.frame matching bregr's results_tidy format
.seed_ordinal_screen <- function(data, y, genes) {
  assert_character(y, allow_na = FALSE)
  assert_character(genes, allow_na = FALSE)

  if (length(y) != 1) {
    cli::cli_abort("{.arg y} must be a single ordinal variable for Spearman screening")
  }
  if (!y %in% colnames(data)) {
    cli::cli_abort("column {.val {y}} not found in data")
  }

  y_vec <- data[[y]]
  if (!is.numeric(y_vec) && !is.ordered(y_vec)) {
    cli::cli_warn("{.arg y} ({.val {y}}) is not numeric or ordered; coercing to numeric for Spearman correlation")
    y_vec <- as.numeric(y_vec)
  }

  results <- purrr::map_dfr(genes, function(gene) {
    if (!gene %in% colnames(data)) {
      return(NULL)
    }
    x_vec <- data[[gene]]
    if (!is.numeric(x_vec)) {
      return(NULL)
    }

    test <- tryCatch(
      stats::cor.test(x_vec, y_vec, method = "spearman", exact = FALSE),
      error = function(e) NULL
    )

    if (is.null(test)) {
      return(NULL)
    }

    data.frame(
      Focal_variable = gene,
      term = gene,
      estimate = unname(test$estimate),
      statistic = unname(test$statistic),
      p.value = unname(test$p.value),
      std.error = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      var_type = "continuous",
      n_obs = sum(!is.na(x_vec) & !is.na(y_vec)),
      stringsAsFactors = FALSE
    )
  })

  results
}

# Intersect significant genes across multiple screening indicators
.seed_intersect_screen <- function(screen_results) {
  gene_sets <- lapply(screen_results, `[[`, "genes")
  gene_sets <- gene_sets[lengths(gene_sets) > 0]

  if (length(gene_sets) == 0) {
    return(character(0))
  }
  if (length(gene_sets) == 1) {
    return(gene_sets[[1]])
  }

  intersected <- Reduce(intersect, gene_sets)

  cli::cli_inform(c(
    "v" = "{.strong {length(intersected)}} genes significant across all {length(gene_sets)} indicators"
  ))

  intersected
}

# Validate SEED inputs
.seed_validate <- function(data, y, y_type, genes, x2) {
  stopifnot(is.data.frame(data))
  assert_character(y, allow_na = FALSE)
  assert_character(y_type, allow_na = FALSE)
  assert_character(x2, allow_na = FALSE, allow_null = TRUE)

  if (nrow(data) == 0) {
    cli::cli_abort("{.arg data} is empty")
  }

  # Validate y_type values
  valid_types <- c("survival", "binary", "ordinal")
  invalid <- setdiff(y_type, valid_types)
  if (length(invalid) > 0) {
    cli::cli_abort("{.arg y_type} must be one of {.val {valid_types}}, got {.val {invalid}}")
  }

  # Determine single vs multi-indicator
  is_single <- is.character(y) && !is.list(y)

  # Validate y/y_type length agreement
  if (is_single) {
    if (length(y_type) != 1) {
      cli::cli_abort(c(
        "single {.arg y} requires a single {.arg y_type}",
        "i" = "{.arg y} is a flat character vector (length {length(y)})",
        "i" = "{.arg y_type} has {length(y_type)} element{?s}"
      ))
    }
    # Single indicator: validate per-type format
    ti <- y_type[1]
    if (ti == "survival") {
      if (length(y) != 2) {
        cli::cli_abort("survival {.arg y} must be a length-2 vector c('time', 'status'), got length {length(y)}")
      }
    } else {
      if (length(y) != 1) {
        cli::cli_abort("{.val {ti}} {.arg y} must be a single column name, got {.val {y}}")
      }
    }
  } else {
    # Multi-indicator: y is a list
    if (length(y) != length(y_type)) {
      cli::cli_abort(c(
        "{.arg y} and {.arg y_type} must have the same length",
        "i" = "{.arg y} has {length(y)} indicator{?s}",
        "i" = "{.arg y_type} has {length(y_type)} element{?s}"
      ))
    }
    for (i in seq_along(y)) {
      yi <- y[[i]]
      ti <- y_type[[i]]
      if (ti == "survival") {
        if (length(yi) != 2) {
          cli::cli_abort("survival {.arg y}[[{i}]] must be a length-2 vector c('time', 'status'), got length {length(yi)}")
        }
      } else {
        if (length(yi) != 1) {
          cli::cli_abort("{.val {ti}} {.arg y}[[{i}]] must be a single column name, got {.val {yi}}")
        }
      }
    }
  }

  # Validate all y columns exist
  all_y_cols <- if (is_single) y else unique(unlist(y))
  missing_y <- setdiff(all_y_cols, colnames(data))
  if (length(missing_y) > 0) {
    cli::cli_abort("columns {.val {missing_y}} in {.arg y} not found in {.arg data}")
  }

  # Auto-detect genes if NULL
  if (is.null(genes)) {
    y_and_x2 <- c(all_y_cols, x2, ".row_names")
    genes <- setdiff(colnames(data), y_and_x2)
    genes <- genes[vapply(data[, genes, drop = FALSE], is.numeric, logical(1))]
    if (length(genes) == 0) {
      cli::cli_abort("no numeric gene columns auto-detected in {.arg data}")
    }
    cli::cli_inform("auto-detected {.strong {length(genes)}} numeric gene columns")
  }

  assert_character(genes, allow_na = FALSE)
  if (length(genes) == 0) {
    cli::cli_abort("{.arg genes} is empty")
  }

  # Validate gene columns exist and are numeric
  missing_genes <- setdiff(genes, colnames(data))
  if (length(missing_genes) > 0) {
    cli::cli_abort("gene column{?s} {.val {missing_genes}} not found in {.arg data}")
  }

  non_numeric <- genes[!vapply(data[, genes, drop = FALSE], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    cli::cli_warn("non-numeric gene column{?s} {.val {non_numeric}} will be excluded")
    genes <- setdiff(genes, non_numeric)
  }

  # Validate no overlap
  if (length(x2) > 0) {
    missing_x2 <- setdiff(x2, colnames(data))
    if (length(missing_x2) > 0) {
      cli::cli_abort("column{?s} {.val {missing_x2}} in {.arg x2} not found in {.arg data}")
    }
    assert_not_overlap(genes, x2,
      msg = "{.arg genes} should not overlap with {.arg x2}"
    )
  }

  list(genes = genes)
}

# ---- br_seed_screen() ----

#' Screen genes via univariate regression
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Step 1 of the SEED pipeline: runs univariate regression for each gene against
#' one or more clinical indicators. Supports Cox regression (survival), logistic
#' regression (binary), and Spearman correlation (ordinal). When multiple
#' indicators are provided, genes significant across *all* indicators are
#' retained (intersection).
#'
#' @param data A `data.frame` containing clinical variables and gene expression
#'   columns side by side. Gene columns must be numeric.
#' @param y Character vector specifying clinical indicators. For survival: a
#'   length-2 vector `c("time", "status")`. For binary/ordinal: a single column
#'   name. When screening against multiple indicators, pass a named list, e.g.,
#'   `list(OS = c("time", "status"), ORR = "response")`.
#' @param y_type Character vector of indicator types: `"survival"`, `"binary"`,
#'   or `"ordinal"`. Must match `y` in length.
#' @param genes Character vector of gene column names. If `NULL`, all numeric
#'   columns not in `y` or `x2` are used.
#' @param x2 Optional character vector of adjustment covariates included in all
#'   models.
#' @param p_threshold Numeric significance threshold for gene filtering (0 to 1).
#'   Default is `0.05`.
#' @param n_workers Integer number of parallel workers for batch regression.
#'   Passed to [br_pipeline()].
#' @param ... Additional arguments passed to [br_pipeline()].
#'
#' @returns A list with class `breg_seed_screen` containing:
#'   - `results`: data.frame of screening results (gene, indicator, p.value, etc.)
#'   - `genes`: character vector of significant genes (intersection across indicators)
#'   - `breg`: the underlying `breg` object (for regression-based screening)
#'   - `n_genes_in`, `n_genes_out`: counts for progress reporting
#' @export
#' @family br_seed
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' # Create gene with real survival signal
#' test_data <- data.frame(
#'   time = rexp(n, 0.1),
#'   status = sample(0:1, n, replace = TRUE),
#'   GENE1 = rnorm(n),
#'   GENE2 = rnorm(n, mean = 0.8 * (1:n) / n),
#'   GENE3 = rnorm(n),
#'   age = rnorm(n, 60, 10)
#' )
#'
#' # Single survival indicator; use relaxed p_threshold for example data
#' res <- br_seed_screen(test_data,
#'   y = c("time", "status"),
#'   y_type = "survival",
#'   genes = c("GENE1", "GENE2", "GENE3"),
#'   x2 = "age",
#'   p_threshold = 0.5
#' )
#' print(res)
#' }
#' @testexamples
#' expect_s3_class(res, "breg_seed_screen")
#' expect_true("genes" %in% names(res))
br_seed_screen <- function(data, y, y_type, genes = NULL, x2 = NULL,
                           p_threshold = 0.05, n_workers = 1L, ...) {
  rlang::check_installed("survival")

  # Validate
  valid <- .seed_validate(data, y, y_type, genes, x2)
  genes <- valid$genes
  n_genes_in <- length(genes)

  if (is.list(y) && !is.character(y)) {
    # Multi-indicator: iterate
    cli::cli_inform("screening {.strong {n_genes_in}} genes against {length(y)} clinical indicators")
  } else {
    cli::cli_inform("screening {.strong {n_genes_in}} genes against {.val {y_type[1]}} indicator")
  }

  # Normalize y to list form
  if (is.character(y) && length(y_type) == 1) {
    y_list <- list(y)
    names(y_list) <- y_type[1]
  } else if (is.list(y)) {
    y_list <- y
  } else {
    y_list <- list(y)
    names(y_list) <- y_type[1]
  }

  # Run screening per indicator
  screen_results <- vector("list", length(y_list))
  names(screen_results) <- names(y_list)

  for (i in seq_along(y_list)) {
    yi <- y_list[[i]]
    ti <- y_type[[i]]
    indicator_name <- names(y_list)[[i]]
    if (is.null(indicator_name) || indicator_name == "") {
      indicator_name <- paste0("indicator_", i)
    }

    cli::cli_inform("  screening {.field {indicator_name}} ({ti})...")

    if (ti == "survival") {
      breg_obj <- br_pipeline(
        data = data, y = yi, x = genes, x2 = x2,
        method = "coxph", n_workers = n_workers,
        filter_x = TRUE, ...
      )
      sig_genes <- .seed_filter_genes(breg_obj, p_threshold)
      screen_results[[i]] <- list(
        breg = breg_obj,
        genes = sig_genes,
        results = br_get_results(breg_obj, tidy = TRUE) |>
          dplyr::filter(.data$Focal_variable == .data$term)
      )
    } else if (ti == "binary") {
      breg_obj <- br_pipeline(
        data = data, y = yi, x = genes, x2 = x2,
        method = "binomial", n_workers = n_workers,
        filter_x = TRUE, ...
      )
      sig_genes <- .seed_filter_genes(breg_obj, p_threshold)
      screen_results[[i]] <- list(
        breg = breg_obj,
        genes = sig_genes,
        results = br_get_results(breg_obj, tidy = TRUE) |>
          dplyr::filter(.data$Focal_variable == .data$term)
      )
    } else if (ti == "ordinal") {
      ord_results <- .seed_ordinal_screen(data, yi, genes)
      sig_genes <- ord_results |>
        dplyr::filter(.data$p.value < p_threshold) |>
        dplyr::pull("Focal_variable") |>
        unique() |>
        sort()
      screen_results[[i]] <- list(
        breg = NULL,
        genes = sig_genes,
        results = ord_results
      )
    }

    cli::cli_inform("    {.strong {length(screen_results[[i]]$genes)}} significant genes found")
  }

  # Intersect across indicators
  intersected_genes <- .seed_intersect_screen(screen_results)
  n_genes_out <- length(intersected_genes)

  # Build combined results data.frame
  combined_results <- purrr::map2_dfr(
    screen_results, names(screen_results),
    function(sr, nm) {
      if (is.null(sr$results) || nrow(sr$results) == 0) {
        return(NULL)
      }
      sr$results$indicator <- nm
      sr$results
    }
  )

  # Create return object
  out <- list(
    results = combined_results,
    genes = intersected_genes,
    breg = screen_results[[1]]$breg,
    screen_results = screen_results,
    p_threshold = p_threshold,
    n_genes_in = n_genes_in,
    n_genes_out = n_genes_out
  )
  class(out) <- c("breg_seed_screen", "list")
  out
}

#' @export
print.breg_seed_screen <- function(x, ...) {
  cli::cli_text("A {.cls breg_seed_screen} object")
  cli::cli_text("")
  cli::cli_text(
    "Genes screened: {.strong {x$n_genes_in}}"
  )
  cli::cli_text(
    "Genes significant (p < {x$p_threshold}): {.strong {x$n_genes_out}}"
  )

  if (x$n_genes_out > 0 && x$n_genes_out <= 20) {
    cli::cli_text("Significant genes: {.val {x$genes}}")
  } else if (x$n_genes_out > 20) {
    cli::cli_text(
      "Significant genes (first 20): {.val {x$genes[1:20]}} ..."
    )
  }
  cli::cli_text("")
  cli::cli_text("Use {.code $results} for full screening results")

  invisible(x)
}

# ---- br_seed_select() ----

#' Select genes via Lasso -> multivariate -> stepwise regression
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Step 3 of the SEED pipeline: refines the gene set through three sequential
#' stages --- Lasso regularization, multivariate regression, and stepwise
#' selection. The final gene set is the intersection of genes retained by all
#' three methods.
#'
#' @inheritParams br_seed_screen
#' @param genes Character vector of gene names (typically from
#'   [br_seed_screen()]), or a `breg_seed_screen` object from which genes are
#'   extracted automatically.
#' @param y_type Character: `"survival"` or `"binary"`. Ordinal indicators are
#'   not supported for selection/modeling steps.
#' @param lasso_lambda Lambda selection criterion: `"lambda.min"` (default) or
#'   `"lambda.1se"`.
#' @param lasso_nfolds Number of folds for cross-validation in Lasso. Default 10.
#' @param step_direction Direction for stepwise selection: `"both"` (default),
#'   `"backward"`, or `"forward"`.
#' @param step_k The penalty multiplier for AIC in stepwise selection. Default 2
#'   (standard AIC).
#' @param seed Integer seed for reproducibility of Lasso cross-validation.
#'
#' @returns A list with class `breg_seed_select` containing:
#'   - `lasso_genes`, `multivariate_genes`, `stepwise_genes`: gene sets from each stage
#'   - `final_genes`: intersection of all three gene sets
#'   - `lasso_model`, `multivariate_model`, `stepwise_model`: fitted model objects
#' @export
#' @family br_seed
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' test_data <- data.frame(
#'   time = rexp(n, 0.1),
#'   status = sample(0:1, n, replace = TRUE),
#'   GENE1 = rnorm(n),
#'   GENE2 = rnorm(n, mean = 0.8 * (1:n) / n),
#'   GENE3 = rnorm(n, mean = -0.5 * (1:n) / n),
#'   GENE4 = rnorm(n),
#'   GENE5 = rnorm(n),
#'   age = rnorm(n, 60, 10)
#' )
#'
#' if (rlang::is_installed("glmnet")) {
#'   sel <- br_seed_select(test_data,
#'     y = c("time", "status"),
#'     y_type = "survival",
#'     genes = c("GENE1", "GENE2", "GENE3", "GENE4", "GENE5"),
#'     x2 = "age",
#'     seed = 42
#'   )
#'   print(sel)
#' }
#' }
#' @testexamples
#' if (rlang::is_installed("glmnet")) {
#'   expect_s3_class(sel, "breg_seed_select")
#' }
br_seed_select <- function(data, y, y_type, genes, x2 = NULL,
                           p_threshold = 0.05,
                           lasso_lambda = c("lambda.min", "lambda.1se"),
                           lasso_nfolds = 10,
                           step_direction = c("both", "backward", "forward"),
                           step_k = 2,
                           seed = NULL) {
  rlang::check_installed("glmnet")

  # Accept breg_seed_screen objects
  if (inherits(genes, "breg_seed_screen")) {
    cli::cli_inform("extracting genes from {.cls breg_seed_screen} object")
    genes <- genes$genes
  }
  assert_character(genes, allow_na = FALSE)

  if (length(genes) < 2) {
    cli::cli_abort("need at least 2 genes for selection, got {length(genes)}")
  }

  y_type <- rlang::arg_match(y_type, c("survival", "binary"))

  # Normalize y
  if (y_type == "survival") {
    if (length(y) != 2) {
      cli::cli_abort("survival {.arg y} must be c('time', 'status')")
    }
  } else {
    if (length(y) != 1) {
      cli::cli_abort("binary {.arg y} must be a single column name")
    }
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  lasso_lambda <- rlang::arg_match(lasso_lambda)
  step_direction <- rlang::arg_match(step_direction)

  # ---- Step A: Lasso ----
  cli::cli_inform("Step A: Lasso regression ({lasso_lambda}) on {length(genes)} genes")

  lasso_result <- .seed_lasso(
    data = data, y = y, genes = genes, x2 = x2,
    y_type = y_type, lambda = lasso_lambda, nfolds = lasso_nfolds
  )
  lasso_genes <- lasso_result$genes
  cli::cli_inform("  retained {.strong {length(lasso_genes)}} genes from Lasso")

  if (length(lasso_genes) < 2) {
    cli::cli_warn("Lasso retained fewer than 2 genes; selection may be unreliable")
  }

  # ---- Step B: Multivariate regression ----
  cli::cli_inform("Step B: Multivariate regression on {length(lasso_genes)} Lasso genes")

  if (y_type == "survival") {
    mv_method <- "coxph"
  } else {
    mv_method <- "binomial"
  }

  mv_breg <- br_pipeline(
    data = data, y = y, x = lasso_genes, x2 = x2,
    method = mv_method
  )

  mv_results <- br_get_results(mv_breg, tidy = TRUE) |>
    dplyr::filter(.data$Focal_variable == .data$term)

  multivariate_genes <- mv_results |>
    dplyr::filter(.data$p.value < p_threshold) |>
    dplyr::pull("Focal_variable") |>
    unique() |>
    sort()

  cli::cli_inform("  retained {.strong {length(multivariate_genes)}} genes from multivariate regression")

  # ---- Step C: Stepwise regression ----
  cli::cli_inform("Step C: Stepwise regression (direction = {step_direction})")

  # Build full model
  if (length(multivariate_genes) < 2) {
    cli::cli_warn("fewer than 2 multivariate genes; using Lasso genes for stepwise model")
    step_genes <- if (length(lasso_genes) >= 2) lasso_genes else genes
  } else {
    step_genes <- multivariate_genes
  }

  # Construct formula: y ~ gene1 + gene2 + ... + x2
  x_terms <- c(step_genes, x2)

  if (y_type == "survival") {
    Surv_y <- glue::glue("survival::Surv({y[1]}, {y[2]})")
    formula_str <- paste(Surv_y, "~", paste(x_terms, collapse = " + "))
    full_model <- survival::coxph(stats::as.formula(formula_str), data = data)
  } else {
    formula_str <- paste(y[1], "~", paste(x_terms, collapse = " + "))
    full_model <- stats::glm(stats::as.formula(formula_str),
      data = data, family = stats::binomial
    )
  }

  step_model <- tryCatch(
    stats::step(full_model, direction = step_direction, k = step_k, trace = 0),
    error = function(e) {
      cli::cli_warn("stepwise regression failed: {e$message}; returning full model")
      full_model
    }
  )

  stepwise_predictors <- insight::find_predictors(step_model, flatten = TRUE)
  stepwise_genes <- intersect(stepwise_predictors, genes)
  cli::cli_inform("  retained {.strong {length(stepwise_genes)}} genes from stepwise regression")

  # ---- Intersection ----
  # Only intersect non-empty gene sets
  gene_sets <- list(
    lasso = lasso_genes,
    multivariate = multivariate_genes,
    stepwise = stepwise_genes
  )
  # Remove empty sets from intersection
  non_empty <- gene_sets[lengths(gene_sets) > 0]

  final_genes <- if (length(non_empty) > 1) {
    Reduce(intersect, non_empty)
  } else if (length(non_empty) == 1) {
    non_empty[[1]]
  } else {
    character(0)
  }

  cli::cli_inform(c(
    "v" = "{.strong {length(final_genes)}} final genes from intersection of all stages"
  ))

  out <- list(
    lasso_genes = lasso_genes,
    multivariate_genes = multivariate_genes,
    stepwise_genes = stepwise_genes,
    final_genes = final_genes,
    lasso_model = lasso_result$model,
    multivariate_model = mv_breg,
    stepwise_model = step_model,
    p_threshold = p_threshold
  )
  class(out) <- c("breg_seed_select", "list")
  out
}

#' @export
print.breg_seed_select <- function(x, ...) {
  cli::cli_text("A {.cls breg_seed_select} object")
  cli::cli_text("")
  cli::cli_text("Lasso genes:         {.strong {length(x$lasso_genes)}}")
  cli::cli_text("Multivariate genes:  {.strong {length(x$multivariate_genes)}}")
  cli::cli_text("Stepwise genes:      {.strong {length(x$stepwise_genes)}}")
  cli::cli_text("")
  cli::cli_text("Final genes: {.val {x$final_genes}}")

  invisible(x)
}

# ---- br_seed_model() ----

#' Build a risk score model from selected genes
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Step 4 of the SEED pipeline: builds a final regression model from the
#' selected gene set and computes a risk score for each sample. The risk score
#' is the linear predictor (gene expression  x  coefficient) summed across genes.
#'
#' @inheritParams br_seed_select
#' @param genes Character vector of final gene names, or a `breg_seed_select`
#'   object from which `final_genes` are extracted.
#' @param risk_scale How to scale expression values before computing risk score.
#'   `"none"` (default) uses raw expression; `"zscore"` centers and scales.
#'
#' @returns A list with class `breg_seed_model` containing:
#'   - `model`: `breg` object with the final model
#'   - `coefficients`: data.frame of gene coefficients
#'   - `risk_score`: numeric vector, one per sample
#'   - `risk_score_formula`: character string representation of the risk formula
#' @export
#' @family br_seed
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' test_data <- data.frame(
#'   time = rexp(n, 0.1),
#'   status = sample(0:1, n, replace = TRUE),
#'   GENE1 = rnorm(n),
#'   GENE2 = rnorm(n, mean = 0.8 * (1:n) / n),
#'   GENE3 = rnorm(n, mean = -0.5 * (1:n) / n),
#'   age = rnorm(n, 60, 10)
#' )
#'
#' mod <- br_seed_model(test_data,
#'   y = c("time", "status"),
#'   y_type = "survival",
#'   genes = c("GENE1", "GENE2", "GENE3"),
#'   x2 = "age"
#' )
#' print(mod)
#' }
#' @testexamples
#' expect_s3_class(mod, "breg_seed_model")
br_seed_model <- function(data, y, y_type, genes, x2 = NULL,
                          risk_scale = c("none", "zscore")) {
  rlang::check_installed("survival")

  # Accept breg_seed_select objects
  if (inherits(genes, "breg_seed_select")) {
    cli::cli_inform("extracting final genes from {.cls breg_seed_select} object")
    genes <- genes$final_genes
  }
  assert_character(genes, allow_na = FALSE)

  if (length(genes) == 0) {
    cli::cli_abort("{.arg genes} is empty")
  }

  y_type <- rlang::arg_match(y_type, c("survival", "binary"))
  risk_scale <- rlang::arg_match(risk_scale)

  # Map to bregr method
  method <- if (y_type == "survival") "coxph" else "binomial"

  # Build final model via br_pipeline
  cli::cli_inform("building final {.field {method}} model with {length(genes)} genes")
  breg_obj <- br_pipeline(
    data = data, y = y, x = genes, x2 = x2,
    method = method
  )

  # Extract coefficients
  coefs <- br_get_results(breg_obj, tidy = TRUE) |>
    dplyr::filter(.data$Focal_variable == .data$term) |>
    dplyr::select(term = "Focal_variable", estimate = "estimate") |>
    dplyr::filter(!is.na(.data$estimate))

  # Compute risk score
  expr_mat <- as.matrix(data[, coefs$term, drop = FALSE])

  if (risk_scale == "zscore") {
    expr_mat <- scale(expr_mat, center = TRUE, scale = TRUE)
    cli::cli_inform("expression values z-score scaled before risk score computation")
  }

  risk_score <- as.numeric(expr_mat %*% coefs$estimate)

  # Build risk score formula string
  terms_str <- paste0(
    sprintf("%.4f * %s", coefs$estimate, coefs$term),
    collapse = " + "
  )
  risk_formula <- paste("Risk Score =", terms_str)

  out <- list(
    model = breg_obj,
    coefficients = coefs,
    risk_score = risk_score,
    risk_score_formula = risk_formula,
    risk_scale = risk_scale,
    n_genes = length(genes),
    n_samples = length(risk_score)
  )
  class(out) <- c("breg_seed_model", "list")
  out
}

#' @export
print.breg_seed_model <- function(x, ...) {
  cli::cli_text("A {.cls breg_seed_model} object")
  cli::cli_text("")
  cli::cli_text("Genes in model: {.strong {x$n_genes}}")
  cli::cli_text("Samples: {.strong {x$n_samples}}")
  cli::cli_text("Risk scale: {.val {x$risk_scale}}")
  cli::cli_text("")
  cli::cli_text("{.field Risk Score Formula}:")
  cli::cli_text("{x$risk_score_formula}")
  cli::cli_text("")
  cli::cli_text("Overview of risk scores:")
  print(summary(x$risk_score))

  invisible(x)
}

# ---- Lasso helper ----

.seed_lasso <- function(data, y, genes, x2, y_type, lambda, nfolds) {
  rlang::check_installed("glmnet")

  # Build predictor matrix (genes only) and response
  x_mat <- as.matrix(data[, genes, drop = FALSE])

  # Remove rows with any NA in predictors
  complete_rows <- stats::complete.cases(x_mat)
  if (!all(complete_rows)) {
    cli::cli_inform("removing {sum(!complete_rows)} rows with missing expression values")
    x_mat <- x_mat[complete_rows, , drop = FALSE]
    data <- data[complete_rows, ]
  }

  if (nrow(x_mat) < nfolds * 2) {
    cli::cli_warn("fewer than {nfolds * 2} complete cases; reducing folds to {floor(nrow(x_mat) / 2)}")
    nfolds <- max(3, floor(nrow(x_mat) / 2))
  }

  # Build response
  if (y_type == "survival") {
    y_surv <- survival::Surv(data[[y[1]]], data[[y[2]]])
    family <- "cox"
  } else {
    y_surv <- data[[y[1]]]
    family <- "binomial"
  }

  # Run cv.glmnet
  cv_fit <- tryCatch(
    glmnet::cv.glmnet(
      x = x_mat, y = y_surv,
      family = family,
      alpha = 1, # Lasso
      nfolds = nfolds,
      standardize = TRUE
    ),
    error = function(e) {
      cli::cli_abort("Lasso fitting failed: {e$message}")
    }
  )

  # Extract non-zero coefficients at chosen lambda
  lambda_val <- if (lambda == "lambda.min") cv_fit$lambda.min else cv_fit$lambda.1se
  coef_mat <- as.matrix(stats::coef(cv_fit, s = lambda_val))
  nonzero_idx <- which(coef_mat[, 1] != 0)

  # Remove intercept from gene list
  nonzero_genes <- setdiff(rownames(coef_mat)[nonzero_idx], "(Intercept)")

  # Intersect with input genes to be safe
  lasso_genes <- intersect(nonzero_genes, genes)

  list(
    model = cv_fit,
    genes = lasso_genes,
    lambda = lambda_val,
    cvm_min = min(cv_fit$cvm)
  )
}

# ---- br_seed() unified pipeline ----

#' Run the full SEED gene selection pipeline
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Implements the complete SEED (Selection of Essential prognostic genes from
#' Expression Data) pipeline for identifying cancer prognosis target genes from
#' RNA-seq data without control samples.
#'
#' The pipeline runs four steps sequentially:
#' 1. **Preliminary screening**: Univariate regression (Cox/logistic/Spearman)
#'    per gene against clinical indicators
#' 2. **Enrichment analysis** (optional): Pathway enrichment to refine gene set
#' 3. **Gene selection**: Lasso -> multivariate -> stepwise regression
#' 4. **Model development**: Build risk score from final gene set
#'
#' Based on Yang, H. et al. (2025) *Cancer Letters*, DOI: 10.1016/j.canlet.2025.217960.
#'
#' @inheritParams br_seed_screen
#' @inheritParams br_seed_select
#' @inheritParams br_seed_model
#' @param enrich Optional enrichment method. One of:
#'   - `NULL` (default): skip enrichment
#'   - A string naming an `org.*.eg.db` package (e.g., `"org.Hs.eg.db"`)
#'   - A function taking a character vector of gene symbols and returning a
#'     character vector of enriched genes
#' @param do_select Logical. If `TRUE` (default), run gene selection step.
#'
#' @returns A list with class `breg_seed` containing:
#'   - `screen`: `breg_seed_screen` object from Step 1
#'   - `select`: `breg_seed_select` object from Step 3 (if `do_select = TRUE`)
#'   - `model`: `breg_seed_model` object from Step 4
#'   - `risk_score`: numeric vector of risk scores per sample
#' @export
#' @family br_seed
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' test_data <- data.frame(
#'   time = rexp(n, 0.1),
#'   status = sample(0:1, n, replace = TRUE),
#'   GENE1 = rnorm(n),
#'   GENE2 = rnorm(n, mean = 0.8 * (1:n) / n),
#'   GENE3 = rnorm(n, mean = -0.5 * (1:n) / n),
#'   GENE4 = rnorm(n),
#'   GENE5 = rnorm(n),
#'   age = rnorm(n, 60, 10)
#' )
#'
#' if (rlang::is_installed("glmnet")) {
#'   res <- br_seed(test_data,
#'     y = c("time", "status"),
#'     y_type = "survival",
#'     genes = c("GENE1", "GENE2", "GENE3", "GENE4", "GENE5"),
#'     x2 = "age",
#'     p_threshold = 0.3,
#'     seed = 42
#'   )
#'   print(res)
#' }
#' }
#' @testexamples
#' if (rlang::is_installed("glmnet")) {
#'   expect_s3_class(res, "breg_seed")
#' }
br_seed <- function(data, y, y_type, genes = NULL, x2 = NULL,
                    p_threshold = 0.05, n_workers = 1L,
                    enrich = NULL,
                    do_select = TRUE,
                    lasso_lambda = c("lambda.min", "lambda.1se"),
                    lasso_nfolds = 10,
                    step_direction = c("both", "backward", "forward"),
                    step_k = 2,
                    risk_scale = c("none", "zscore"),
                    seed = NULL,
                    ...) {
  rlang::check_installed("survival")

  # Validate
  valid <- .seed_validate(data, y, y_type, genes, x2)
  genes <- valid$genes

  # ---- Step 1: Screening ----
  cli::cli_h1("SEED Pipeline")
  cli::cli_h2("Step 1: Preliminary Screening")
  screen <- br_seed_screen(
    data = data, y = y, y_type = y_type,
    genes = genes, x2 = x2,
    p_threshold = p_threshold,
    n_workers = n_workers, ...
  )

  genes <- screen$genes
  if (length(genes) == 0) {
    cli::cli_abort("no genes passed screening; try increasing {.arg p_threshold}")
  }

  # ---- Step 2: Enrichment (optional) ----
  if (!is.null(enrich)) {
    cli::cli_h2("Step 2: Enrichment Analysis")
    genes <- .seed_run_enrichment(genes, enrich)
    cli::cli_inform("{.strong {length(genes)}} genes after enrichment filtering")
  } else {
    cli::cli_inform("Step 2: Enrichment Analysis (skipped)")
  }

  # ---- Step 3: Gene Selection ----
  if (do_select && length(genes) >= 2) {
    cli::cli_h2("Step 3: Gene Selection")
    lasso_lambda <- rlang::arg_match(lasso_lambda)
    step_direction <- rlang::arg_match(step_direction)
    risk_scale <- rlang::arg_match(risk_scale)

    select <- br_seed_select(
      data = data, y = y, y_type = y_type,
      genes = genes, x2 = x2,
      p_threshold = p_threshold,
      lasso_lambda = lasso_lambda,
      lasso_nfolds = lasso_nfolds,
      step_direction = step_direction,
      step_k = step_k,
      seed = seed
    )
    genes <- select$final_genes
  } else {
    cli::cli_inform("Step 3: Gene Selection (skipped --- need >= 2 genes)")
    select <- NULL
  }

  if (length(genes) == 0) {
    cli::cli_abort("no genes remaining after selection; try relaxing {.arg p_threshold}")
  }

  # ---- Step 4: Model Development ----
  cli::cli_h2("Step 4: Model Development")
  model <- br_seed_model(
    data = data, y = y, y_type = y_type,
    genes = genes, x2 = x2,
    risk_scale = risk_scale
  )

  # ---- Assemble result ----
  out <- list(
    screen = screen,
    select = select,
    model = model,
    risk_score = model$risk_score,
    risk_score_formula = model$risk_score_formula
  )
  class(out) <- c("breg_seed", "list")
  out
}

#' @export
print.breg_seed <- function(x, ...) {
  cli::cli_text("A {.cls breg_seed} object --- SEED pipeline results")
  cli::cli_text("")
  cli::cli_h3("Pipeline Summary")
  cli::cli_ul()
  cli::cli_li("Step 1 (Screening): {x$screen$n_genes_in} genes -> {x$screen$n_genes_out} significant")
  if (!is.null(x$select)) {
    cli::cli_li("Step 3 (Selection): {length(x$select$final_genes)} final genes")
  }
  cli::cli_li("Step 4 (Model): {x$model$n_genes} genes, {x$model$n_samples} samples")
  cli::cli_end()
  cli::cli_text("")
  cli::cli_text("{.field Risk Score Formula}:")
  cli::cli_text("{x$risk_score_formula}")

  invisible(x)
}

# ---- Enrichment helper ----

.seed_run_enrichment <- function(genes, enrich) {
  if (is.null(genes) || length(genes) == 0) {
    cli::cli_warn("no genes to enrich")
    return(character(0))
  }

  if (rlang::is_function(enrich)) {
    cli::cli_inform("running custom enrichment function on {length(genes)} genes")
    result <- enrich(genes)
    if (!is.character(result)) {
      cli::cli_abort("custom enrichment function must return a character vector of gene names")
    }
    return(result)
  }

  if (rlang::is_string(enrich)) {
    # e.g., enrich = "org.Hs.eg.db"
    rlang::check_installed(c("clusterProfiler", enrich, "AnnotationDbi"))

    cli::cli_inform("running GO enrichment via {.pkg clusterProfiler} with {.pkg {enrich}}")

    orgdb <- getExportedValue(enrich, enrich) # get the orgdb object

    # Use get() to avoid R CMD check NOTE about undeclared AnnotationDbi
    ann_select <- get("select", envir = asNamespace("AnnotationDbi"))

    # Map gene symbols to ENTREZ IDs
    entrez_ids <- tryCatch(
      ann_select(
        orgdb,
        keys = genes,
        columns = "ENTREZID",
        keytype = "SYMBOL"
      ),
      error = function(e) {
        cli::cli_warn("gene ID mapping failed: {e$message}")
        return(NULL)
      }
    )

    if (is.null(entrez_ids) || nrow(entrez_ids) == 0) {
      cli::cli_warn("no ENTREZ ID mappings found; returning original genes")
      return(genes)
    }

    entrez_ids <- entrez_ids[!is.na(entrez_ids$ENTREZID), ]

    ego <- tryCatch(
      clusterProfiler::enrichGO(
        gene = unique(entrez_ids$ENTREZID),
        OrgDb = orgdb,
        ont = "BP",
        pAdjustMethod = "BH",
        qvalueCutoff = 0.05
      ),
      error = function(e) {
        cli::cli_warn("GO enrichment failed: {e$message}")
        return(NULL)
      }
    )

    if (is.null(ego) || nrow(ego@result) == 0) {
      cli::cli_warn("no enriched GO terms found; returning original genes")
      return(genes)
    }

    # Extract genes from significant GO terms
    enriched_entrez <- unique(unlist(strsplit(ego@result$geneID, "/")))
    enriched_genes <- entrez_ids$SYMBOL[entrez_ids$ENTREZID %in% enriched_entrez]
    enriched_genes <- unique(enriched_genes)
    enriched_genes <- intersect(enriched_genes, genes)

    cli::cli_inform("{.strong {length(enriched_genes)}} genes mapped to enriched GO terms")
    return(enriched_genes)
  }

  cli::cli_abort("{.arg enrich} must be NULL, a function, or an org.db package name string")
}
