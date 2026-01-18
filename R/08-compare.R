# Comparison utilities for univariate vs multivariate models
#
# Provides functions to compare univariate and multivariate regression models
# side by side, similar to the functionality shown in autoReg package.
# =====================


#' Compare univariate and multivariate models
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function builds both univariate models (each predictor separately) and
#' a multivariate model (all predictors together), then combines the results
#' for comparison. This is useful for understanding how predictor effects change
#' when accounting for other variables.
#'
#' @param data A `data.frame` containing all necessary variables for analysis.
#' @param y Character vector specifying dependent variables (response variables).
#' For GLM models, this is typically a single character (e.g., `"outcome"`).
#' For Cox-PH models, it should be a length-2 vector in the format `c("time", "status")`.
#' @param x Character vector specifying focal independent terms (predictors).
#' These will be modeled both individually (univariate) and together (multivariate).
#' @param x2 Character vector specifying control independent terms (predictors, optional).
#' These are included in all models (both univariate and multivariate).
#' @param method Method for model construction. See [br_set_model()] for details.
#' @param ... Additional arguments passed to [br_run()].
#' @param n_workers Integer, indicating number of workers for parallel processing.
#' @param model_args A list of arguments passed to `br_set_model()`.
#' @param run_args A list of arguments passed to `br_run()`.
#'
#' @returns A list with class `breg_comparison` containing:
#' - `univariate`: breg object with univariate model results
#' - `multivariate`: breg object with multivariate model results
#' - `combined_results`: Combined results data frame with a `mode` column
#' - `combined_results_tidy`: Combined tidy results with a `mode` column
#'
#' @export
#' @family br_compare
#' @examples
#' # Compare univariate vs multivariate for Cox models
#' lung <- survival::lung |>
#'   dplyr::filter(ph.ecog != 3)
#' lung$ph.ecog <- factor(lung$ph.ecog)
#'
#' comparison <- br_compare_models(
#'   lung,
#'   y = c("time", "status"),
#'   x = c("ph.ecog", "ph.karno", "pat.karno"),
#'   x2 = c("age", "sex"),
#'   method = "coxph"
#' )
#'
#' # View combined results
#' comparison$combined_results_tidy
#'
#' # Create forest plot comparison
#' br_show_forest_comparison(comparison)
#' @testexamples
#' expect_s3_class(comparison, "breg_comparison")
#' expect_true("mode" %in% colnames(comparison$combined_results_tidy))
br_compare_models <- function(
  data,
  y,
  x,
  x2 = NULL,
  method,
  ...,
  n_workers = 1L,
  model_args = list(),
  run_args = list()
) {
  # Validate inputs
  assert_character(y, allow_na = FALSE)
  assert_character(x, allow_na = FALSE)
  assert_character(x2, allow_na = FALSE, allow_null = TRUE)

  if (length(x) < 2) {
    cli::cli_abort("{.arg x} must contain at least 2 variables for comparison")
  }

  # Build univariate models (each x separately with x2 as controls)
  cli::cli_inform("Building univariate models...")
  univariate <- br_pipeline(
    data = data,
    y = y,
    x = x,
    x2 = x2,
    method = method,
    n_workers = n_workers,
    model_args = model_args,
    run_args = run_args,
    ...
  )

  # Build multivariate model (all x together with x2 as controls)
  cli::cli_inform("Building multivariate model...")
  # For multivariate, we treat all x as x2 (controls) with a dummy focal variable
  # Actually, we need to create a single model with all x variables
  # We can do this by setting x to just the first variable name and x2 to include all others

  # Create a multivariate model by combining all x into the model
  # We'll use a different approach: build using the same pipeline but with all x combined
  x_combined <- paste(x, collapse = " + ")

  # Build the multivariate model with all focal variables
  multivariate_br <- breg(data) |>
    br_set_y(y) |>
    br_set_x(x[1]) # Use first as placeholder

  # Set x2 to include all x except first, plus original x2
  if (is.null(x2)) {
    multivariate_br <- multivariate_br |> br_set_x2(x[-1])
  } else {
    multivariate_br <- multivariate_br |> br_set_x2(c(x[-1], x2))
  }

  # Now we need to hack this to create a single model with all x variables
  # Let's take a different approach - manually construct the multivariate model

  # Actually, let's use a cleaner approach:
  # Build a single-model breg object with all x as focal (using a dummy approach)
  multivariate <- build_multivariate_model(data, y, x, x2, method, run_args)

  # Add mode column to distinguish results
  univariate_results <- br_get_results(univariate)
  univariate_results$mode <- "univariate"

  multivariate_results <- br_get_results(multivariate)
  multivariate_results$mode <- "multivariate"

  univariate_results_tidy <- br_get_results(univariate, tidy = TRUE)
  univariate_results_tidy$mode <- "univariate"

  multivariate_results_tidy <- br_get_results(multivariate, tidy = TRUE)
  multivariate_results_tidy$mode <- "multivariate"

  # Filter to only focal variables for comparison
  univariate_results_focal <- univariate_results |>
    dplyr::filter(.data$Focal_variable == .data$variable)

  multivariate_results_focal <- multivariate_results |>
    dplyr::filter(.data$variable %in% x)

  univariate_results_tidy_focal <- univariate_results_tidy |>
    dplyr::filter(.data$Focal_variable == .data$term)

  multivariate_results_tidy_focal <- multivariate_results_tidy |>
    dplyr::filter(.data$term %in% x)

  # Combine results
  combined_results <- dplyr::bind_rows(
    univariate_results_focal,
    multivariate_results_focal
  )

  combined_results_tidy <- dplyr::bind_rows(
    univariate_results_tidy_focal,
    multivariate_results_tidy_focal
  )

  # Create comparison object
  comparison <- list(
    univariate = univariate,
    multivariate = multivariate,
    combined_results = combined_results,
    combined_results_tidy = combined_results_tidy
  )

  class(comparison) <- c("breg_comparison", "list")
  attr(comparison, "exponentiate") <- attr(univariate, "exponentiate")

  comparison
}


#' Build a multivariate model with all focal variables
#' @keywords internal
#' @noRd
build_multivariate_model <- function(data, y, x, x2, method, run_args) {
  # Create a breg object with all x in a single model
  # We'll use the first x as focal and rest as x2
  br <- breg(data) |>
    br_set_y(y) |>
    br_set_x(x[1])

  # Combine remaining x with x2
  if (length(x) > 1) {
    combined_x2 <- c(x[-1], x2)
  } else {
    combined_x2 <- x2
  }

  if (!is.null(combined_x2) && length(combined_x2) > 0) {
    br <- br |> br_set_x2(combined_x2)
  }

  br <- br |> br_set_model(method)

  # Run with specified arguments
  if (length(run_args) > 0) {
    br <- do.call(br_run, c(list(obj = br), run_args))
  } else {
    br <- br_run(br)
  }

  # Modify the results to treat all x variables as focal
  # We need to extract results for all x variables and mark them as focal
  results <- br@results
  results_tidy <- br@results_tidy

  # Update Focal_variable for all x variables
  results <- results |>
    dplyr::mutate(
      Focal_variable = dplyr::if_else(
        .data$variable %in% x,
        .data$variable,
        .data$Focal_variable
      )
    )

  results_tidy <- results_tidy |>
    dplyr::mutate(
      Focal_variable = dplyr::if_else(
        .data$term %in% x,
        .data$term,
        .data$Focal_variable
      )
    )

  br@results <- results
  br@results_tidy <- results_tidy

  br
}


#' Show forest plot for model comparison
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Creates a forest plot comparing univariate and multivariate model results
#' side by side. Each variable shows estimates from both modeling approaches.
#'
#' @param comparison A `breg_comparison` object from [br_compare_models()].
#' @param ... Additional arguments passed to [forestploter::forest()].
#' @param xlim Numeric vector of length 2 specifying x-axis limits.
#' @param rm_controls If `TRUE`, show only focal variables (default).
#'
#' @returns A forest plot object.
#' @export
#' @family br_compare
#' @examples
#' lung <- survival::lung |>
#'   dplyr::filter(ph.ecog != 3)
#' lung$ph.ecog <- factor(lung$ph.ecog)
#'
#' comparison <- br_compare_models(
#'   lung,
#'   y = c("time", "status"),
#'   x = c("ph.ecog", "ph.karno", "pat.karno"),
#'   x2 = c("age", "sex"),
#'   method = "coxph"
#' )
#'
#' br_show_forest_comparison(comparison)
#' @testexamples
#' expect_s3_class(br_show_forest_comparison(comparison), "forestplot")
br_show_forest_comparison <- function(
  comparison,
  ...,
  xlim = NULL,
  rm_controls = TRUE
) {
  if (!inherits(comparison, "breg_comparison")) {
    cli::cli_abort("{.arg comparison} must be a {.cls breg_comparison} object from {.fn br_compare_models}")
  }

  dots <- rlang::list2(...)
  exponentiate <- attr(comparison, "exponentiate")

  # Get combined results
  dt <- comparison$combined_results

  if (rm_controls) {
    # Only show focal variables
    dt <- dt |>
      dplyr::filter(.data$Focal_variable == .data$variable)
  }

  # Format the data for forest plot
  dt <- dt |>
    dplyr::arrange(.data$variable, .data$mode) |>
    dplyr::mutate(
      ` ` = paste(rep(" ", 20), collapse = " "),
      `Estimate (95% CI)` = dplyr::case_when(
        dt$reference_row ~ "Reference",
        is.na(dt$std.error) ~ "",
        TRUE ~
          sprintf(
            "%.2f (%.2f to %.2f)",
            .data$estimate,
            .data$conf.low,
            .data$conf.high
          )
      ),
      P = if_else(
        is.na(.data$p.value),
        "",
        format.pval(.data$p.value, digits = 2, eps = 0.001)
      ),
      conf.low = if_else(is.na(.data$conf.low), .data$estimate, .data$conf.low),
      conf.high = if_else(
        is.na(.data$conf.high),
        .data$estimate,
        .data$conf.high
      ),
      mode = tools::toTitleCase(.data$mode)
    )

  # Calculate xlim if not provided
  if (is.null(xlim)) {
    xlim <- c(
      floor(min(dt$conf.low, na.rm = TRUE)),
      ceiling(max(dt$conf.high, na.rm = TRUE))
    )
    if (is.infinite(xlim[1])) {
      cli_warn("infinite CI detected, set a minimal value -100")
      xlim[1] <- -100
    }
    if (is.infinite(xlim[2])) {
      cli_warn("infinite CI detected, set a maximal value 100")
      xlim[2] <- 100
    }
  }

  # Group by variable to show them together
  dt <- dt |>
    dplyr::group_by(.data$variable) |>
    dplyr::mutate(
      variable = if_else(
        dplyr::row_number() == 1,
        .data$variable,
        ""
      )
    ) |>
    dplyr::ungroup()

  # Select and rename columns for display
  dt <- dt |>
    dplyr::select(
      Variable = "variable",
      Level = "label",
      Mode = "mode",
      N = "n_obs",
      " ",
      "Estimate (95% CI)",
      c("P", "estimate", "conf.low", "conf.high")
    )

  # Set reference line
  if (exponentiate && !("ref_line" %in% names(dots))) {
    dots[["ref_line"]] <- 1L
  }

  # Create forest plot
  p <- do.call(
    forestploter::forest,
    c(
      list(
        data = dt[, 1:7],
        est = dt$estimate,
        lower = dt$conf.low,
        upper = dt$conf.high,
        ci_column = 5,
        xlim = xlim
      ),
      dots
    )
  )

  p
}


#' Print method for breg_comparison object
#' @param x A `breg_comparison` object.
#' @param ... Additional arguments (not used).
#' @export
#' @method print breg_comparison
print.breg_comparison <- function(x, ...) {
  cli::cli_text("A {.cls breg_comparison} object")
  cli::cli_text("")
  cli::cli_text("Univariate models: {nrow(x$univariate@results_tidy)} terms from {length(x$univariate@models)} models")
  cli::cli_text("Multivariate model: {nrow(x$multivariate@results_tidy)} terms from 1 model")
  cli::cli_text("")
  cli::cli_text("Use {.fn br_show_forest_comparison} to visualize the comparison")
  cli::cli_text("Access results via {.code $combined_results} or {.code $combined_results_tidy}")
  invisible(x)
}
