# Visualization and display utilities for model results
#
# Provides functions to display regression model outputs in various formats
# including plots, tables, and other visual representations. Organized as
# a family of related functions rather than a single monolithic function.
# =====================


#' Show a forest plot for regression results
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function takes regression results and formats them into a forest plot display. It handles:
#' - Formatting of estimates, CIs and p-values
#' - Automatic x-axis limits calculation
#' - Cleaning of redundant group/focal variable labels
#' - Custom subsetting and column dropping
#' The function uses [forestploter::forest()] internally for the actual plotting.
#'
#' @param breg A regression object with results (must pass `assert_breg_obj_with_results()`).
#' @param clean Logical indicating whether to clean/condense redundant group/focal variable labels.
#' If `TRUE`, remove "Group" or "Focal" variable column when the values in the result table
#' are same (before performing `subset` and `drop`),
#' and reduce repeat values in column "Group", "Focal", and "Variable".
#' @param rm_controls If `TRUE`, remove control terms.
#' @param ... Additional arguments passed to [forestploter::forest()], run `vignette("forestploter-post", "forestploter")`
#' to see more plot options.
#' For example, use `ticks_at` to specify
#' custom ticks, generally a vector of 4-5 elements.
#' @param subset Expression for subsetting the results data (`br_get_results(breg)`).
#' @param drop Column indices to drop from the display table.
#' @param tab_headers Character vector of custom column headers (must match number of displayed columns).
#' @param log_first Log transformed the estimates and their confident intervals.
#' For only log scaled axis of the forest, use `x_trans = "log"`.
#' @returns A plot
#' @export
#' @family br_show
#' @examples
#' m <- br_pipeline(mtcars,
#'   y = "mpg",
#'   x = colnames(mtcars)[2:4],
#'   x2 = "vs",
#'   method = "gaussian"
#' )
#' br_show_forest(m)
#' br_show_forest(m, clean = TRUE, drop = 3)
#' br_show_forest(m, clean = FALSE)
#' @testexamples
#' assert_s3_class(br_show_forest(m), "forestplot")
br_show_forest <- function(
    breg,
    clean = TRUE,
    rm_controls = FALSE,
    ...,
    subset = NULL,
    drop = NULL,
    tab_headers = NULL,
    log_first = FALSE) {
  assert_breg_obj_with_results(breg)
  assert_bool(rm_controls)

  # TODO: grouped (compared) forestplot for group_by???
  dots <- rlang::list2(...)

  dt <- br_get_results(breg)
  x2 <- br_get_x2(breg)

  if (log_first) {
    dt <- dt |> dplyr::mutate(
      estimate = log(.data$estimate),
      conf.high = log(.data$conf.high),
      conf.low = log(.data$conf.low)
    )
  }
  exponentiate <- attr(breg, "exponentiate")
  if (exponentiate && !log_first && !("ref_line" %in% names(dots))) {
    dots[["ref_line"]] <- 1L
  }

  if (rm_controls) {
    dt <- dt |> dplyr::filter(.data$Focal_variable == .data$variable)
  }
  subset <- rlang::enquo(subset)
  if (!rlang::quo_is_null(subset)) {
    dt <- dt |> dplyr::filter(!!subset)
  }

  has_group <- !is.null(br_get_group_by(breg))
  dt <- dt |>
    dplyr::mutate(
      ` ` = paste(rep(" ", 20), collapse = " "),
      `Estimate (95% CI)` = dplyr::case_when(
        dt$reference_row ~ "Reference",
        is.na(dt$std.error) ~ "",
        TRUE ~
          sprintf(
            "%.2f (%.2f to %.2f)",
            estimate,
            conf.low,
            conf.high
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
      )
    ) #|> dplyr::mutate_all(~dplyr::if_else(is.na(.), "", as.character(.)))

  if (!"xlim" %in% names(dots)) {
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
  } else {
    xlim <- dots[["xlim"]]
    dots[["xlim"]] <- NULL
  }

  grp_is_null <- if (has_group) FALSE else TRUE
  fcl_is_null <- FALSE
  if (clean) {
    dt <- dt |>
      dplyr::mutate(
        label = if_else(
          vctrs::vec_equal(.data$variable, .data$label, na_equal = TRUE),
          "", .data$label
        )
      )

    # Drop Group or Focal column if necessary
    if (!grp_is_null) {
      if (length(unique(dt$Group_variable)) == 1L) {
        dt$Group_variable <- NULL
        grp_is_null <- TRUE
      }
    }
    if (grp_is_null && length(unique(dt$Focal_variable)) == 1L) {
      dt$Focal_variable <- NULL
      fcl_is_null <- TRUE
    }

    # Keep unique variable in single model at plotting
    if (!grp_is_null) {
      dt <- dt |>
        dplyr::group_by(
          .data$Group_variable,
          .data$Focal_variable,
          .data$variable
        )
    } else if (!fcl_is_null) {
      dt <- dt |> dplyr::group_by(.data$Focal_variable, .data$variable)
    } else {
      dt <- dt |> dplyr::group_by(.data$variable)
    }
    dt <- dt |>
      dplyr::mutate(
        variable = if_else(
          is.na(.data$reference_row) | .data$reference_row,
          .data$variable,
          ""
        )
      ) |>
      dplyr::ungroup()

    if (!all(grp_is_null, fcl_is_null)) {
      # Keep unique Focal
      if (!grp_is_null) {
        dt <- dt |> dplyr::group_by(.data$Group_variable, .data$Focal_variable)
      } else if (!fcl_is_null) {
        dt <- dt |> dplyr::group_by(.data$Focal_variable)
      }
      dt <- dt |>
        dplyr::mutate(
          Focal_variable = if_else(
            dplyr::row_number() == 1,
            .data$Focal_variable,
            ""
          )
        ) |>
        dplyr::ungroup()

      # Keep unique Group
      if (!grp_is_null) {
        dt <- dt |>
          dplyr::group_by(.data$Group_variable) |>
          dplyr::mutate(
            Group_variable = if_else(
              dplyr::row_number() == 1,
              .data$Group_variable,
              ""
            )
          ) |>
          dplyr::ungroup()
      }
    }
  }

  sel_cols <- c(
    if (!grp_is_null) "Group_variable" else NULL,
    if (!fcl_is_null) "Focal_variable" else NULL,
    "variable",
    "label",
    "n_obs",
    " ",
    "Estimate (95% CI)",
    "P",
    "estimate",
    "conf.low",
    "conf.high"
  )
  dt <- dt |>
    dplyr::select(dplyr::all_of(sel_cols), dplyr::everything()) |>
    rename(c(
      "Group_variable" = "Group",
      "Focal_variable" = "Focal",
      "variable" = "Variable",
      "label" = "Level",
      "n_obs" = "N",
      if (log_first) {
        c("Estimate (95% CI)" = "log(Estimate) (95% CI)")
      } else {
        NULL
      }
    ))

  if (!is.null(drop)) {
    for (i in drop) {
      assert_number_whole(
        i,
        min = 1,
        max = as.numeric(ncol(dt)),
        allow_null = TRUE
      )
    }
    dt[, drop] <- NULL
  }

  idx_end <- which(colnames(dt) == "P")
  idx_ci <- idx_end - 2L

  if (!is.null(tab_headers)) {
    assert_character_len(tab_headers, len = idx_end)
    colnames(dt)[1:idx_end] <- tab_headers
  }

  rlang::inject(
    forestploter::forest(
      dt[, 1:idx_end],
      est = dt$estimate,
      lower = dt$conf.low,
      upper = dt$conf.high,
      ci_column = idx_ci,
      xlim = xlim,
      !!!dots
    )
  )
}

#' Show a forest plot with `ggstats` interface
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Provides an interface to visualize the model results with [**ggstats**](https://github.com/larmarange/ggstats/) package.
#'
#' @inheritParams br_show_forest
#' @param idx Index or names (focal variables) of the model(s).
#' @param ... Arguments passing to [ggstats::ggcoef_table()] or [ggstats::ggcoef_compare()] excepts `model`.
#' @returns A plot
#' @export
#' @family br_show
#' @examples
#' if (rlang::is_installed("ggstats")) {
#'   m <- br_pipeline(mtcars,
#'     y = "mpg",
#'     x = colnames(mtcars)[2:4],
#'     x2 = "vs",
#'     method = "gaussian"
#'   )
#'   br_show_forest_ggstats(m)
#' }
#'
#' @testexamples
#' expect_true(TRUE)
br_show_forest_ggstats <- function(breg, idx = NULL, ...) {
  assert_breg_obj_with_results(breg)
  rlang::check_installed("ggstats")

  mds <- br_get_models(breg, idx)

  .f <- if (identical(class(mds), "list")) {
    ggstats::ggcoef_compare
  } else {
    ggstats::ggcoef_table
  }

  do.call(.f, vctrs::vec_c(list(mds), list(..., interaction_sep = ":")))
}

#' Show a forest plot with `ggstatsplot` interface
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Provides an interface to visualize the model results with [**ggstatsplot**](https://github.com/IndrajeetPatil/ggstatsplot/) package.
#'
#' @inheritParams br_show_forest
#' @param idx Length-1 vector. Index or name (focal variable) of the model.
#' This is different from `idx` in [br_show_forest_ggstats], only one model is supported
#' to visualized here, so only length-1 vector is supported as `idx`.
#' @param ... Arguments passing to [ggstatsplot::ggcoefstats()] excepts `x`.
#' @export
#' @returns A plot
#' @family br_show
#' @examples
#' if (rlang::is_installed("ggstats")) {
#'   m <- br_pipeline(mtcars,
#'     y = "mpg",
#'     x = colnames(mtcars)[2:4],
#'     x2 = "vs",
#'     method = "gaussian"
#'   )
#'   br_show_forest_ggstatsplot(m)
#' }
#'
#' @testexamples
#' expect_true(TRUE)
br_show_forest_ggstatsplot <- function(breg, idx = 1, ...) {
  assert_breg_obj_with_results(breg)
  if (length(idx) != 1) {
    cli_abort("length-1 {.arg idx} (integer index or a focal variable name) is required")
  }
  rlang::check_installed("ggstatsplot")

  mod <- br_get_models(breg, idx)
  ggstatsplot::ggcoefstats(mod, ...)
}

#' Show fitted regression line with `visreg` interface
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Provides an interface to visualize the model results with [**visreg**](https://github.com/larmarange/ggstats/) package, to show how a predictor variable `x` affects an outcome `y`.
#'
#' @inheritParams br_show_forest_ggstatsplot
#' @param ... Arguments passing to [visreg::visreg()] excepts `fit` and `data`.
#' @export
#' @returns A plot
#' @family br_show
#' @examples
#' if (rlang::is_installed("visreg")) {
#'   m <- br_pipeline(mtcars,
#'     y = "mpg",
#'     x = colnames(mtcars)[2:4],
#'     x2 = "vs",
#'     method = "gaussian"
#'   )
#'
#'   if (interactive()) {
#'     br_show_fitted_line(m)
#'   }
#'   br_show_fitted_line(m, xvar = "cyl")
#' }
#'
#' @testexamples
#' expect_true(TRUE)
br_show_fitted_line <- function(breg, idx = 1, ...) {
  assert_breg_obj_with_results(breg)
  if (length(idx) != 1) {
    cli_abort("length-1 {.arg idx} (integer index or a focal variable name) is required")
  }
  rlang::check_installed("visreg")
  cli_inform("subset model list with idx: {.val {idx}}")
  mod <- br_get_models(breg, idx)
  cal <- if (isS4(mod)) mod@call else mod$call
  cli_inform("model call: {rlang::expr_deparse(cal)}")
  visreg::visreg(mod, data = broom.helpers::model_get_model_frame(mod), ...)
}

#' Show 2d fitted regression line with `visreg` interface
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Similar to [br_show_fitted_line()], but visualize how *two variables* interact to affect the response in regression models.
#'
#' @inheritParams br_show_forest_ggstatsplot
#' @param ... Arguments passing to [visreg::visreg2d()] excepts `fit` and `data`.
#' @export
#' @returns A plot
#' @family br_show
#' @examples
#' if (rlang::is_installed("visreg")) {
#'   m <- br_pipeline(mtcars,
#'     y = "mpg",
#'     x = colnames(mtcars)[2:4],
#'     x2 = "vs",
#'     method = "gaussian"
#'   )
#'
#'   br_show_fitted_line_2d(m, xvar = "cyl", yvar = "mpg")
#' }
#'
#' @testexamples
#' expect_true(TRUE)
br_show_fitted_line_2d <- function(breg, idx = 1, ...) {
  assert_breg_obj_with_results(breg)
  if (length(idx) != 1) {
    cli_abort("length-1 {.arg idx} (integer index or a focal variable name) is required")
  }
  rlang::check_installed("visreg")

  mod <- br_get_models(breg, idx)
  cal <- if (isS4(mod)) mod@call else mod$call
  cli_inform("model call: {rlang::expr_deparse(cal)}")
  visreg::visreg2d(mod, data = broom.helpers::model_get_model_frame(mod), ...)
}

#' Show model tidy results in table format
#'
#' @description
#' `r lifecycle::badge('stable')`
#' @inheritParams br_show_forest
#' @param ... Arguments passing to [br_get_results()] for subsetting table.
#' @param args_table_format A list of arguments passing to [insight::format_table()].
#' @param export Logical. If `TRUE`, show table for export purpose, e.g., present the table in Markdown or HTML format.
#' @param args_table_export A list of arguments passing to [insight::export_table()]. Only works when `export` is `TRUE`.
#' @export
#' @returns A table
#' @family br_show
#' @examples
#' m <- br_pipeline(mtcars,
#'   y = "mpg",
#'   x = colnames(mtcars)[2:4],
#'   x2 = "vs",
#'   method = "gaussian"
#' )
#'
#' br_show_table(m)
#' br_show_table(m, export = TRUE)
#' if (interactive()) {
#'   br_show_table(m, export = TRUE, args_table_export = list(format = "html"))
#' }
#' @testexamples
#' expect_true(TRUE)
br_show_table <- function(breg, ..., args_table_format = list(), export = FALSE, args_table_export = list()) {
  assert_breg_obj_with_results(breg)

  tidy_result <- br_get_results(breg, tidy = TRUE, ...)
  tbl <- do.call(insight::format_table, vctrs::vec_c(list(tidy_result), args_table_format))
  if (export) tbl <- do.call(insight::export_table, vctrs::vec_c(list(tbl), args_table_export))
  tbl
}

#' Show regression models with `gtsummary` interface
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Provides an interface to visualize the model results with [**gtsummary**](https://github.com/ddsjoberg/gtsummary/) package in table format.
#' check <https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#customize-output> to see possible output customization.
#'
#' @inheritParams br_show_forest
#' @inheritParams gtsummary::tbl_merge
#' @param idx Index or names (focal variables) of the model(s).
#' @param ... Arguments passing to [gtsummary::tbl_regression()] excepts `x`.
#' @export
#' @returns A table
#' @family br_show
#' @examples
#' if (rlang::is_installed("gtsummary")) {
#'   m <- br_pipeline(mtcars,
#'     y = "mpg",
#'     x = colnames(mtcars)[2:4],
#'     x2 = "vs",
#'     method = "gaussian"
#'   )
#'   br_show_table_gt(m)
#' }
#'
#' @testexamples
#' expect_true(TRUE)
br_show_table_gt <- function(
    breg, idx = NULL, ...,
    tab_spanner = NULL) {
  assert_breg_obj_with_results(breg)
  rlang::check_installed("gtsummary")

  mds <- if (!is.null(idx)) {
    br_get_model(breg, idx)
  } else {
    br_get_models(breg)
  }
  if (length(mds) == 1) {
    mds <- mds[[1]]
  }

  if (insight::is_model(mds)) {
    if (!is.null(tab_spanner)) {
      cli_warn("{.arg tab_spanner} is not used when only one model selected")
    }
    t <- gtsummary::tbl_regression(mds, ...)
  } else {
    t <- map(mds, gtsummary::tbl_regression, ...)
    t <- t |>
      gtsummary::tbl_merge(
        tab_spanner = if (is.null(tab_spanner)) {
          paste0("**", names(mds), "**")
        } else {
          tab_spanner
        }
      )
  }
  t
}

#' Show a nomogram for survival models
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function creates a nomogram visualization for survival models, particularly 
#' Cox proportional hazards models. A nomogram is a graphical calculating device 
#' that provides a visual representation of a regression model to calculate 
#' individualized predictions and risk scores. For survival models, it includes
#' survival probability predictions at specified time points.
#'
#' The nomogram displays:
#' - A points scale (0-100) for scoring variable contributions
#' - Individual variable scales with their ranges and point contributions  
#' - A total points scale for summing individual variable points
#' - Survival probability scales for clinical predictions at specified time points
#'
#' @param breg A regression object with results (must pass `assert_breg_obj_with_results()`).
#' @param idx Length-1 vector. Index or name (focal variable) of the model.
#' Only one model is supported for nomogram visualization.
#' @param funlabel Character string for the function label on the nomogram.
#' Default is "Linear Predictor" for Cox models.
#' @param fun Function to be applied to the linear predictor. For Cox models,
#' this could be the survival function at a specific time point.
#' @param fun.at Numeric vector of points where the function should be evaluated.
#' @param lp Logical indicating whether to include linear predictor axis.
#' Default is `TRUE`.
#' @param points Logical indicating whether to include points axis for calculating
#' total score. Default is `TRUE`.
#' @param total.points Logical indicating whether to include total points axis.
#' Default is `TRUE`.
#' @param surv.at Numeric vector of time points (in years) for survival probability
#' prediction. Default is c(3, 5, 10) for 3, 5, and 10-year survival.
#' @param time.inc Time increment for baseline survival calculation.
#' Default is 365.25 (days per year).
#' @param ... Additional arguments passed to the plotting function.
#' @returns A plot
#' @export
#' @family br_show
#' @examples
#' # Cox proportional hazards model
#' lung <- survival::lung |>
#'   dplyr::filter(ph.ecog != 3)
#' lung$ph.ecog <- factor(lung$ph.ecog)
#' 
#' mds <- br_pipeline(
#'   lung,
#'   y = c("time", "status"),
#'   x = c("age", "ph.ecog"),
#'   x2 = "sex",
#'   method = "coxph"
#' )
#' 
#' # Create nomogram for the first model (age) with default survival times
#' br_show_nomogram(mds, idx = 1)
#' 
#' # Create nomogram for the second model (ph.ecog)  
#' br_show_nomogram(mds, idx = 2)
#' 
#' # Use with custom survival time points and labels
#' br_show_nomogram(mds, idx = "age", surv.at = c(1, 2, 5), 
#'                  funlabel = "Log Hazard")
#' 
#' @testexamples
#' expect_true(inherits(br_show_nomogram(mds, idx = 1), "ggplot"))
br_show_nomogram <- function(
    breg, 
    idx = 1, 
    funlabel = "Linear Predictor",
    fun = NULL,
    fun.at = NULL,
    lp = TRUE,
    points = TRUE,
    total.points = TRUE,
    surv.at = c(3, 5, 10),
    time.inc = 365.25,
    ...) {
  
  assert_breg_obj_with_results(breg)
  if (length(idx) != 1) {
    cli_abort("length-1 {.arg idx} (integer index or a focal variable name) is required")
  }

  # Get the specific model
  mod <- br_get_models(breg, idx)
  # br_get_models returns the model directly when idx has length 1
  
  # Check if it's a supported model type
  model_name <- insight::model_name(mod)
  if (!model_name %in% c("coxph", "survreg")) {
    cli_abort("nomogram visualization is currently only supported for survival models (coxph, survreg)")
  }
  
  # Get model results and data
  results <- br_get_results(breg, tidy = FALSE)
  model_data <- br_get_data(breg)
  
  # For nomogram, we want to show all variables in the specific model
  # The idx parameter selects which model to visualize
  # We need to get the focal variable name for this model
  model_names <- br_get_model_names(breg)
  if (is.character(idx)) {
    focal_var <- idx
  } else {
    focal_var <- model_names[idx]
  }
  
  # Filter results for the specific model's focal variable
  model_results <- results |>
    dplyr::filter(.data$Focal_variable == focal_var)
  
  # Create a nomogram using ggplot2 following medical nomogram conventions
  .create_nomogram_plot(mod, model_results, model_data, funlabel, fun, fun.at, lp, points, total.points, surv.at, time.inc, ...)
}

# Helper function to create nomogram plot following medical nomogram conventions
.create_nomogram_plot <- function(model, results, data, funlabel, fun, fun.at, lp, points, total.points, surv.at, time.inc, ...) {
  
  # Extract model coefficients (use log scale for Cox models)
  coefs <- stats::coef(model)
  
  # Get variable information from results - handle all variables in the model
  all_vars <- results |>
    dplyr::select(.data$variable, .data$estimate, .data$var_type, .data$var_class, .data$label, .data$reference_row) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$variable)
  
  if (nrow(all_vars) == 0) {
    cli_abort("No variables found for nomogram creation")
  }
  
  # Calculate nomogram structure like rms package
  nomogram_scales <- .build_nomogram_scales(model, all_vars, data, coefs)
  
  if (length(nomogram_scales$variables) == 0) {
    cli_abort("Unable to create nomogram scales from model results")
  }
  
  # Calculate survival probabilities for Cox models
  survival_scales <- NULL
  if (insight::model_name(model) == "coxph" && !is.null(surv.at) && length(surv.at) > 0) {
    baseline_surv <- tryCatch({
      survival::survfit(model)
    }, error = function(e) {
      cli_warn("Could not calculate baseline survival: {e$message}")
      NULL
    })
    
    if (!is.null(baseline_surv)) {
      survival_scales <- .build_survival_scales(model, baseline_surv, surv.at, time.inc, nomogram_scales)
    }
  }
  
  # Create the nomogram plot
  .plot_nomogram(nomogram_scales, survival_scales, points, total.points, lp, funlabel, surv.at)
}

# Build nomogram scales following rms conventions
.build_nomogram_scales <- function(model, all_vars, data, coefs) {
  
  # Initialize scales
  variable_scales <- list()
  max_points <- 100
  
  # Get unique variables (excluding reference rows for factors)
  unique_vars <- unique(all_vars$variable)
  
  # Process each variable
  for (var_name in unique_vars) {
    var_data <- all_vars[all_vars$variable == var_name, ]
    
    if (!var_name %in% names(data)) {
      next
    }
    
    var_type <- var_data$var_type[1]
    var_class <- var_data$var_class[1]
    
    if (var_type == "continuous") {
      # For continuous variables, create a range and calculate points
      var_range <- range(data[[var_name]], na.rm = TRUE)
      
      # Get coefficient for this variable
      if (var_name %in% names(coefs)) {
        coef_val <- coefs[var_name]
        
        # Create sequence of values across the range
        var_seq <- seq(var_range[1], var_range[2], length.out = 11)
        
        # Calculate points for each value (relative to minimum)
        points_contrib <- coef_val * (var_seq - var_range[1])
        
        variable_scales[[var_name]] <- list(
          variable = var_name,
          type = "continuous", 
          values = var_seq,
          points = points_contrib,
          range = var_range,
          coefficient = coef_val
        )
      }
      
    } else if (var_class == "factor") {
      # For factor variables, use levels and their estimates
      
      # Get all levels including reference
      factor_data <- var_data[!is.na(var_data$estimate) | var_data$reference_row, ]
      
      if (nrow(factor_data) > 0) {
        # Include reference level (0 points) 
        ref_row <- factor_data[factor_data$reference_row %in% TRUE, ]
        non_ref_rows <- factor_data[!factor_data$reference_row %in% TRUE & !is.na(factor_data$estimate), ]
        
        if (nrow(ref_row) > 0) {
          level_names <- c(ref_row$label[1], non_ref_rows$label)
          point_values <- c(0, non_ref_rows$estimate)
        } else {
          level_names <- non_ref_rows$label
          point_values <- non_ref_rows$estimate
        }
        
        variable_scales[[var_name]] <- list(
          variable = var_name,
          type = "factor",
          values = level_names,
          points = point_values,
          levels = level_names
        )
      }
    }
  }
  
  # Normalize all points to 0-100 scale
  all_points <- unlist(lapply(variable_scales, function(x) x$points))
  if (length(all_points) > 0) {
    max_abs_point <- max(abs(all_points), na.rm = TRUE)
    if (max_abs_point > 0) {
      for (i in seq_along(variable_scales)) {
        variable_scales[[i]]$points_norm <- (variable_scales[[i]]$points / max_abs_point) * max_points
      }
    }
  }
  
  # Calculate total points range
  total_range <- range(unlist(lapply(variable_scales, function(x) x$points_norm)), na.rm = TRUE)
  
  return(list(
    variables = variable_scales,
    total_range = total_range,
    max_points = max_points
  ))
}

# Build survival probability scales
.build_survival_scales <- function(model, baseline_surv, surv.at, time.inc, nomogram_scales) {
  
  # Convert survival times from years to model time units
  surv_times <- surv.at * time.inc
  
  survival_scales <- list()
  
  for (i in seq_along(surv.at)) {
    surv_time <- surv_times[i]
    
    # Find closest time point in baseline survival
    time_idx <- which.min(abs(baseline_surv$time - surv_time))
    if (length(time_idx) > 0 && time_idx <= length(baseline_surv$surv)) {
      baseline_surv_t <- baseline_surv$surv[time_idx]
      
      # Calculate range of linear predictors
      lp_range <- nomogram_scales$total_range / nomogram_scales$max_points * 2  # Scale back to approximate LP
      lp_seq <- seq(lp_range[1], lp_range[2], length.out = 21)
      
      # Calculate survival probabilities: S(t|x) = S0(t)^exp(LP)
      surv_probs <- baseline_surv_t^exp(lp_seq)
      
      # Map probabilities back to points scale
      prob_at_points <- approx(lp_seq, surv_probs, 
                              xout = seq(lp_range[1], lp_range[2], length.out = 11))
      
      survival_scales[[paste0("surv_", surv.at[i])]] <- list(
        time_years = surv.at[i],
        probabilities = prob_at_points$y,
        points_scale = seq(nomogram_scales$total_range[1], nomogram_scales$total_range[2], length.out = 11)
      )
    }
  }
  
  return(survival_scales)
}

# Plot the nomogram using ggplot2
.plot_nomogram <- function(nomogram_scales, survival_scales, points, total.points, lp, funlabel, surv.at) {
  
  # Prepare data for plotting
  plot_data <- data.frame()
  
  # Add variable scales
  for (i in seq_along(nomogram_scales$variables)) {
    var_scale <- nomogram_scales$variables[[i]]
    
    # Create data frame for this variable
    var_df <- data.frame(
      variable = var_scale$variable,
      axis_type = "variable",
      x = var_scale$points_norm,
      y = i,
      label = if (var_scale$type == "continuous") {
        round(var_scale$values, 1)
      } else {
        var_scale$values
      },
      stringsAsFactors = FALSE
    )
    
    plot_data <- rbind(plot_data, var_df)
  }
  
  n_vars <- length(nomogram_scales$variables)
  
  # Create the base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    ) +
    ggplot2::labs(
      title = "Nomogram for Survival Prediction",
      x = NULL,
      y = NULL
    )
  
  # Add variable axis lines and labels
  for (i in seq_along(nomogram_scales$variables)) {
    var_scale <- nomogram_scales$variables[[i]]
    y_pos <- i
    
    # Add axis line
    p <- p + ggplot2::annotate("segment", 
                               x = min(var_scale$points_norm), 
                               xend = max(var_scale$points_norm),
                               y = y_pos, yend = y_pos,
                               linewidth = 0.8, color = "black")
    
    # Add variable name
    p <- p + ggplot2::annotate("text", 
                               x = min(nomogram_scales$total_range) - 5, 
                               y = y_pos,
                               label = var_scale$variable, 
                               hjust = 1, vjust = 0.5, 
                               size = 3.5)
    
    # Add tick marks and labels
    if (var_scale$type == "continuous") {
      # For continuous, add ticks at regular intervals
      tick_indices <- seq(1, length(var_scale$points_norm), by = 2)
      for (j in tick_indices) {
        x_pos <- var_scale$points_norm[j]
        label_val <- round(var_scale$values[j], 1)
        
        # Tick mark
        p <- p + ggplot2::annotate("segment", 
                                   x = x_pos, xend = x_pos,
                                   y = y_pos - 0.1, yend = y_pos + 0.1,
                                   linewidth = 0.5, color = "black")
        
        # Label
        p <- p + ggplot2::annotate("text", 
                                   x = x_pos, y = y_pos + 0.3,
                                   label = as.character(label_val), 
                                   hjust = 0.5, vjust = 0, 
                                   size = 2.5)
      }
    } else {
      # For factors, add ticks for each level
      for (j in seq_along(var_scale$points_norm)) {
        x_pos <- var_scale$points_norm[j]
        label_val <- var_scale$values[j]
        
        # Tick mark
        p <- p + ggplot2::annotate("segment", 
                                   x = x_pos, xend = x_pos,
                                   y = y_pos - 0.1, yend = y_pos + 0.1,
                                   linewidth = 0.5, color = "black")
        
        # Label
        p <- p + ggplot2::annotate("text", 
                                   x = x_pos, y = y_pos + 0.3,
                                   label = as.character(label_val), 
                                   hjust = 0.5, vjust = 0, 
                                   size = 2.5)
      }
    }
  }
  
  current_y <- n_vars + 1
  
  # Add points scale if requested
  if (points) {
    points_y <- current_y
    current_y <- current_y + 1
    
    # Points axis line
    p <- p + ggplot2::annotate("segment", 
                               x = nomogram_scales$total_range[1], 
                               xend = nomogram_scales$total_range[2],
                               y = points_y, yend = points_y,
                               linewidth = 1, color = "black")
    
    # Points label
    p <- p + ggplot2::annotate("text", 
                               x = min(nomogram_scales$total_range) - 5, 
                               y = points_y,
                               label = "Points", 
                               hjust = 1, vjust = 0.5, 
                               size = 3.5)
    
    # Points tick marks and labels
    point_ticks <- seq(0, nomogram_scales$max_points, by = 20)
    for (pt in point_ticks) {
      if (pt >= nomogram_scales$total_range[1] && pt <= nomogram_scales$total_range[2]) {
        # Tick mark
        p <- p + ggplot2::annotate("segment", 
                                   x = pt, xend = pt,
                                   y = points_y - 0.1, yend = points_y + 0.1,
                                   linewidth = 0.5, color = "black")
        
        # Label
        p <- p + ggplot2::annotate("text", 
                                   x = pt, y = points_y + 0.3,
                                   label = as.character(pt), 
                                   hjust = 0.5, vjust = 0, 
                                   size = 2.5)
      }
    }
  }
  
  # Add total points scale if requested
  if (total.points) {
    total_y <- current_y
    current_y <- current_y + 1
    
    # Total points axis line
    p <- p + ggplot2::annotate("segment", 
                               x = nomogram_scales$total_range[1], 
                               xend = nomogram_scales$total_range[2],
                               y = total_y, yend = total_y,
                               linewidth = 1, color = "black")
    
    # Total points label
    p <- p + ggplot2::annotate("text", 
                               x = min(nomogram_scales$total_range) - 5, 
                               y = total_y,
                               label = "Total Points", 
                               hjust = 1, vjust = 0.5, 
                               size = 3.5)
    
    # Total points ticks (same as points scale)
    point_ticks <- seq(0, nomogram_scales$max_points, by = 20)
    for (pt in point_ticks) {
      if (pt >= nomogram_scales$total_range[1] && pt <= nomogram_scales$total_range[2]) {
        # Tick mark
        p <- p + ggplot2::annotate("segment", 
                                   x = pt, xend = pt,
                                   y = total_y - 0.1, yend = total_y + 0.1,
                                   linewidth = 0.5, color = "black")
        
        # Label
        p <- p + ggplot2::annotate("text", 
                                   x = pt, y = total_y + 0.3,
                                   label = as.character(pt), 
                                   hjust = 0.5, vjust = 0, 
                                   size = 2.5)
      }
    }
  }
  
  # Add survival probability scales if available
  if (!is.null(survival_scales) && length(survival_scales) > 0) {
    for (i in seq_along(survival_scales)) {
      surv_scale <- survival_scales[[i]]
      surv_y <- current_y
      current_y <- current_y + 1
      
      # Survival axis line
      p <- p + ggplot2::annotate("segment", 
                                 x = nomogram_scales$total_range[1], 
                                 xend = nomogram_scales$total_range[2],
                                 y = surv_y, yend = surv_y,
                                 linewidth = 1, color = "red")
      
      # Survival label
      p <- p + ggplot2::annotate("text", 
                                 x = min(nomogram_scales$total_range) - 5, 
                                 y = surv_y,
                                 label = paste0(surv_scale$time_years, "-Year Survival"), 
                                 hjust = 1, vjust = 0.5, 
                                 size = 3.5)
      
      # Add probability tick marks
      prob_values <- c(0.9, 0.7, 0.5, 0.3, 0.1)
      for (prob in prob_values) {
        # Find corresponding x position
        prob_idx <- which.min(abs(surv_scale$probabilities - prob))
        if (length(prob_idx) > 0 && prob_idx <= length(surv_scale$points_scale)) {
          x_pos <- surv_scale$points_scale[prob_idx]
          
          # Tick mark
          p <- p + ggplot2::annotate("segment", 
                                     x = x_pos, xend = x_pos,
                                     y = surv_y - 0.1, yend = surv_y + 0.1,
                                     linewidth = 0.5, color = "red")
          
          # Label
          p <- p + ggplot2::annotate("text", 
                                     x = x_pos, y = surv_y + 0.3,
                                     label = paste0(round(prob * 100), "%"), 
                                     hjust = 0.5, vjust = 0, 
                                     size = 2.5, color = "red")
        }
      }
    }
  }
  
  # Set y-axis limits and labels
  y_labels <- c(sapply(nomogram_scales$variables, function(x) x$variable))
  if (points) y_labels <- c(y_labels, "Points")
  if (total.points) y_labels <- c(y_labels, "Total Points") 
  if (!is.null(survival_scales)) {
    surv_labels <- sapply(survival_scales, function(x) paste0(x$time_years, "-Year Survival"))
    y_labels <- c(y_labels, surv_labels)
  }
  
  p <- p + ggplot2::scale_y_continuous(
    limits = c(0.5, current_y - 0.5),
    breaks = seq_along(y_labels),
    labels = y_labels
  ) +
  ggplot2::scale_x_continuous(
    limits = c(min(nomogram_scales$total_range) - 20, max(nomogram_scales$total_range) + 10)
  )
  
  return(p)
}


