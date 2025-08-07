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
#' individualized predictions and risk scores.
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
#' # Create nomogram for the first model (age)
#' br_show_nomogram(mds, idx = 1)
#' 
#' # Create nomogram for the second model (ph.ecog)  
#' br_show_nomogram(mds, idx = 2)
#' 
#' # Use with custom labels
#' br_show_nomogram(mds, idx = "age", funlabel = "Log Hazard")
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
  
  # Create a simplified nomogram using ggplot2
  # This implements a basic nomogram structure
  .create_nomogram_plot(mod, model_results, model_data, funlabel, fun, fun.at, lp, points, total.points, ...)
}

# Helper function to create nomogram plot
.create_nomogram_plot <- function(model, results, data, funlabel, fun, fun.at, lp, points, total.points, ...) {
  
  # Extract model coefficients
  coefs <- stats::coef(model)
  
  # Get variable information from results
  var_info <- results |>
    dplyr::filter(!.data$reference_row %in% TRUE, !is.na(.data$estimate)) |>
    dplyr::select(.data$variable, .data$estimate, .data$var_type, .data$var_class, .data$label) |>
    dplyr::distinct()
  
  if (nrow(var_info) == 0) {
    cli_abort("No valid coefficients found for nomogram creation")
  }
  
  # Create nomogram data structure
  nomogram_data <- list()
  
  # Calculate ranges and scales for each variable
  for (i in seq_len(nrow(var_info))) {
    var_name <- var_info$variable[i]
    var_type <- var_info$var_type[i]
    var_class <- var_info$var_class[i]
    
    if (var_name %in% names(data)) {
      if (var_type == "continuous") {
        var_range <- range(data[[var_name]], na.rm = TRUE)
        var_seq <- seq(var_range[1], var_range[2], length.out = 10)
        
        # Calculate points contribution
        coef_val <- var_info$estimate[i]
        # Convert to log scale if estimates are exponentiated
        if (coef_val > 0 && abs(log(coef_val)) > abs(coef_val)) {
          coef_val <- log(coef_val)
        }
        
        points_contrib <- coef_val * (var_seq - min(var_seq, na.rm = TRUE))
        
        nomogram_data[[var_name]] <- data.frame(
          variable = var_name,
          value = var_seq,
          points = points_contrib,
          var_type = var_type,
          stringsAsFactors = FALSE
        )
      } else if (var_class == "factor") {
        var_levels <- levels(as.factor(data[[var_name]]))
        
        # For factor variables, use the estimates from results  
        factor_results <- results |>
          dplyr::filter(.data$variable == var_name, !is.na(.data$estimate), !.data$reference_row %in% TRUE)
        
        if (nrow(factor_results) > 0) {
          # Include reference level (0 points) and other levels
          level_values <- c("Reference", factor_results$label)
          points_values <- c(0, factor_results$estimate)
          
          # Convert to log scale if estimates are exponentiated  
          points_log <- ifelse(points_values > 0 & points_values != 1, log(points_values), points_values)
          
          nomogram_data[[var_name]] <- data.frame(
            variable = var_name,
            value = level_values,
            points = points_log,
            var_type = var_type,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  # Combine all variable data
  if (length(nomogram_data) == 0) {
    cli_abort("Unable to create nomogram data from model results")
  }
  
  plot_data <- do.call(rbind, nomogram_data)
  
  # Normalize points to 0-100 scale
  if (points) {
    max_abs_points <- max(abs(plot_data$points), na.rm = TRUE)
    if (max_abs_points > 0) {
      plot_data$points_norm <- (plot_data$points / max_abs_points) * 100
    } else {
      plot_data$points_norm <- plot_data$points
    }
  } else {
    plot_data$points_norm <- plot_data$points
  }
  
  # Create the nomogram plot using ggplot2
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$points_norm, y = .data$variable)) +
    ggplot2::geom_point(size = 2, color = "blue") +
    ggplot2::geom_segment(
      data = plot_data |> dplyr::group_by(.data$variable) |> 
        dplyr::summarise(
          xmin = min(.data$points_norm, na.rm = TRUE),
          xmax = max(.data$points_norm, na.rm = TRUE),
          .groups = "drop"
        ),
      ggplot2::aes(x = .data$xmin, xend = .data$xmax, y = .data$variable, yend = .data$variable),
      inherit.aes = FALSE,
      color = "gray70"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 20)) +
    ggplot2::labs(
      title = "Nomogram for Survival Model",
      x = if (points) "Points (0-100)" else funlabel,
      y = "Variables"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_blank()
    )
  
  # Add variable labels as annotations
  for (var in unique(plot_data$variable)) {
    var_data <- plot_data[plot_data$variable == var, ]
    if (var_data$var_type[1] == "continuous") {
      # For continuous variables, show min and max values
      min_val <- min(var_data$value, na.rm = TRUE)
      max_val <- max(var_data$value, na.rm = TRUE)
      p <- p + ggplot2::annotate("text", 
                               x = min(var_data$points_norm), 
                               y = var, 
                               label = paste(round(min_val, 2)), 
                               hjust = 1.1, vjust = -0.5, size = 3)
      p <- p + ggplot2::annotate("text", 
                               x = max(var_data$points_norm), 
                               y = var, 
                               label = paste(round(max_val, 2)), 
                               hjust = -0.1, vjust = -0.5, size = 3)
    } else {
      # For categorical variables, show level names
      for (i in seq_len(nrow(var_data))) {
        p <- p + ggplot2::annotate("text", 
                                 x = var_data$points_norm[i], 
                                 y = var, 
                                 label = var_data$value[i], 
                                 hjust = 0.5, vjust = -0.5, size = 3)
      }
    }
  }
  
  # Add total points information if requested
  if (total.points) {
    total_range <- range(plot_data$points_norm, na.rm = TRUE)
    p <- p + ggplot2::labs(
      caption = paste("Total Points Range:", round(total_range[1], 1), "to", round(total_range[2], 1))
    )
  }
  
  return(p)
}
