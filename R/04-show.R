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

#' Show a circular forest plot for regression results
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function creates a circular (polar) forest plot from regression results,
#' providing an alternative visualization to the traditional linear forest plot.
#' The function uses the same input as [br_show_forest()] but displays the results
#' in a circular format using [ggplot2::coord_polar()].
#'
#' @inheritParams br_show_forest
#' @param style Character string specifying the style of circular forest plot.
#' Options are:
#' - `"points"` (default): Display point estimates with error bars in circular format
#' - `"bars"`: Display as bars with points overlaid (inspired by reference code 2)
#' @param reference_line Logical indicating whether to show reference lines.
#' If `TRUE`, adds dashed horizontal lines at y = 0 and other meaningful values.
#' @param sort_by Character string specifying how to sort the variables.
#' Options are:
#' - `"none"` (default): No sorting, use original order
#' - `"estimate"`: Sort by effect estimate (ascending)
#' - `"estimate_desc"`: Sort by effect estimate (descending)
#' - `"pvalue"`: Sort by p-value (ascending, most significant first)
#' - `"variable"`: Sort alphabetically by variable name
#' @param ... Additional arguments passed to ggplot2 functions.
#' @returns A ggplot object
#' @export
#' @family br_show
#' @examples
#' m <- br_pipeline(mtcars,
#'   y = "mpg",
#'   x = colnames(mtcars)[2:4],
#'   x2 = "vs",
#'   method = "gaussian"
#' )
#' br_show_forest_circle(m)
#' br_show_forest_circle(m, clean = TRUE, style = "bars")
#' br_show_forest_circle(m, sort_by = "estimate")
#' @testexamples
#' assert_s3_class(br_show_forest_circle(m), "ggplot")
br_show_forest_circle <- function(
    breg,
    clean = TRUE,
    rm_controls = FALSE,
    style = c("points", "bars"),
    reference_line = TRUE,
    sort_by = c("none", "estimate", "estimate_desc", "pvalue", "variable"),
    subset = NULL,
    drop = NULL,
    log_first = FALSE) {

  assert_breg_obj_with_results(breg)
  assert_bool(rm_controls)
  assert_bool(reference_line)
  style <- match.arg(style)
  sort_by <- match.arg(sort_by)

  # Get the data using the same logic as br_show_forest
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
  ref_line_value <- if (exponentiate && !log_first) 1L else 0L

  if (rm_controls) {
    dt <- dt |> dplyr::filter(.data$Focal_variable == .data$variable)
  }

  subset <- rlang::enquo(subset)
  if (!rlang::quo_is_null(subset)) {
    dt <- dt |> dplyr::filter(!!subset)
  }

  has_group <- !is.null(br_get_group_by(breg))

  # Clean data following br_show_forest logic but adapted for circular plot
  if (clean) {
    dt <- dt |>
      dplyr::mutate(
        label = if_else(
          vctrs::vec_equal(.data$variable, .data$label, na_equal = TRUE),
          "", .data$label
        )
      )
  }

  # Create a unique identifier for positioning and handle missing values
  dt <- dt |>
    dplyr::mutate(
      # Ensure confidence intervals are valid
      conf.low = if_else(is.na(.data$conf.low) | is.infinite(.data$conf.low), .data$estimate, .data$conf.low),
      conf.high = if_else(is.na(.data$conf.high) | is.infinite(.data$conf.high), .data$estimate, .data$conf.high),
      # Filter out rows with invalid estimates
      valid_estimate = !is.na(.data$estimate) & !is.infinite(.data$estimate)
    ) |>
    dplyr::filter(.data$valid_estimate) |>
    dplyr::select(-valid_estimate)

  # If no valid data after filtering, return an empty plot
  if (nrow(dt) == 0) {
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0, y = 0, label = "No valid data to plot") +
           ggplot2::theme_void())
  }

  # Apply sorting
  if (sort_by != "none") {
    dt <- switch(sort_by,
      "estimate" = dt |> dplyr::arrange(.data$estimate),
      "estimate_desc" = dt |> dplyr::arrange(dplyr::desc(.data$estimate)),
      "pvalue" = dt |> dplyr::arrange(.data$p.value),
      "variable" = dt |> dplyr::arrange(.data$variable),
      dt  # fallback to original order
    )
  }

  # Create display labels and positioning
  dt <- dt |>
    dplyr::mutate(
      display_label = if_else(.data$label == "", .data$variable, .data$label),
      # Create unique labels in case of duplicates
      display_label = make.unique(.data$display_label, sep = "_"),
      x_pos = factor(.data$display_label, levels = unique(.data$display_label))
    )

  # Handle grouping for colors - use a more robust approach
  if (has_group && "Group_variable" %in% colnames(dt) && length(unique(dt$Group_variable)) > 1) {
    color_var <- "Group_variable"
  } else if ("Focal_variable" %in% colnames(dt) && length(unique(dt$Focal_variable)) > 1) {
    color_var <- "Focal_variable"
  } else {
    color_var <- "variable"
  }

  # Create a base offset for better visualization in polar coordinates
  base_offset <- max(abs(c(dt$conf.low, dt$conf.high, dt$estimate)), na.rm = TRUE) + 1

  if (style == "points") {
    # Style 1: Points with segments for error bars (more suitable for polar coordinates)
    # Based on reference code 2 pattern but adapted for forest plot data
    p <- ggplot2::ggplot(dt, ggplot2::aes(x = .data$x_pos)) +
      ggplot2::geom_point(
        ggplot2::aes(y = .data$estimate, color = .data[[color_var]]),
        size = 2
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          y = .data$conf.low,
          yend = .data$conf.high,
          color = .data[[color_var]]
        ),
        linewidth = 0.8
      ) +
      ggplot2::coord_polar()
  } else {
    # Style 2: Bars with points overlaid (closely following reference code 2)
    dt <- dt |>
      dplyr::mutate(
        bar_height = 1,  # Base height for bars
        point_y = .data$estimate + base_offset,  # Offset points above bars
        ci_low = .data$conf.low + base_offset,   # Offset CI accordingly
        ci_high = .data$conf.high + base_offset
      )

    p <- ggplot2::ggplot(dt, ggplot2::aes(x = .data$x_pos)) +
      ggplot2::geom_col(
        ggplot2::aes(y = .data$bar_height, fill = .data[[color_var]]),
        alpha = 0.3, width = 1
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = .data$point_y, color = .data[[color_var]]),
        size = 1.5
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          y = .data$ci_low,
          yend = .data$ci_high,
          color = .data[[color_var]]
        ),
        linewidth = 0.8
      ) +
      ggplot2::coord_polar()
  }

  # Add reference lines if requested (adjusted for style)
  if (reference_line) {
    ref_y <- if (style == "points") ref_line_value else ref_line_value + base_offset
    p <- p + ggplot2::geom_hline(
      yintercept = ref_y,
      linetype = "dashed",
      color = "gray60",
      linewidth = 0.5
    )
  }

  # Enhanced theming following reference code patterns
  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8),
      panel.grid.major.y = ggplot2::element_line(
        color = 'gray60',
        linewidth = 0.5,
        linetype = 'dashed'
      ),
      panel.grid.minor.y = ggplot2::element_line(
        color = 'gray80',
        linewidth = 0.3,
        linetype = 'dotted'
      ),
      axis.text.x = ggplot2::element_text(size = 8, color = "black"),
      axis.text.y = ggplot2::element_text(size = 8, color = "black"),
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Circular Forest Plot",
      color = gsub("_", " ", color_var)
    )

  # Use color palette similar to reference code
  n_groups <- length(unique(dt[[color_var]]))
  if (n_groups > 1) {
    # Colors inspired by reference code
    colors <- c('#3cc34e', '#00aeff', '#ff800e', '#6A51A3', '#2B8CBE', '#E31A1C', '#FF7F00', '#33A02C')
    if (n_groups > length(colors)) {
      colors <- rainbow(n_groups)
    }
    colors <- colors[1:n_groups]

    p <- p + ggplot2::scale_color_manual(values = colors)

    # Only add fill scale if style uses bars (which uses fill aesthetic)
    if (style == "bars") {
      p <- p + ggplot2::scale_fill_manual(values = colors, guide = "none")
    }
  }

  return(p)
}
