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
#' @param subset Expression for subsetting the results data (`br_get_results(breg)`).
#' @param drop Column indices to drop from the display table.
#' @param tab_headers Character vector of custom column headers (must match number of displayed columns).
#'
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
    tab_headers = NULL) {
  assert_breg_obj_with_results(breg)
  assert_bool(rm_controls)

  # TODO: grouped (compared) forestplot for group_by???
  dots <- rlang::list2(...)
  exponentiate <- attr(breg, "exponentiate")
  if (exponentiate) {
    dots[["x_trans"]] <- "log"
  }

  dt <- br_get_results(breg)
  x2 <- br_get_x2(breg)

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
      "n_obs" = "N"
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

  mds <- if (!is.null(idx)) {
    br_get_model(breg, idx)
  } else {
    br_get_models(breg)
  }
  if (length(mds) == 1) {
    mds <- mds[[1]]
  }
  .f <- if (identical(class(mds), "list")) {
    ggstats::ggcoef_compare
  } else {
    ggstats::ggcoef_table
  }

  do.call(.f, vctrs::vec_c(list(mds), list(...)))
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

  mod <- br_get_model(breg, idx)
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
  mod <- br_get_model(breg, idx)
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

  mod <- br_get_model(breg, idx)
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
