# Show model results in plots, tables, and some other formats
# Set a family of functions, instead of including them all into one doc file

#' Show forest
#'
#' @param breg An object of class `breg` with results.
#' @export
#' @family br_show
br_show_forest <- function(breg, ...) {
  assert_breg_obj_with_results(breg)
}

#' Show forest with `ggstats` interface
#'
#' Provide an interface to visualize the model results with [**ggstats**](https://github.com/larmarange/ggstats/) package.
#' Illustration for arguments and examples could be found at [`ggcoef_model` reference page](https://larmarange.github.io/ggstats/reference/ggcoef_model.html), or please check the doc for dynamic dots `...`.
#'
#' @inheritParams br_show_forest
#' @param idx Index or names (focal variables) of the model(s).
#' @param ... Arguments passing to [ggstats::ggcoef_table()] or [ggstats::ggcoef_compare()] excepts `model`.
#' @export
#' @family br_show
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

#' Show forest with `ggstatsplot` interface
#'
#' Provide an interface to visualize the model results with [**ggstatsplot**](https://github.com/IndrajeetPatil/ggstatsplot/) package.
#' Illustration for arguments and examples could be found at [`ggcoefstats` reference page](https://indrajeetpatil.github.io/ggstatsplot/reference/ggcoefstats.html), or please check the doc for dynamic dots `...`.
#'
#' @inheritParams br_show_forest
#' @param idx Length-1. Index or name (focal variable) of the model.
#' @param ... Arguments passing to [ggstatsplot::ggcoefstats()] excepts `x`.
#' @export
#' @family br_show
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
#' Provide an interface to visualize the model results with [**visreg**](https://github.com/larmarange/ggstats/) package, to show how a predictor variable x affects an outcome y.
#' Illustration for arguments and examples could be found at [`visreg` reference page](https://pbreheny.github.io/visreg/reference/visreg.html), or please check the doc for dynamic dots `...`.
#'
#' @inheritParams br_show_forest_ggstatsplot
#' @param ... Arguments passing to [visreg::visreg()] excepts `fit`.
#' @export
#' @family br_show
br_show_fitted_line <- function(breg, idx = 1, ...) {
  assert_breg_obj_with_results(breg)
  if (length(idx) != 1) {
    cli_abort("length-1 {.arg idx} (integer index or a focal variable name) is required")
  }
  rlang::check_installed("visreg")

  mod <- br_get_model(breg, idx)
  visreg::visreg(mod, ...)
}

#' Show fitted regression line with `visreg` interface
#'
#' Similar to [br_show_fitted_line()], but visualize how two variables interact to affect the response in regression models.
#' Illustration for arguments and examples could be found at [`visreg2d` reference page](https://pbreheny.github.io/visreg/reference/visreg2d.html), or please check the doc for dynamic dots `...`.
#'
#' @inheritParams br_show_forest_ggstatsplot
#' @param ... Arguments passing to [visreg::visreg2d()] excepts `fit`.
#' @export
#' @family br_show
br_show_fitted_line_2d <- function(breg, idx = 1, ...) {
  assert_breg_obj_with_results(breg)
  if (length(idx) != 1) {
    cli_abort("length-1 {.arg idx} (integer index or a focal variable name) is required")
  }
  rlang::check_installed("visreg")

  mod <- br_get_model(breg, idx)
  visreg::visreg2d(mod, ...)
}

#' Show model tidy results in table format
#'
#'
#' @inheritParams br_show_forest
#' @param ... Arguments passing to [br_get_results()] for subsetting table.
#' @param args_table_format A list of arguments passing to [insight::format_table()].
#' @param export Logical. If `TRUE`, show table for export purpose, e.g., present the table in Markdown or HTML format.
#' @param args_table_export A list of arguments passing to [insight::export_table()]. Only works when `export` is `TRUE`.
#' @export
#' @family br_show
br_show_table <- function(breg, ..., args_table_format = list(), export = FALSE, args_table_export = list()) {
  assert_breg_obj_with_results(breg)

  tidy_result <- br_get_results(breg, tidy = TRUE, ...)
  tbl <- do.call(insight::format_table, vctrs::vec_c(list(tidy_result), args_table_format))
  if (export) tbl <- do.call(insight::export_table, vctrs::vec_c(list(tbl), args_table_export))
  tbl
}
