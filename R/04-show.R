# Show model results in plots, tables, and some other formats
# Set a family of functions, instead of including them all into one doc file

br_show_forest <- function(breg, ...) {
  assert_breg_obj_with_results(breg)
}

#' Show forest with `ggstats` interface
#'
#' Provide an interface to visualize the model results with [**ggstats**](https://github.com/larmarange/ggstats/) package.
#' Illustration for arguments and examples could be found at [`ggcoef_model` reference page](https://larmarange.github.io/ggstats/reference/ggcoef_model.html), or please check the doc for dynamic dots `...`.
#'
#' @param breg An object of class `breg` with results.
#' @param idx Index or names (focal variables) of the model(s).
#' @param ... Arguments passing to [ggstats::ggcoef_table()] or [ggstats::ggcoef_compare()].
#' @export
#' @family br_show
br_show_forest_ggstats <- function(breg, ..., idx = NULL) {
  assert_breg_obj_with_results(breg)
  # if (!is_installed("ggstats")) {
  #   cli_inform("installing required package {.pkg ggstats}")
  #   install_pkgs("ggstats")
  # }
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
    # if (use_dodged) ggstats::ggcoef_dodged else ggstats::ggcoef_faceted
    ggstats::ggcoef_compare
  } else {
    ggstats::ggcoef_table
  }

  do.call(.f, vctrs::vec_c(list(mds), list(...)))
}

br_show_table <- function(breg, ...) {
  assert_breg_obj_with_results(breg)
}
