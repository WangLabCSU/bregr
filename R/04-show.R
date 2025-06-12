# Show model results in plots, tables, and some other formats
# Set a family of functions, instead of including them all into one doc file

#' Show forest
#'
#' @param breg An object of class `breg` with results.
#' @export
#' @family br_show
br_show_forest <- function(breg, ...) {
  assert_breg_obj_with_results(breg)

  # TODO: grouped (compared) forestplot for group_by???

  dt <- br_get_results(breg)

  # if (global_p) {
  #   if (inherits(model, "coxph")) {
  #     p_val <- as.numeric(summary(model)$sctest[3])
  #     label <- paste("Global p ", format.pval(p_val, digits = 2, eps = 1e-3))
  #     forest_terms <- forest_terms |>
  #       dplyr::add_row(term_label = "Global p", variable = label)
  #   } else {
  #     message("No global p value availabe for non-Cox model")
  #   }
  # }

  # ref_line = NULL, xlim = NULL, vars = NULL, p = NULL,
  # ref_line = 1, xlim = c(0, 2)

  # if (!is.null(vars))
  #   data <- data[data$focal_term %in% vars]
  # if (!is.null(p)) {
  #   minps <- sapply(split(data, data$focal_term), function(x) min(x$p,
  #                                                                 na.rm = TRUE))
  #   vars2 <- names(minps[minps < p])
  #   data <- data[data$focal_term %in% vars2]
  # }

  ref_line <- if (inherits(model, "coxph") || (inherits(
    model,
    "glm"
  ) && model$family$link == "logit")) {
    1L
  } else {
    0L
  }
  xlim <- c(floor(min(data$CI_low, na.rm = TRUE)), ceiling(max(data$CI_high,
    na.rm = TRUE
  )))
  if (is.infinite(xlim[1])) {
    warning("\ninfinite CI detected, set a minimal value -100",
      immediate. = TRUE
    )
    xlim[1] <- -100
  }
  if (is.infinite(xlim[2])) {
    warning("\ninfinite CI detected, set a maximal value 100",
      immediate. = TRUE
    )
    xlim[2] <- 100
  }


  c(
    "Group_variable", "Focal_variable", "reference_row", "label", "n_obs",
    "estimate", "std.error", "p.value", "conf.low", "conf.high"
  )

  has_group <- !is.null(br_get_group_by(breg))
  dt <- dt |>
    dplyr::mutate(
      ` ` = paste(rep(" ", 20), collapse = " "),
      `Estimate (95% CI)` = dplyr::case_when(
        dt$reference_row ~ "Reference",
        is.na(dt$std.error) ~ "",
        TRUE ~ sprintf(
          "%.2f (%.2f to %.2f)",
          estimate,
          conf.low,
          conf.high
        )
      ),
      P = if_else(is.na(p.value), "", format.pval(p.value,
        digits = 2,
        eps = 0.001
      )),
      conf.low = if_else(is.na(conf.low), estimate, conf.low),
      conf.high = if_else(is.na(conf.high), estimate, conf.high)
    ) #|> dplyr::mutate_all(~dplyr::if_else(is.na(.), "", as.character(.)))

  sel_cols <- c(
    if (has_group) "Group_variable" else NULL,
    "Focal_variable", "variable", "label", "n_obs", " ", "Estimate (95% CI)",
    "P", "estimate", "conf.low", "conf.high"
  )
  dt2 <- dt |>
    dplyr::select(dplyr::all_of(sel_cols), dplyr::everything()) |>
    rename(c(
      "Group_variable" = "Group",
      "Focal_variable" = "Focal",
      "variable" = "Variable",
      "label" = "Level",
      "n_obs" = "N"
    ))

  idx_end <- 7L
  idx_ci <- 5L
  if (has_group) {
    idx_end <- 8L
    idx_end <- 6L
  }
  forestploter::forest(dt2[, 1:idx_end],
    est = dt$estimate,
    lower = dt$conf.low,
    upper = dt$conf.high,
    ci_column = idx_ci
  )
  forestploter::forest(dt2[, 1:idx_end],
    est = dt$estimate,
    lower = dt$conf.low,
    upper = dt$conf.high,
    ci_column = idx_ci,
    ref_line = ref_line, xlim = xlim, ...
  )
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

# TODO: show table with gtsummary
# https://github.com/WangLabCSU/bregr/issues/16
