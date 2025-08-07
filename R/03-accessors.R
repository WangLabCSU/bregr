#' Accessor functions for `breg` objects
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' These functions provide access to components of `breg` objects, serving as counterparts
#' to the `br_set_*()` functions. Some functions include additional arguments for extended
#' functionality.
#'
#' @returns
#' Output depends on the function called:
#' - `br_get_data()` returns a `data.frame`.
#' - `br_get_y()`, `br_get_x()`, `br_get_x2()` return modeling terms.
#' - `br_get_n_x()` and `br_get_n_x2()` return the length of terms `x` and `x2`.
#' - `br_get_group_by()` returns variable(s) for group analysis.
#' - `br_get_config()` returns modeling method and extra arguments.
#' - `br_get_models()` returns all or a subset of constructed models.
#' - `br_get_model()` returns a subset of constructed models.
#' - `br_get_results()` returns modeling result `data.frame`.
#'
#' @name accessors
#' @seealso [pipeline] for building `breg` objects.
#' @examples
#' m <- br_pipeline(mtcars,
#'   y = "mpg",
#'   x = colnames(mtcars)[2:4],
#'   x2 = "vs",
#'   method = "gaussian"
#' )
#' br_get_data(m)
#' br_get_y(m)
#' br_get_x(m)
#' br_get_n_x(m)
#' br_get_x2(m)
#' br_get_n_x2(m)
#' br_get_group_by(m)
#' br_get_config(m)
#' br_get_models(m)
#' br_get_model(m, 1)
#' br_get_n_x2(m)
#' br_get_results(m)
#' br_get_results(m, tidy = TRUE)
#' br_get_results(m, tidy = TRUE, term == "cyl")
#'
#' @testexamples
#' assert_breg_obj_with_results(m)
NULL

#' @rdname accessors
#' @param obj A `breg` object.
#' @export
br_get_data <- function(obj) {
  assert_breg_obj(obj)
  obj@data
}

#' @rdname accessors
#' @export
br_get_y <- function(obj) {
  assert_breg_obj(obj)
  obj@y
}

#' @rdname accessors
#' @export
br_get_x <- function(obj) {
  assert_breg_obj(obj)
  obj@x
}

#' @rdname accessors
#' @export
br_get_n_x <- function(obj) {
  assert_breg_obj(obj)
  obj@n_x
}

#' @rdname accessors
#' @export
br_get_x2 <- function(obj) {
  assert_breg_obj(obj)
  obj@x2
}

#' @rdname accessors
#' @export
br_get_n_x2 <- function(obj) {
  assert_breg_obj(obj)
  obj@n_x2
}

#' @rdname accessors
#' @export
br_get_group_by <- function(obj) {
  assert_breg_obj(obj)
  obj@group_by
}

#' @rdname accessors
#' @export
br_get_config <- function(obj) {
  assert_breg_obj(obj)
  obj@config
}

#' @rdname accessors
#' @param idx Index or names (focal variables) of the model(s) to return.
#' Default returns all.
#' @export
br_get_models <- function(obj, idx = NULL) {
  assert_breg_obj(obj)
  mds <- obj@models
  len <- length(mds)

  # mds could be files, models, or model construction text
  if (is.null(idx)) {
    if (!insight::is_model(mds[[1]]) && fs::is_file(mds[[1]])) {
      if (len > 1000) {
        cli::cli_inform("directly retrieve >1000 models may resource-consuming, subsetting with {.arg idx} is more recommended")
      }
      mds <- map(mds, qs::qread)
    }
  } else {
    if (is.numeric(idx)) {
      idx <- as.integer(idx)
      if (any(idx < 1 | idx > len)) {
        cli_abort("{.arg idx} index out of range (input model integer indexs)")
      }
    } else if (is.character(idx)) {
      idx <- as.character(idx)
      if (!all(idx %in% names(mds))) {
        cli_abort("{.arg idx} index out of range (input focal variable names)")
      }
    }

    mds <- mds[idx]
    rlang::check_installed("fs", "qs")
    if (!insight::is_model(mds[[1]]) && fs::is_file(mds[[1]])) {
      mds <- map(mds, qs::qread)
    }
    if (length(idx) == 1) mds <- mds[[1]]
  }
  mds
}

#' @rdname accessors
#' @export
br_get_model <- function(obj, idx) {
  lifecycle::deprecate_soft("1.1.0", "br_get_model()", "br_get_models()")
  br_get_models(obj, idx)
}
# parameters::model_parameters()

#' @rdname accessors
#' @param tidy If `TRUE` return tidy (compact) results, otherwise return comprehensive results.
#' The tidy results are obtained from [broom::tidy()] while comprehensive results are obtained from
#' [broom.helpers::tidy_plus_plus()]. The results can be configured when run with [br_run()].
#' @param ... Subset operations passing to [dplyr::filter()] to filter results.
#' @export
br_get_results <- function(obj, tidy = FALSE, ...) {
  assert_breg_obj(obj)
  if (tidy) {
    results <- obj@results_tidy
  } else {
    results <- obj@results
  }
  dplyr::filter(results, ...)
}
