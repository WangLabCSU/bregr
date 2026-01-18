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
#' - `br_get_model_names()` returns all model names.
#' - `br_rename_models()` returns a renamed object.
#' - `br_get_results()` returns modeling result `data.frame`.
#'
#' @name accessors
#' @family accessors
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
#' br_get_models(m, 1)
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
#' @param auto_drop If `TRUE`, automatically drop the list if only one model
#' is selected.
#' @export
br_get_models <- function(obj, idx = NULL, auto_drop = TRUE) {
  assert_breg_obj(obj)
  assert_logical(auto_drop, allow_na = FALSE)
  mds <- obj@models
  len <- length(mds)

  # mds could be files, models, or model construction text
  if (is.null(idx)) {
    if (!insight::is_model(mds[[1]]) && requireNamespace("fs", quietly = TRUE) && fs::is_file(mds[[1]])) {
      if (len > 1000) {
        cli::cli_inform("directly retrieve >1000 models may resource-consuming, subsetting with {.arg idx} is more recommended")
      }
      rlang::check_installed("qs2")
      mds <- map(mds, qs2::qs_read)
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
    if (!insight::is_model(mds[[1]]) && requireNamespace("fs", quietly = TRUE) && fs::is_file(mds[[1]])) {
      rlang::check_installed("qs2")
      mds <- map(mds, qs2::qs_read)
    }
    if (length(idx) == 1 && auto_drop) mds <- mds[[1]]
  }
  mds
}

#' @rdname accessors
#' @export
br_get_model <- function(obj, idx) {
  lifecycle::deprecate_soft("1.1.0", "br_get_model()", "br_get_models()")
  br_get_models(obj, idx)
}

#' @rdname accessors
#' @export
br_get_model_names <- function(obj) {
  assert_breg_obj(obj)
  names(obj@models)
}

#' @rdname accessors
#' @param new_names Character vector to replace existing model names.
#' @export
br_rename_models <- function(obj, new_names) {
  assert_breg_obj(obj)
  assert_character_len(new_names, len = length(obj@models))

  old_names <- br_get_model_names(obj)
  names(obj@models) <- new_names
  cli::cli_inform("rename model names from {.val {old_names}} to {.val {new_names}}")
  obj
}


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

#' Predict method for breg objects
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Generate predictions from fitted models in a `breg` object.
#' For Cox regression models, returns linear predictors (log relative hazard).
#' For other models, returns predicted values.
#'
#' @param obj A `breg` object with fitted models.
#' @param newdata Optional data frame for predictions. If NULL, uses original data.
#' @param idx Model index, an integer or string.
#' @param type Type of prediction. For Cox models: "lp" (linear predictor, default)
#' or "risk" (relative risk). For other models: "response" (default) or "link".
#' @returns Typically, a numeric vector of predictions.
#' @export
#' @family accessors
#' @examples
#' # Cox regression example
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   lung <- survival::lung |> dplyr::filter(ph.ecog != 3)
#'   mds <- br_pipeline(
#'     lung,
#'     y = c("time", "status"),
#'     x = c("age", "ph.ecog"),
#'     x2 = "sex",
#'     method = "coxph"
#'   )
#'   scores <- br_predict(mds)
#'   head(scores)
#' }
br_predict <- function(obj, newdata = NULL, idx = NULL, type = NULL) {
  assert_breg_obj_with_results(obj)

  # Get the model to use
  if (is.null(idx)) {
    cli::cli_inform("{.arg idx} not set, use the first model")
    idx <- 1
  } else {
    if (length(idx) != 1) {
      cli::cli_abort("please specify one model")
    }
  }
  model <- br_get_models(obj, idx)

  # Get data for prediction
  if (is.null(newdata)) {
    newdata <- br_get_data(obj)
  }

  # Determine prediction type
  model_class <- class(model)[1]
  if (is.null(type)) {
    if (model_class == "coxph") {
      # https://www.rdocumentation.org/packages/survival/versions/3.8-3/topics/predict.coxph
      type <- "lp" # linear predictor (log relative hazard)
    } else {
      type <- "response" # predicted response
    }
    cli::cli_inform("{.arg type} is not specified, use {type} for the model")
  }

  # Generate predictions
  tryCatch(
    {
      predictions <- predict(model, newdata = newdata, type = type)

      # Handle missing values
      if (any(is.na(predictions))) {
        cli::cli_warn("some predictions are NA, consider checking your data for missing values")
      }

      predictions
    },
    error = function(e) {
      cli::cli_abort("failed to generate predictions: {e$message}")
    }
  )
}
