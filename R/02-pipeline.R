#' Modeling and analysis pipeline
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Provides a set of functions for running batch regression analysis.
#' Combines data setup, model configuration, and execution steps into a single workflow.
#' Supports both GLM and Cox-PH models with options for focal/control terms and parallel processing.
#'
#' @details
#' Please note the difference between [variables](https://easystats.github.io/insight/#variables) and
#' [terms](https://easystats.github.io/insight/#terms),
#' e.g., `x + poly(x, 2)` has *one* variable `x`, but *two* terms `x` and `poly(x, 2)`.
#'
#' @returns
#' An object of class `breg` with input values added to corresponding slot(s).
#' For `br_run()`, the returned object is a `breg` object with results added to
#' the slots `@results` and `@results_tidy`, note that `@models` is updated to a list
#' of constructed model object (See [accessors]).
#'
#' @name pipeline
#' @param data A `data.frame` containing all necessary variables for analysis.
#' Column names should follow R's naming conventions.
#' @param obj An object of class `breg`.
#' @param y Character vector specifying dependent variables (response variables).
#' For GLM models, this is typically a single character (e.g., `"outcome"`).
#' For Cox-PH models, it should be a length-2 vector in the format `c("time", "status")`.
#' @param x Character vector specifying focal independent terms (predictors).
#' @param x2 Character vector specifying control independent terms (predictor, optional).
#' @param method Method for model construction.
#' A string representing a complex method setting is acceptable,
#' e.g., 'quasi(variance = "mu", link = "log")'.
#' @param group_by A string specifying the group by column.
#' @param run_parallel Integer, indicating cores to run the task, default is `1`.
#' @param ... Additional arguments depending on the called function.
#' - `br_set_x()` for passing focal terms as characters.
#' - `br_set_x2()` for passing control terms as characters.
#' - `br_set_model()` for passing other configurations for modeling.
#' - `br_run()` for passing other configurations for obtaining modeling results with [tidy_plus_plus()].
#' e.g., The default value for `exponentiate` is `FALSE` (coefficients are not exponentiated).
#' For logistic, and Cox-PH regressions models, `exponentiate` is set to `TRUE` at default.
#' @param model_args A list of arguments passed to `br_set_model()`.
#' @param run_args A list of arguments passed to `br_run()`.
#' @examples
#' library(bregr)
#' # 1. Pipeline -------------------------
#' # 1.1. A single linear model ----------
#' m <- breg(mtcars) |> # set model data
#'   br_set_y("mpg") |> # set dependent variable
#'   br_set_x("qsec") |> # set focal variables
#'   br_set_model("gaussian") |> # set model
#'   br_run() # run analysis
#'
#' # get model tidy result
#' br_get_results(m, tidy = TRUE)
#' # or m@results_tidy
#'
#' # compare with R's built-in function
#' lm(mpg ~ qsec, data = mtcars) |> summary()
#' # 1.2. Batch linear model -------------
#' # control variables are injected in all constructed models
#' # focal variables are injected in constructed models one by one
#' m2 <- breg(mtcars) |>
#'   br_set_y("mpg") |>
#'   br_set_x(colnames(mtcars)[2:4]) |> # set focal variables
#'   br_set_x2("vs") |> # set control variables
#'   br_set_model("gaussian") |>
#'   br_run()
#' # 1.3. Group by model -------------
#' m3 <- breg(mtcars) |>
#'   br_set_y("mpg") |>
#'   br_set_x("cyl") |>
#'   br_set_x2("wt") |> # set control variables
#'   br_set_model("gaussian") |>
#'   br_run(group_by = "am")
#'
#' # 2. All-in-one pipeline wrapper ---
#' m4 <- br_pipeline(mtcars,
#'   y = "mpg",
#'   x = colnames(mtcars)[2:4],
#'   x2 = "vs",
#'   method = "gaussian"
#' )
#'
#' @testexamples
#' assert_breg_obj(m)
#' assert_breg_obj(m2)
#' assert_breg_obj(m3)
#' assert_breg_obj(m4)
#' @seealso [accessors] for accessing `breg` object properties.
NULL

#' @describeIn pipeline All-in-one end to end wrapper to run the regression analysis in batch.
#' Which could be splitted into the following steps
#' @export
br_pipeline <- function(
    data, y, x, method, x2 = NULL,
    group_by = NULL, run_parallel = 1L,
    model_args = list(),
    run_args = list()) {
  breg(data) |>
    br_set_y(y) |>
    br_set_x(x) |>
    br_set_x2(x2) |>
    br_set_model(method = method, !!!model_args) |>
    br_run(group_by = group_by, run_parallel = run_parallel, !!!run_args)
}

#' @describeIn pipeline Set dependent variables for model construction.
#' @export
br_set_y <- function(obj, y) {
  assert_breg_obj(obj)
  assert_character(y)

  data <- br_get_data(obj)
  if (nrow(data) == 0) {
    cli_abort("cannot set {.arg y} for {.arg obj} with void data")
  } else {
    .in <- y %in% colnames(data)
    if (!all(.in)) {
      cli_abort("column(s) {.val {y[!.in]}} specified in {.arg y} not in {.field data} (columns: {.val {colnames(data)}}) of {.arg obj}")
    }
  }

  obj@y <- y
  obj
}

#' @describeIn pipeline Set focal terms for model construction.
#' @export
br_set_x <- function(obj, ...) {
  assert_breg_obj(obj)

  x <- rlang::list2(...) |>
    unlist() |>
    as.character()
  assert_character(x, allow_na = FALSE)

  x_ <- get_vars(x)
  data <- br_get_data(obj)
  if (nrow(data) == 0) {
    cli_abort("cannot set {.arg x} for {.arg obj} with void data")
  } else {
    .in <- x_ %in% colnames(data)
    if (!all(.in)) {
      cli_abort("column(s) {.val {x_[!.in]}} specified in {.arg x} not in {.field data} (columns: {.val {colnames(data)}}) of {.arg obj}")
    }
  }

  obj@x <- x
  obj
}

#' @describeIn pipeline  Set control terms for model construction (Optional in pipeline).
#' @export
br_set_x2 <- function(obj, ...) {
  assert_breg_obj(obj)
  x <- br_get_x(obj)
  if (is.null(x)) {
    cli_abort("{.fn br_set_x2()} should be called after {.fn br_set_x()}")
  }

  x2 <- rlang::list2(...) |>
    unlist() |>
    as.character()
  assert_character(x2, allow_na = FALSE, allow_null = TRUE)
  assert_not_overlap(x, x2)

  x2_ <- get_vars(x2)
  data <- br_get_data(obj)
  if (nrow(data) == 0) {
    cli_abort("cannot set {.arg x2} for {.arg obj} with void data")
  } else {
    .in <- x2_ %in% colnames(data)
    if (!all(.in)) {
      cli_abort("column(s) {.val {x2_[!.in]}} specified in {.arg x2} not in {.field data} (columns: {.val {colnames(data)}}) of {.arg obj}")
    }
  }

  obj@x2 <- x2
  obj
}


#' @describeIn pipeline Set model configurations.
#' @export
br_set_model <- function(obj, method, ...) {
  assert_breg_obj(obj)
  assert_string(method, allow_empty = FALSE)

  # TODO: method is a string represent a call
  # e.g., 'quasi(variance = "mu", link = "log")'
  if (!grepl("\\(", method)) {
    rlang::arg_match0(method, br_avail_methods())
  }

  config <- rlang::list2(...)
  config_text <- gsub(
    "^list\\(|\\)$", "",
    paste(deparse(config, width.cutoff = 500),
      collapse = ""
    )
  )

  if (method == "coxph") {
    assert_character_len(
      obj@y,
      len = 2,
      msg = "two dependent variables corresponding to {.val time} and {.val status} are required for Cox proportional hazards model"
    )

    models <- list()
    for (i in seq_len(obj@n_x)) {
      recipe <- glue::glue("survival::Surv({paste(obj@y, collapse = ', ')}) ~ {paste(vctrs::vec_c(obj@x[i], obj@x2), collapse = ' + ')}")
      if (identical(config, list())) {
        models[[i]] <- glue::glue("survival::coxph({recipe}, data = data)")
      } else {
        models[[i]] <- glue::glue("survival::coxph({recipe}, data = data, {config_text})")
      }
    }
  } else {
    assert_character_len(
      obj@y,
      len = 1,
      msg = "only one dependent variable is allowed for non-Cox proportional hazards models"
    )

    models <- list()
    for (i in seq_len(obj@n_x)) {
      recipe <- glue::glue("{paste(obj@y, collapse = ', ')} ~ {paste(vctrs::vec_c(obj@x[i], obj@x2), collapse = ' + ')}")

      if (identical(config, list())) {
        models[[i]] <- glue::glue("stats::glm({recipe}, data = data, family = {method})")
      } else {
        models[[i]] <- glue::glue("stats::glm({recipe}, data = data, family = {method}, {config_text})")
      }
    }
  }

  names(models) <- obj@x
  obj@config <- list(method = method, extra = config_text)
  obj@models <- models
  obj
}

#' @describeIn pipeline Run the regression analysis in batch.
#' @export
br_run <- function(obj, ..., group_by = NULL, run_parallel = 1L) {
  assert_breg_obj(obj)
  assert_character(group_by, allow_na = FALSE, allow_null = TRUE)
  assert_number_whole(run_parallel, min = 1, max = parallel::detectCores() - 1)

  if (.Platform$OS.type == "windows") {
    cli::cli_warn("running in parallel is not supported on Windows")
    run_parallel <- 1L
  }
  if (run_parallel > 1) {
    if (length(obj@n_x) < 100) {
      cli::cli_warn("running in parallel is not recommended for small number of focal terms")
    }
  }

  # TODO: supported run in parallel with future.apply or any others??

  if (!is.null(group_by)) {
    assert_not_overlap(group_by, obj@x,
      msg = "{.arg group_by} variables should not overlap with modeling (focal) variables"
    )
    assert_not_overlap(group_by, obj@x2,
      msg = "{.arg group_by} variables should not overlap with modeling (control) variables"
    )
  }

  ms <- br_get_models(obj)
  config <- br_get_config(obj)
  dots <- rlang::list2(...)

  # For logistic, and Cox-PH regressions models, `exponentiate` is typically set to `TRUE`.
  exponentiate <- FALSE
  if (!"exponentiate" %in% names(dots)) {
    if (config$method %in% br_avail_methods_use_exp()) {
      dots[["exponentiate"]] <- TRUE
      cli_inform("set {.code exponentiate=TRUE} for model(s) constructed from {.field {config$method}} method at default")
    } else {
      dots[["exponentiate"]] <- FALSE
    }
    exponentiate <- dots[["exponentiate"]]
  }

  if (is.null(group_by)) {
    res <- runner(ms, obj@data, dots, obj@x, run_parallel)
  } else {
    obj@group_by <- group_by
    data_split <- obj@data |>
      named_group_split(obj@data[, group_by, drop = FALSE])
    res_list <- map(data_split, function(data) {
      runner(ms, data, dots, obj@x, run_parallel)
    })
    res <- list_transpose(res_list)
    res$models <- purrr::list_flatten(res$models)
    res$results <- vctrs::vec_rbind(!!!res$results, .names_to = "Group_variable")
    res$results_tidy <- vctrs::vec_rbind(!!!res$results_tidy, .names_to = "Group_variable")
  }

  obj@models <- res$models
  obj@results <- res$results
  obj@results_tidy <- res$results_tidy
  attr(obj, "exponentiate") <- exponentiate
  obj
}


runner <- function(ms, data, dots, x, run_parallel) {
  f <- function(m, data, dots) {
    # m: model template
    # data: data frame for modeling
    # dots: arguments passing to parse model parameters
    # x: focal variables
    model <- rlang::eval_bare(rlang::parse_expr(m))

    # NOTE:
    # broom.helpers::model_* funs
    # when weights were assigned to observations
    # the number of observations will be multiplied
    # see: https://github.com/larmarange/broom.helpers/blob/210cc945bd6a462148a358f8d4851e0d16d208e3/R/model_get_n.R#L96

    # Get comprehensive result for models
    result <- do.call(
      broom.helpers::tidy_plus_plus,
      args = vctrs::vec_c(list(model), dots)
    )

    # Get tidy result for models
    # output conf.int while exponentiate depends on dots
    result_tidy <- do.call(
      broom::tidy,
      args = vctrs::vec_c(
        list(model), list(conf.int = TRUE),
        if ("exponentiate" %in% names(dots)) {
          list(exponentiate = dots[["exponentiate"]])
        } else {
          list()
        },
        if ("conf.level" %in% names(dots)) {
          list(conf.level = dots[["conf.level"]])
        } else {
          list()
        }
      )
    )

    if (!("intercept" %in% names(dots) && isTRUE(dots[["intercept"]]))) {
      result_tidy <- result_tidy |> dplyr::filter(!.data$term %in% "(Intercept)")
    }

    list(model = model, result = result, result_tidy = result_tidy)
  }

  # names(ms) <- x
  if (run_parallel > 1) {
    res <- parallel::mclapply(ms, f, data = data, dots = dots, mc.cores = run_parallel)
  } else {
    res <- map(ms, f, data = data, dots = dots)
  }

  res <- list_transpose(res)
  models <- res$model
  results <- vctrs::vec_rbind(!!!res$result, .names_to = "Focal_variable")
  results_tidy <- vctrs::vec_rbind(!!!res$result_tidy, .names_to = "Focal_variable")

  list(
    models = models,
    results = results,
    results_tidy = results_tidy
  )
}
