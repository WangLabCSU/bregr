#' Modeling and analysis pipeline in bregr
#'
#' - `br_pipeline()`: All-in-one end to end wrapper to run the regression analysis in batch.
#' Which could be splitted into the following steps:
#' - `br_set_data()`: Set data for model construction.
#' - `br_set_y()`: Set independent variables for model construction.
#' - `br_set_x()`: Set focal variables for model construction.
#' - `br_set_x2()`: (Optional) Set control variables for model construction.
#' - `br_set_model()`: Set model configurations.
#' - `br_run()`: Run the regression analysis in batch.
#'
#' @rdname pipeline
#' @param data A `data.frame` containing all necessary variables for analysis.
#' Column names should follow R's naming conventions.
#' @param obj An object of class `breg`.
#' @param y Character vector specifying dependent variables.
#' For GLM models, this is typically a single character (e.g., `"outcome"`).
#' For Cox-PH models, it should be a length-2 vector in the format `c("time", "status")`.
#' @param x Character vector specifying focal variables.
#' @param x2 Character vector specifying control variables (optional).
#' @param method Method for model construction.
#' @param group_by A string specifying the group by column.
#' @param run_parallel Integer, indicating cores to run the task, default is `1`.
#' @param ... Additional arguments depending on the called function.
#' - `br_set_x()` for passing focal variables as characters.
#' - `br_set_x2()` for passing control variables as characters.
#' - `br_set_model()` for passing other configurations for modeling.
#' - `br_run()` for passing other configurations for obtaining modeling results with [tidy_plus_plus()].
#' e.g., The default value for `exponentiate` is `FALSE` (coefficients are not exponentiated).
#' For logistic, multinomial, and Cox-PH regressions models, `exponentiate` is typically set to `TRUE`.
#' @param model_args A list of arguments passed to `br_set_model()`.
#' @param run_args A list of arguments passed to `br_run()`.
#' @export
#' @examples
#' library(bregr)
#' # 1. Pipeline -------------------------
#' # 1.1. A single linear model ----------
#' m <- br_set_data(mtcars) |> # set model data
#'   br_set_y("mpg") |> # set dependent variable
#'   br_set_x("qsec") |> # set focal variables
#'   br_set_model("gaussian") |> # set model
#'   br_run() # run analysis
#'
#' # get model tidy result
#' m@results_tidy
#' # compare with R's built-in function
#' lm(mpg ~ qsec, data = mtcars) |> summary()
#' # 1.2. Batch linear model -------------
#' # control variables are injected in all constructed models
#' # focal variables are injected in constructed models one by one
#' m2 <- br_set_data(mtcars) |>
#'   br_set_y("mpg") |>
#'   br_set_x(colnames(mtcars)[2:4]) |> # set focal variables
#'   br_set_x2("vs") |> # set control variables
#'   br_set_model("gaussian") |>
#'   br_run()
#' # 1.3. Group by model -------------
#' m3 <- br_set_data(mtcars) |>
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
br_pipeline <- function(
    data, y, x, method, x2 = NULL,
    group_by = NULL, run_parallel = 1L,
    model_args = list(),
    run_args = list()) {
  br_set_data(data) |>
    br_set_y(y) |>
    br_set_x(x) |>
    br_set_x2(x2) |>
    br_set_model(method = method, !!!model_args) |>
    br_run(group_by = group_by, run_parallel = run_parallel, !!!run_args)
}

#' @rdname pipeline
#' @export
br_set_data <- function(data) {
  assert_s3_class(data, "data.frame")

  obj <- breg()
  obj@data <- tibble::as_tibble(data, rownames = ".row_names")
  obj
}

#' @rdname pipeline
#' @export
br_set_y <- function(obj, y) {
  assert_breg_obj(obj)
  assert_character(y)

  obj@y <- y
  obj
}

#' @rdname pipeline
#' @export
br_set_x <- function(obj, ...) {
  assert_breg_obj(obj)

  x <- rlang::list2(...) |> unlist()
  assert_character(x, allow_na = FALSE)

  # drop names if necessary
  obj@x <- x |> as.character()
  obj
}

#' @rdname pipeline
#' @export
br_set_x2 <- function(obj, ...) {
  assert_breg_obj(obj)
  assert_character(obj@x)

  x2 <- rlang::list2(...) |> unlist()
  assert_character(x2, allow_na = FALSE, allow_null = TRUE)
  # drop names if necessary
  x2 <- as.character(x2)
  assert_not_overlap(obj@x, x2)

  obj@x2 <- x2
  obj
}


#' @rdname pipeline
#' @export
br_set_model <- function(obj, method, ...) {
  assert_breg_obj(obj)
  assert_string(method, allow_empty = FALSE)

  if (!grepl("\\(", method)) {
    method_list <- c(
      "coxph", "binomial", "gaussian",
      "Gamma", "inverse.gaussian",
      "poisson", "quasi", "quasibinomial",
      "quasipoisson"
    )
    rlang::arg_match0(method, method_list)
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
      msg = "two dependent variables corresponding to 'time' and 'status' are required for Cox proportional hazards model"
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

  obj@config <- config_text
  obj@models <- models
  obj
}

#' @rdname pipeline
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
      cli::cli_warn("running in parallel is not recommended for small number of focal variables")
    }
  }

  # TODO: supported run in parallel with future.apply or any others??

  if (!is.null(group_by)) {
    assert_not_overlap(group_by, obj@x,
      msg = "group_by variables should not overlap with modeling (focal) variables"
    )
    assert_not_overlap(group_by, obj@x2,
      msg = "group_by variables should not overlap with modeling (control) variables"
    )
  }

  ms <- obj@models
  dots <- rlang::list2(...)

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
  obj
}


runner <- function(ms, data, dots, x, run_parallel) {
  f <- function(m, data, dots) {
    # m: model template
    # data: data frame for modeling
    # dots: arguments passing to parse model parameters
    # x: focal variables
    model <- rlang::eval_bare(rlang::parse_expr(m))
    # Get comprehensive result for models
    # broom.helpers::model_* funs
    # when weights were assigned to observations
    # the number of observations will be multiplied
    # see: https://github.com/larmarange/broom.helpers/blob/210cc945bd6a462148a358f8d4851e0d16d208e3/R/model_get_n.R#L96
    result <- do.call(
      broom.helpers::tidy_plus_plus,
      args = vctrs::vec_c(list(model), dots)
    )
    # TODO: Intercept not included in tidy_plus_plus, double check default settings.

    # output conf.int while exponentiate depends on dots
    result_tidy <- do.call(
      broom::tidy,
      args = vctrs::vec_c(
        list(model), list(conf.int = TRUE),
        if ("exponentiate" %in% names(dots)) {
          list(exponentiate = dots[["exponentiate"]])
        } else {
          list()
        }
      )
    )

    list(model = model, result = result, result_tidy = result_tidy)
  }

  names(ms) <- x
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
