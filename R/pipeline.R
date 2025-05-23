#' Modeling and analysis pipeline in bregr
#'
#' - `br_pipeline()`: All-in-one end to end pipeline to run the basic regression analysis in batch.
#' Which could be splitted into the following steps:
#' - `br_set_data()`: Set data for model construction.
#' - `br_set_y()`: Set independent variables for model construction.
#' - `br_set_x()`: Set dependent variables for model construction.
#' - `br_set_model()`: Set model configurations.
#' - `br_run()`: Run the regression analysis in batch.
#'
#' @rdname pipeline
#' @param data A `data.frame` containing all necessary variables for analysis.
#' Column names should follow R's naming conventions.
#' @param obj An object of class `breg`.
#' @param y Character vector specifying dependent variables.
#' @param x Character vector specifying focal variables.
#' @param x2 Character vector specifying control variables (optional).
#' @param method Method for model construction.
#' @param group_by A string specifying the group by column.
#' @param run_parallel Integer, indicating cores to run the task, default is `1`.
#' @param ... Additional arguments for model construction in `br_set_model()`
#' or for [parameters::model_parameters()] in `br_run()`.
#' Note: The default value for `exponentiate` is `FALSE` (coefficients are not exponentiated).
#' For coxph models, this is typically set to `TRUE`.
#' @param model_args A list of arguments passed to `br_set_model()`.
#' @param run_args A list of arguments passed to `br_run()`.
#' @export
#' @examples
#' data <- survival::lung
#' br_set_data(data) |>
#'   br_set_y(c("time", "status")) |>
#'   br_set_x(c("age", "sex"), "ph.ecog") |>
#'   br_set_model("coxph") |>
#'   br_run()
#'
#' br_set_data(data) |>
#'   br_set_y(c("time", "status")) |>
#'   br_set_x(c("age", "sex"), "ph.ecog") |>
#'   br_set_model("coxph") |>
#'   br_run()
#'
#' br_set_data(data) |>
#'   br_set_y(c("time", "status")) |>
#'   br_set_x(c("age * sex"), "pat.karno") |>
#'   br_set_model("coxph") |>
#'   br_run(exponentiate = TRUE, ci = 0.90)
#'
#' br_set_data(data) |>
#'   br_set_y(c("time")) |>
#'   br_set_x(c("age", "sex"), "pat.karno") |>
#'   br_set_model('quasi(variance = "mu", link = "log")') |>
#'   br_run()
#'
br_pipeline <- function(
    data, y, x, method, x2 = NULL,
    group_by = NULL, run_parallel = 1L,
    model_args = list(),
    run_args = list()) {
  br_set_data(data) |>
    br_set_y(y) |>
    br_set_x(x, x2) |>
    br_set_model(method = method, !!!model_args) |>
    br_run(group_by = group_by, run_parallel = run_parallel, !!!run_args)
}

#' @rdname pipeline
#' @export
br_set_data <- function(data) {
  assert_s3_class(data, "data.frame")

  obj <- breg()
  obj@data <- data
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
br_set_x <- function(obj, x, x2 = NULL) {
  # all.vars(quote(x * y))
  # lobster::ast()

  assert_breg_obj(obj)
  assert_character(x)
  assert_character(x2, allow_null = TRUE)
  assert_not_overlap(x, x2)

  obj@x <- x
  obj@x2 <- x2
  obj
}

#' @rdname pipeline
#' @export
br_set_model <- function(obj, method, ...) {
  assert_breg_obj(obj)
  assert_string(method, allow_empty = FALSE)
  # rlang::check_dots_used()

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
  # if (identical(config, list(NULL))) {
  #   config <- list()
  # }
  # TODO: config text directly from ... by deparse?substitute?
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

  obj@config <- config
  obj@models <- models
  obj
}


# TODO: support group by
# support group by, i.e. convert groups into focal variables
# if necessary, use helper function to set parameters

#' @rdname pipeline
#' @export
br_run <- function(obj, ..., group_by = NULL, run_parallel = 1L) {
  assert_breg_obj(obj)
  assert_string(group_by, allow_null = TRUE, allow_empty = FALSE)
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

  # rlang::check_dots_used()
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

  runner <- function(ms, data, dots, x, run_parallel) {
    f <- function(m, data, dots) {
      # m: model template
      # data: data frame for modeling
      # dots: arguments passing to model_parameters
      # x: focal variables
      model <- rlang::eval_bare(rlang::parse_expr(m))
      param <- do.call(
        parameters::model_parameters,
        args = vctrs::vec_c(list(model), dots)
      )
      list(model = model, param = param)
    }

    if (run_parallel > 1) {
      res <- parallel::mclapply(ms, f, data = data, dots = dots, mc.cores = run_parallel)
    } else {
      res <- map(ms, f, data = data, dots = dots)
    }
    models <- map(res, function(x) x$model)
    params <- map(res, function(x) x$param)
    names(params) <- names(models) <- x

    # TODO: Get comprehensive data for models
    # broom.helpers::model_get_model_frame(obj@models[[1]])
    results <- vctrs::vec_rbind(!!!map(params, as.data.frame), .names_to = "Focal_variable")
    # self$model, as.data.frame(self$result), private$model_data
    #vars <- sapply(self$forest_data$term_label, get_vars)
    #self$forest_data <- self$forest_data[order(match(vars, self$terms), decreasing = FALSE)]
    # https://github.com/WangLabCSU/regverse/blob/523dd97137d7a468f8eb60e4a9ef026fb55936a7/R/REGModel.R#L318
    

    list(
      models = models,
      params = params,
      results = results
    )
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
    res$models = purrr::list_flatten(res$models)
    res$params = purrr::list_flatten(res$params)
    res$results = vctrs::vec_rbind(!!!res$results, .names_to = "Group_variable")
  }

  # result_list <- list()
  # for (i in seq_along(ms)) {
  #   obj@models[[i]] <- local({
  #     data <- obj@data
  #     rlang::eval_bare(rlang::parse_expr(ms[[i]]))
  #   })
  #   result_list[[i]] <- do.call(
  #     parameters::model_parameters,
  #     args = vctrs::vec_c(list(obj@models[[i]]), dots)
  #   )
  # }
  # names(result_list) <- names(obj@models) <- obj@x
  # obj@params <- result_list
  # obj@results <- vctrs::vec_rbind(!!!map(result_list, as.data.frame), .names_to = "Focal_variable")

  obj@models <- res$models
  obj@params <- res$params
  obj@results <- res$results
  obj
}
