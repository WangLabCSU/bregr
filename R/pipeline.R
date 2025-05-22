#' Modeling and analysis pipeline in bregr
#'
#' - `br_set_data()`: Set data for model construction.
#' - `br_set_y()`: Set independent variables for model construction.
#' - `br_set_x()`: Set dependent variables for model construction.
#' - `br_set_model()`: Set model configurations.
#' - `br_run()`: Run the regression analysis in batch.
#'
#' @param y Character vector representing dependent variables.
#' @rdname pipeline
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
#'   br_set_x(c("age * sex"), "pat.karno") |>
#'   br_set_model("coxph") |>
#'   br_run()
#'
#' br_set_data(data) |>
#'   br_set_y(c("time")) |>
#'   br_set_x(c("age", "sex"), "pat.karno") |>
#'   br_set_model('quasi(variance = "mu", link = "log")') |>
#'   br_run()
#'
br_set_data <- function(data) {
  assert_s3_class(data, "data.frame")

  obj <- breg()
  obj@data <- data
  obj
}

#' @param obj An object of class `breg`.
#' @rdname pipeline
#' @export
br_set_y <- function(obj, y) {
  assert_breg_obj(obj)
  assert_character(y)

  obj@y <- y
  obj
}

#' @param x Character vector representing focal variables.
#' @param x2 Character vector representing control variables, optional.
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
#' @param method Method for model construction.
#' @param ... Additional arguments for model construction.
#' @export
br_set_model <- function(obj, method, ...) {
  assert_breg_obj(obj)
  assert_string(method)
  rlang::check_dots_used()

  # if (is_call) {
  #   # e.g., quasi(variance = "mu", link = "log")
  #   f <- eval(parse(text = f))
  # }
  if (!grepl("\\(", method)) {
    method_list <- c(
      "coxph", "binomial", "gaussian",
      "Gamma", "inverse.gaussian",
      "poisson", "quasi", "quasibinomial",
      "quasipoisson"
    )
    rlang::arg_match0(method, method_list)
  }

  config <- list(...)
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
br_run <- function(obj, ...) {
  assert_breg_obj(obj)
  # TODO: other parameters from https://easystats.github.io/parameters/
  # TODO: supported run in parallele with future.apply?

  ms <- obj@models

  result_list <- list()
  for (i in seq_along(ms)) {
    obj@models[[i]] <- local({
      data <- obj@data
      rlang::eval_bare(rlang::parse_expr(ms[[i]]))
    })
    result_list[[i]] <- parameters::model_parameters(
      obj@models[[i]]
    ) # , exponentiate = exp, ci = ci)  #TODO
  }
  names(result_list) <- names(obj@models) <- obj@x
  obj@params <- result_list
  obj@results <- vctrs::vec_rbind(!!!map(result_list, as.data.frame), .names_to = "Focal_variable")
  obj
}

# All-in-one pipeline to run the basic regression analysis in batch
br_pipeline <- function() {

}
