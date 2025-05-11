# TODO: br开头函数，set y，set x，set model，run，输出tidy结果，再对接可视化
# TODO: support group by, i.e. convert groups into focal variables
#
# if necessary, use helper function to set parameters

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
  assert_character(y)
  assert_breg_obj(obj)

  obj@y <- y
  obj
}

#' @param x Character vector representing focal variables.
#' @param x2 Character vector representing control variables, optional.
#' @rdname pipeline
#' @export
br_set_x <- function(obj, x, x2 = NULL) {
  assert_character(x)
  assert_character(x2, allow_null = TRUE)
  assert_breg_obj(obj)

  obj@x <- x
  obj@x2 <- x2
  obj
}

#' @rdname pipeline
#' @param method Method for model construction.
#' @param ... Additional arguments for model construction.
#' @export
br_set_model <- function(obj, method, ...) {
  assert_string(method)
  assert_breg_obj(obj)

  method_list = c(
    "coxph", "binomial", "gaussian",
    "Gamma", "inverse.gaussian",
    "poisson", "quasi", "quasibinomial",
    "quasipoisson"
  )
  rlang::arg_match0(method, method_list)
  # TODO: support ...
  rlang::check_dots_used()

  obj@config <- method
  obj
}

#' @rdname pipeline
#' @export
br_run <- function(obj, ...) {
  assert_breg_obj(obj)

  # TODO: https://github.com/WangLabCSU/regverse/blob/523dd97137d7a468f8eb60e4a9ef026fb55936a7/R/REGModel.R#L130
}

# All-in-one pipeline to run the basic regression analysis in batch
br_pipeline <- function() {

}
