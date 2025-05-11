# TODO: br开头函数，set y，set x，set model，run，输出tidy结果，再对接可视化
# TODO: support group by, i.e. convert groups into focal variables
#
# if necessary, use helper function to set parameters

#' Set independent variables for model construction
#' @param y Character vector representing dependent variables.
#' @export
br_set_y <- function(y) {
  stopifnot(is.character(y))

  obj <- breg()
  obj@y <- y
  obj
}

#' Set independent variables for model construction
#' @param obj An object of class `breg`.
#' @param x Character vector representing focal variables.
#' @param x2 Character vector representing control variables, optional.
#' @export
br_set_x <- function(obj, x, x2 = NULL) {
  stopifnot(is.character(x) | is.character(x2))
  assert_breg_obj(obj)

  obj@x <- x
  obj@x2 <- x2
  obj
}

br_set_model <- function(obj, config) {
  stopifnot(is.character(config) | is.list(config))
  assert_breg_obj(obj)

  obj@config <- config
  obj
}

br_run <- function(obj, ...) {
  assert_breg_obj(obj)

  # TODO: run
}

# All-in-one pipeline to run the basic regression analysis in batch
br_pipeline <- function() {

}
