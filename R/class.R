# Class design

#' New breg-class object
#' @param y Character vector representing dependent variables.
#' @param x Character vector representing focal variables.
#' @param x2 Character vector representing control variables, optional.
#' @param config Configurations for model construction.
#' @param models List of model.
#' @param data Data frame containing the model result data.
#' @export
#' @examples
#' obj = breg("y", "x1", c("x2", "x3"))
#' 
breg <- S7::new_class("breg", properties = list(
  y = S7::class_character,
  x = S7::class_character,
  x2 = S7::class_character,
  config = S7::class_list,
  models = S7::class_list,
  data = S7::class_data.frame,
  n_x = S7::new_property(
    S7::class_integer,
    getter = function(self) length(self@x)
  ),
  n_x2 = S7::new_property(
    S7::class_integer,
    getter = function(self) length(self@x2)
  )
))

#' @name breg
#' @export
#' @param raw Logical, whether to print raw S7 representation. Default is FALSE.
S7::method(print, breg) <- function(x, ..., raw = FALSE) {
  cli_text("A object of {.cls breg} class\n")

  cli_ul()
  cli_li("{.field Y}: {x@y}")
  cli_li("{.field X}:")
  ulid <- cli_ul()
  # https://cli.r-lib.org/reference/pluralization.html#choosing-the-right-quantity
  cli_li("{col_blue('focal')}{qty(x@n_x)} variable{?s}: {x@x}")
  cli_li("{col_blue('control')}{qty(x@n_x2)} variable{?s}: {x@x2}")
  cli_end(ulid)
  cli_li("{.field Model config}:")
  cli_end()

  cli_text()
  cli_text(col_grey("Focal variables (or primary variables) are injected into the model one by one, while covariates (or control variables) remain constant across all models in the batch."))
}