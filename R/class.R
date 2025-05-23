# Class design

#' New breg-class object
#' @param y Character vector representing dependent variables.
#' @param x Character vector representing focal variables.
#' @param x2 Character vector representing control variables, optional.
#' @param group_by Character vector representing group by column, optional.
#' @param data A `data.frame` containing the data for modeling.
#' @param config Configurations for model construction.
#' @param models List of model.
#' @param results A `data.frame` containing the result data of models (from [broom.helpers::tidy_plus_plus()]).
#' @param results_tidy A `data.frame` containing the tidy result data of models (from [broom::tidy()]).
#' @export
#' @import S7
#' @rdname breg
#' @examples
#' obj <- breg("y", letters[1:5], LETTERS[1:5])
#' obj
#' print(obj, raw = TRUE)
#'
breg <- new_class("breg",
  properties = list(
    y = NULL | class_character,
    x = NULL | class_character,
    x2 = NULL | class_character,
    group_by = NULL | class_character,
    data = class_data.frame,
    config = NULL | class_character | class_list,
    models = class_list,
    results = class_data.frame,
    results_tidy = class_data.frame,
    n_x = new_property(
      class_integer,
      getter = function(self) length(self@x)
    ),
    n_x2 = new_property(
      class_integer,
      getter = function(self) length(self@x2)
    )
  ),
  constructor = function(y = NULL, x = NULL, x2 = NULL, group_by = NULL,
                         data = NULL,
                         config = NULL,
                         models = list(),
                         results = NULL,
                         results_tidy = NULL) {
    new_object(
      S7_object(),
      y = y,
      x = x,
      x2 = x2,
      group_by = group_by,
      data = data %||% data.frame(),
      config = config,
      models = models,
      results = results %||% data.frame(),
      results_tidy = results %||% data.frame()
    )
  }
)

#' Print method for breg object
#'
#' Print a breg object.
#' @name print.breg
#' @param x An object of class `breg`.
#' @param ... Additional arguments (currently not used).
#' @param raw Logical, whether to print raw S7 representation. Default is `FALSE`.
#' @return Invisibly returns `x`.
#'
#' @method print breg
method(print, breg) <- function(x, ..., raw = FALSE) {
  if (raw) {
    print(utils::str(x))
  } else {
    cli_text("A object of {.cls breg} class\n")

    cli_ul()
    cli_li("{.field Y}: {.emph {x@y}}")
    cli_li("{.field X}:")
    ulid <- cli_ul()
    # https://cli.r-lib.org/reference/pluralization.html#choosing-the-right-quantity
    cli_li("{col_blue('focal')}{qty(x@n_x)} variable{?s}: {.emph {x@x}}")
    cli_li("{col_blue('control')}{qty(x@n_x2)} variable{?s}: {.emph {x@x2}}")
    cli_end(ulid)
    cli_li("{.field group_by}: {.emph {x@group_by}}")
    cli_li("{.field data}: {.emph {rlang::expr_deparse(x@data)}}")
    cli_li("{.field config}: {.emph {x@config}}")
    cli_li("{.field models}: {.emph {rlang::expr_deparse(x@models)}}")
    cli_li("{.field results}: {.emph {rlang::expr_deparse(x@results)}}")
    cli_end()

    cli_text()
    cli_text(col_grey("Focal variables (or primary variables) are injected into the model one by one, while covariates (or control variables) remain constant across all models in the batch."))
  }

  invisible(x)
}
