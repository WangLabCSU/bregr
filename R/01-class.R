# Class design

#' Creates a new breg-class object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Constructs a breg-class object containing regression model specifications and results.
#'
#' @param data A `data.frame` containing modeling data.
#' @param y Character vector of dependent variable names.
#' @param x Character vector of focal independent variable names.
#' @param x2 Optional character vector of control variable names.
#' @param group_by Optional character vector specifying grouping column.
#' @param config List of model configuration parameters.
#' @param models List containing fitted model objects.
#' @param results A `data.frame` of model results from [broom.helpers::tidy_plus_plus()].
#' @param results_tidy A `data.frame` of tidy model results from [broom::tidy()].
#' @export
#' @returns A constructed `breg` object.
#' @import S7
#' @rdname breg
#' @examples
#' obj <- breg()
#' obj
#' print(obj, raw = TRUE)
#'
breg <- new_class("breg",
  properties = list(
    data = class_data.frame,
    y = NULL | class_character,
    x = NULL | class_character,
    x2 = NULL | class_character,
    group_by = NULL | class_character,
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
  constructor = function(data = NULL,
                         y = NULL, x = NULL, x2 = NULL, group_by = NULL,
                         config = NULL,
                         models = list(),
                         results = NULL,
                         results_tidy = NULL) {
    data <- tibble::as_tibble(data, rownames = ".row_names")
    new_object(
      S7_object(),
      y = y,
      x = x,
      x2 = x2,
      group_by = group_by,
      data = data,
      config = config,
      models = models,
      results = results %||% data.frame(),
      results_tidy = results %||% data.frame()
    )
  }
)

#' Print method for breg object
#' @description
#' `r lifecycle::badge('experimental')`
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
    cli_text("A object of {.cls breg} class with slots:\n")

    qty_x <- qty(x@n_x)
    qty_x2 <- qty(x@n_x2)
    if (qty_x == 0) qty_x <- qty_x + 1
    if (qty_x2 == 0) qty_x2 <- qty_x2 + 1

    # TODO: add variable?
    cli_ul()
    cli_li("{.field y} ({col_blue('response')} variable): {.emph {x@y}}")
    # cli_li("{.field x}:")
    # ulid <- cli_ul()
    # https://cli.r-lib.org/reference/pluralization.html#choosing-the-right-quantity
    cli_li("{.field x} ({col_blue('focal')}{qty_x} term{?s}): {.emph {x@x}}")
    cli_li("{.field x2} ({col_blue('control')}{qty_x2} term{?s}): {.emph {x@x2}}")
    # cli_end(ulid)
    cli_li("{.field group_by}: {.emph {x@group_by}}")
    cli_li("{.field data}: {.emph {rlang::expr_deparse(x@data)}}")
    cli_li("{.field config}: {.emph {rlang::expr_deparse(x@config)}}")
    cli_li("{.field models}: {.emph {rlang::expr_deparse(x@models)}}")
    cli_li("{.field results}: {.emph {rlang::expr_deparse(x@results)}} with colnames {.emph {colnames(x@results)}}")
    cli_li("{.field results_tidy}: {.emph {rlang::expr_deparse(x@results_tidy)}} with colnames {.emph {colnames(x@results_tidy)}}")
    cli_end()

    cli_text()
    cli_text(col_grey("Focal term(s) are injected into the model one by one,"))
    cli_text(col_grey("while control term(s) remain constant across all models in the batch."))
  }

  invisible(x)
}

# gen_template(y = c("time", "status"),
#              x = colnames(survival::lung)[6:10],
#              x2 = c("age", "sex"),
#              f_call = "survival::coxph",
#              f_cnst_y = function(y) {
#                glue::glue("survival::Surv({paste(y, collapse = ', ')})")
#              }, config = "weights = c(1, 2, 3)")


gen_template = function(y, x, x2,
                        f_call, f_cnst_y = NULL,
                        config = NULL) {


  expr_y = "{y}"
  recipe = "{y} ~ {x}"
  model = "{f}({recipe}, data = data)"
  model2 = "{f}({recipe}, data = data, {config})"

  if (is.null(f_cnst_y)) {
    y = glue::glue(expr_y, y = y)
  } else {
    y = glue::glue(expr_y, y = f_cnst_y(y))
  }

  f_cnst_x = function(x1, x2) {
    paste(vctrs::vec_c(x1, x2), collapse = ' + ')
  }

  recp = sapply(x, function(x1, x2) {
    glue::glue(recipe, y = y, x = f_cnst_x(x1, x2))
  }, x2 = x2)


  if (is.null(config)) {
    rv = glue::glue(model, f = f_call, recipe = recp)
  } else {
    rv = glue::glue(model2, f = f_call, recipe = recp, config = config)
  }

  rv
}
