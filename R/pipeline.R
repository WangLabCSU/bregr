# TODO: br开头函数，set y，set x，set model，run，输出tidy结果，再对接可视化
# if necessary, use helper function to set parameters

# Create a new breg-class object
new_breg = function() {
  structure(
    list(
      y = list(),
      x = list(),
      config = list(),
      models = list()
    ),
    class = "breg"
  )
}

#' @export
print.breg = function(x, ...) {
  cli_text("A object of {.cls breg} class\n")
  cli_ul()
  cli_li("{.field Y}:")
  cli_li("{.field X}:")
  ulid <- cli_ul()
  cli_li("focal variable{?s}: {letters[1:10]}")
  #cli_li("{col_blue('focal')} term{?s}: {letters[1:10]}")
  cli_li("control variable{?s}: {letters[1:10]}")
  #cli_li("{col_blue('covariable')} term{?s}: {letters[1:10]}")
  cli_end(ulid)
  cli_li("{.field Model config}:")
  cli_end()

  cli_text()
  cli_text(col_grey("Focal variables (or primary variables) are injected into the model one by one, while covariates (or control variables) remain constant across all models in the batch."))
}


br_set_y = function(y) {
  obj = new_breg()
  obj$y = y
  obj
}

br_set_x = function(obj, x) {
  if (!rlang::inherits_any(obj, "breg")) {
    cli_abort("bad input for argument {.arg obj}, a object of class {.cls breg} is required")
  }
  obj$x = x
  obj
}

br_set_model = function(obj, m) {
  if (!rlang::inherits_any(obj, "breg")) {
    cli_abort("bad input for argument {.arg obj}, a object of class {.cls breg} is required")
  }
  obj$m = m
  obj
}

br_run = function(obj, ...) {
  if (!rlang::inherits_any(obj, "breg")) {
    cli_abort("bad input for argument {.arg obj}, a object of class {.cls breg} is required")
  }
  # TODO: run
}

# All-in-one pipeline to run the basic regression analysis in batch
br_pipeline = function() {

}
