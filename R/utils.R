# x <- tibble::tibble(
#   x = 1, x = 2, `a1:` = 3, `_x_y}` = 4,
#   .name_repair = "universal"
# )
# vctrs:::make_syntactic("a 1")
#
# stop_not_found <- function(path) {
#   abort(
#     .subclass = "fs_error_not_found",
#     path = path
#   )
# }

# Chunk
# @description
# `r lifecycle::badge('superseded')`
# `top_n()` has been superseded in favour of ...


assert_breg_obj <- function(obj) {
  if (!rlang::inherits_any(obj, "breg")) {
    cli_abort("bad input for argument {.arg obj}, a object of class {.cls breg} is required")
  } else {
    obj
  }
}
