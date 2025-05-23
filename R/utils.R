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
  if (!rlang::inherits_any(obj, "bregr::breg")) {
    cli_abort("bad input for argument {.arg obj}, a object of class {.cls breg} is required")
  } else {
    obj
  }
}

assert_character_len <- function(x, ..., len = 1, msg = NULL) {
  assert_character(x, ...)
  if (length(x) != len) {
    if (is.null(msg)) {
      cli_abort("bad input for argument {.arg x}, a character vector of length {.val {len}} is required")
    } else {
      cli_abort(msg)
    }
  } else {
    x
  }
}

assert_not_overlap <- function(x, y, msg = NULL) {
  if (any(x %in% y)) {
    if (is.null(msg)) {
      cli_abort("bad input for argument {.arg x}, values in {.arg x} cannot be in {.arg y}")
    } else {
      cli_abort(msg)
    }
  }
}

# https://github.com/tidyverse/dplyr/issues/4223#issuecomment-469269857
named_group_split <- function(.tbl, ..., sep = " / ") {
  grouped <- dplyr::group_by(.tbl, ...)
  names <- rlang::inject(paste(!!!dplyr::group_keys(grouped), sep = sep))

  grouped |>
    dplyr::group_split() |>
    rlang::set_names(names)
}
