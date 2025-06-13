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

get_vars <- function(text) {
  if (!is.null(text)) {
    all.vars(parse(text = text))
  } else {
    NULL
  }
}

merge_vars <- function(...) {
  vars_list <- list(...)
  rv <- NULL
  for (i in vars_list) {
    v <- unique(sapply(i, get_vars))
    if (length(v) > 0) rv <- union(rv, v)
  }
  rv
}

remove_backticks <- function(x) {
  gsub("^`|`$|\\\\(?=`)|`(?=:)|(?<=:)`", "", x, perl = TRUE)
}

`%:::%` <- function(pkg, fun, inherits = TRUE) {
  get(fun,
      envir = asNamespace(pkg),
      inherits = inherits
  )
}

remove_missing <- "ggplot2" %:::% "remove_missing"
empty <- "ggplot2" %:::% "empty"

assert_breg_obj <- function(obj) {
  if (!rlang::inherits_any(obj, "bregr::breg")) {
    cli_abort("bad input for argument {.arg obj}, a object of class {.cls breg} is required")
  } else {
    obj
  }
}

assert_breg_obj_with_results <- function(obj) {
  assert_breg_obj(obj)
  y <- obj@results_tidy
  if (!is.data.frame(y) || nrow(y) == 0) {
    cli_abort("bad input for argument {.arg obj}, a object of class {.cls breg} with results (obtain from {.code br_run()}) is required")
  } else {
    obj
  }
}

assert_character_len <- function(x, ..., len = 1, msg = NULL, arg = caller_arg(x)) {
  assert_character(x, ...)
  if (length(x) != len) {
    if (is.null(msg)) {
      cli_abort("bad input for argument {.arg {arg}}, a character vector of length {.val {len}} is required")
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
  if (length(dplyr::group_keys(grouped)) > 1) {
    names <- rlang::inject(paste0("[", paste(!!!dplyr::group_keys(grouped), sep = sep), "]"))
  } else {
    names <- rlang::inject(paste(!!!dplyr::group_keys(grouped), sep = sep))
  }

  grouped |>
    dplyr::group_split() |>
    rlang::set_names(names)
}


# https://stackoverflow.com/questions/66196451/draw-straight-line-between-any-two-point-when-using-coord-polar-in-ggplot2-r
geom_segment_straight <- function(...) {
  layer <- geom_segment(...)
  new_layer <- ggproto(NULL, layer)
  old_geom <- new_layer$geom
  geom <- ggproto(
    NULL, old_geom,
    draw_panel = function(data, panel_params, coord,
                          arrow = NULL, arrow.fill = NULL,
                          lineend = "butt", linejoin = "round",
                          na.rm = FALSE) {
      data <- remove_missing(
        data,
        na.rm = na.rm, c(
          "x", "y", "xend", "yend",
          "linetype", "linewidth", "size", "shape"
        )
      )
      if (empty(data)) {
        return(zeroGrob())
      }
      coords <- coord$transform(data, panel_params)
      # xend and yend need to be transformed separately, as coord doesn't understand
      ends <- transform(data, x = xend, y = yend)
      ends <- coord$transform(ends, panel_params)

      arrow.fill <- if (!is.null(arrow.fill)) arrow.fill else coords$colour
      return(grid::segmentsGrob(
        coords$x, coords$y, ends$x, ends$y,
        default.units = "native", gp = grid::gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(arrow.fill, coords$alpha),
          lwd = (coords$linewidth %||% coords$size) * .pt,
          lty = coords$linetype,
          lineend = lineend,
          linejoin = linejoin
        ),
        arrow = arrow
      ))
    }
  )
  new_layer$geom <- geom
  return(new_layer)
}

utils::globalVariables(
  c("xend", "yend")
)
