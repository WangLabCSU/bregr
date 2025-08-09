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

get_necessary_columns <- function(y, x, x2, group_by, available_cols) {
  # Get all variable names from y, x, x2, and group_by
  necessary_vars <- merge_vars(y, x, x2, group_by)

  # Filter to only include columns that actually exist in the data
  necessary_cols <- intersect(necessary_vars, available_cols)

  # Always include .row_names if it exists (added by tibble constructor)
  if (".row_names" %in% available_cols) {
    necessary_cols <- union(necessary_cols, ".row_names")
  }

  return(necessary_cols)
}

merge_vars <- function(...) {
  vars_list <- list(...) |> unlist()
  rv <- NULL
  for (i in vars_list) {
    v <- unique(sapply(i, get_vars))
    if (length(v) > 0) rv <- union(rv, v)
  }
  rv
}

# x: terms to repair
# y: reference column names
#
# for 3 types of cases:
# 1: valid column names: keep
# 2: invalid column names: transform
# 3: other cases (treat as formula term): keep
# repair_names(c("abc", "?|100", "abc * d"), c("abc", "?|100", "d"))
repair_names <- function(x, y) {
  if_else(
    (x %in% y) & !(make.names(x) == x | (startsWith(x, "`") & endsWith(x, "`"))),
    paste0("`", x, "`"),
    x
  )
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
    TRUE
  }
}

assert_breg_obj_with_results <- function(obj) {
  assert_breg_obj(obj)
  y <- obj@results_tidy
  if (!is.data.frame(y) || nrow(y) == 0) {
    cli_abort("bad input for argument {.arg obj}, a object of class {.cls breg} with results (obtain from {.code br_run()}) is required")
  } else {
    TRUE
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

# Variable filtering functions for br_run pre-filtering
filter_variables_x <- function(data, x, filter_na_prop = 0.8, filter_sd_min = 1e-6, filter_var_min = 1e-6) {
  if (length(x) == 0) {
    return(list(
      filtered_x = character(0),
      filtered_out = character(0),
      filter_summary = list(total = 0, kept = 0, filtered = 0, prop_filtered = 0)
    ))
  }
  
  # Get variable names from terms (handle complex terms like I(x^2))
  x_vars <- sapply(x, get_vars)
  x_simple <- x_vars[sapply(x_vars, function(v) length(v) == 1)]
  
  # For complex terms with multiple variables, we keep them for now
  # Only filter simple single-variable terms
  complex_terms <- x[sapply(x_vars, function(v) length(v) != 1)]
  simple_terms <- x[sapply(x_vars, function(v) length(v) == 1)]
  simple_vars <- x_simple
  
  if (length(simple_vars) == 0) {
    return(list(
      filtered_x = x,
      filtered_out = character(0),
      filter_summary = list(total = length(x), kept = length(x), filtered = 0, prop_filtered = 0)
    ))
  }
  
  # Check which variables are available in data
  available_vars <- intersect(simple_vars, colnames(data))
  if (length(available_vars) == 0) {
    return(list(
      filtered_x = x,
      filtered_out = character(0),
      filter_summary = list(total = length(x), kept = length(x), filtered = 0, prop_filtered = 0)
    ))
  }
  
  # Filter based on criteria
  filtered_out_vars <- character(0)
  
  for (var in available_vars) {
    var_data <- data[[var]]
    
    # Skip if not numeric
    if (!is.numeric(var_data)) {
      next
    }
    
    # Check NA proportion
    na_prop <- sum(is.na(var_data)) / length(var_data)
    if (na_prop > filter_na_prop) {
      filtered_out_vars <- c(filtered_out_vars, var)
      next
    }
    
    # Get non-NA values for variance/sd checks
    non_na_values <- var_data[!is.na(var_data)]
    if (length(non_na_values) < 2) {
      filtered_out_vars <- c(filtered_out_vars, var)
      next
    }
    
    # Check standard deviation
    var_sd <- sd(non_na_values, na.rm = TRUE)
    if (is.na(var_sd) || var_sd < filter_sd_min) {
      filtered_out_vars <- c(filtered_out_vars, var)
      next
    }
    
    # Check variance
    var_var <- var(non_na_values, na.rm = TRUE)
    if (is.na(var_var) || var_var < filter_var_min) {
      filtered_out_vars <- c(filtered_out_vars, var)
      next
    }
  }
  
  # Map filtered variables back to terms
  filtered_out_terms <- simple_terms[simple_vars %in% filtered_out_vars]
  kept_terms <- setdiff(x, filtered_out_terms)
  
  # Create summary
  filter_summary <- list(
    total = length(x),
    kept = length(kept_terms),
    filtered = length(filtered_out_terms),
    prop_filtered = length(filtered_out_terms) / length(x)
  )
  
  list(
    filtered_x = kept_terms,
    filtered_out = filtered_out_terms,
    filter_summary = filter_summary
  )
}
