#' Package availability
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Package resource, definitions ready for use.
#'
#' @name avails
#' @returns A character vector representing the available methods or options.
#' @seealso [pipeline] for building `breg` objects.
NULL

#' @describeIn avails Returns available modeling methods. This correlates
#' to [br_set_model()].
#' @export
br_avail_methods <- function() {
  # TODO: test examples one by one
  c(
    # lm
    "lm",
    # survival
    "coxph", "survreg", "clogit", "cch",
    # glm
    "binomial", "gaussian",
    "Gamma", "inverse.gaussian",
    "poisson", "quasi", "quasibinomial",
    "quasipoisson",
    # other
    "nls", "aov"
  )
}

#' @describeIn avails Returns available modeling methods which
#' set `exponentiate=TRUE` at default by **bregr**.
#' @export
br_avail_methods_use_exp <- function() {
  # This is typical for logistic, Poisson and Cox models,
  # but a bad idea if there is no log or logit link
  c(
    "coxph", "binomial", "quasibinomial",
    "poisson", "quasipoisson"
  )
}

#' @describeIn avails Returns model configs for specified method to
#' generate modeling templates.
#' @param method Method for model construction. See [br_avail_methods()]
#' for available options.
#' @export
br_avail_method_config <- function(method) {
  assert_string(method, allow_empty = FALSE)
  if (method %in% c("lm", "nls", "aov")) {
    list(
      f_call = glue::glue("stats::{method}"),
      f_cnst_y = NULL,
      args_method = NULL,
      args_data = glue::glue("data = data")
    )
  } else if (method %in% "clogit") {
    list(
      f_call = glue::glue("survival::clogit"),
      f_cnst_y = NULL,
      args_method = NULL,
      args_data = glue::glue("data = data")
    )
  } else if (method %in% c("coxph", "survreg", "cch")) {
    list(
      f_call = glue::glue("survival::{method}"),
      f_cnst_y = function(y) {
        glue::glue("survival::Surv({paste(y, collapse = ', ')})")
      },
      args_method = NULL,
      args_data = glue::glue("data = data")
    )
  } else if (method %in% c(
    "binomial", "gaussian",
    "Gamma", "inverse.gaussian",
    "poisson", "quasi", "quasibinomial",
    "quasipoisson"
  )) {
    list(
      f_call = glue::glue("stats::glm"),
      f_cnst_y = NULL,
      args_method = glue::glue("family = stats::{method}"),
      args_data = glue::glue("data = data")
    )
  } else {
    cli::cli_warn("nonstandard {.arg method} passed to {.fn stats::glm}, double-check if it's correct")
    list(
      f_call = glue::glue("stats::glm"),
      f_cnst_y = NULL,
      args_method = glue::glue("family = stats::{method}"),
      args_data = glue::glue("data = data")
    )
  }
}
