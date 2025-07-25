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
  c(
    "coxph", "binomial", "gaussian",
    "Gamma", "inverse.gaussian",
    "poisson", "quasi", "quasibinomial",
    "quasipoisson"
  )
}

#' @describeIn avails Returns available modeling methods which
#' set `exponentiate=TRUE` at default by **bregr**.
#' @export
br_avail_methods_use_exp <- function() {
  c("coxph", "binomial")
}
