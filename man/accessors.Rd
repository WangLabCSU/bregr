% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-accessors.R
\name{accessors}
\alias{accessors}
\alias{br_get_data}
\alias{br_get_y}
\alias{br_get_x}
\alias{br_get_n_x}
\alias{br_get_x2}
\alias{br_get_n_x2}
\alias{br_get_group_by}
\alias{br_get_config}
\alias{br_get_models}
\alias{br_get_model}
\alias{br_get_results}
\title{Accessor functions for \code{breg} objects}
\usage{
br_get_data(obj)

br_get_y(obj)

br_get_x(obj)

br_get_n_x(obj)

br_get_x2(obj)

br_get_n_x2(obj)

br_get_group_by(obj)

br_get_config(obj)

br_get_models(obj)

br_get_model(obj, idx)

br_get_results(obj, tidy = FALSE, ...)
}
\arguments{
\item{obj}{A \code{breg} object.}

\item{idx}{Index or names (focal variables) of the model(s) to return.}

\item{tidy}{If \code{TRUE} return tidy (compact) results, otherwise return comprehensive results.
The tidy results are obtained from \code{\link[broom:reexports]{broom::tidy()}} while comprehensive results are obtained from
\code{\link[broom.helpers:tidy_plus_plus]{broom.helpers::tidy_plus_plus()}}. The results can be configured when run with \code{\link[=br_run]{br_run()}}.}

\item{...}{Subset operations passing to \code{\link[dplyr:filter]{dplyr::filter()}} to filter results.}
}
\value{
Output depends on the function called:
\itemize{
\item \code{br_get_data()} returns a \code{data.frame}.
\item \code{br_get_y()}, \code{br_get_x()}, \code{br_get_x2()} return modeling terms.
\item \code{br_get_n_x()} and \code{br_get_n_x2()} return the length of terms \code{x} and \code{x2}.
\item \code{br_get_group_by()} returns variable(s) for group analysis.
\item \code{br_get_config()} returns modeling method and extra arguments.
\item \code{br_get_models()} returns all constructed models.
\item \code{br_get_model()} returns a subset of constructed models.
\item \code{br_get_results()} returns modeling result \code{data.frame}.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

These functions provide access to components of \code{breg} objects, serving as counterparts
to the \verb{br_set_*()} functions. Some functions include additional arguments for extended
functionality.
}
\examples{
m <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)
br_get_data(m)
br_get_y(m)
br_get_x(m)
br_get_n_x(m)
br_get_x2(m)
br_get_n_x2(m)
br_get_group_by(m)
br_get_config(m)
br_get_models(m)
br_get_model(m, 1)
br_get_n_x2(m)
br_get_results(m)
br_get_results(m, tidy = TRUE)
br_get_results(m, tidy = TRUE, term == "cyl")

}
\seealso{
\link{pipeline} for building \code{breg} objects.
}
