% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-class.R
\name{breg}
\alias{breg}
\title{Creates a new breg-class object}
\usage{
breg(
  data = NULL,
  y = NULL,
  x = NULL,
  x2 = NULL,
  group_by = NULL,
  config = NULL,
  models = list(),
  results = NULL,
  results_tidy = NULL
)
}
\arguments{
\item{data}{A \code{data.frame} containing modeling data.}

\item{y}{Character vector of dependent variable names.}

\item{x}{Character vector of focal independent variable names.}

\item{x2}{Optional character vector of control variable names.}

\item{group_by}{Optional character vector specifying grouping column.}

\item{config}{List of model configuration parameters.}

\item{models}{List containing fitted model objects.}

\item{results}{A \code{data.frame} of model results from \code{\link[broom.helpers:tidy_plus_plus]{broom.helpers::tidy_plus_plus()}}.}

\item{results_tidy}{A \code{data.frame} of tidy model results from \code{\link[broom:reexports]{broom::tidy()}}.}
}
\value{
A constructed \code{breg} object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Constructs a breg-class object containing regression model specifications and results.
}
\examples{
obj <- breg()
obj
print(obj, raw = TRUE)

}
