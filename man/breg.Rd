% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-class.R
\name{breg}
\alias{breg}
\title{New breg-class object}
\usage{
breg(
  y = NULL,
  x = NULL,
  x2 = NULL,
  group_by = NULL,
  data = NULL,
  config = NULL,
  models = list(),
  results = NULL,
  results_tidy = NULL
)
}
\arguments{
\item{y}{Character vector representing dependent variables.}

\item{x}{Character vector representing focal variables.}

\item{x2}{Character vector representing control variables, optional.}

\item{group_by}{Character vector representing group by column, optional.}

\item{data}{A \code{data.frame} containing the data for modeling.}

\item{config}{Configurations for model construction.}

\item{models}{List of model.}

\item{results}{A \code{data.frame} containing the result data of models (from \code{\link[broom.helpers:tidy_plus_plus]{broom.helpers::tidy_plus_plus()}}).}

\item{results_tidy}{A \code{data.frame} containing the tidy result data of models (from \code{\link[broom:reexports]{broom::tidy()}}).}
}
\description{
New breg-class object
}
\examples{
obj <- breg("y", letters[1:5], LETTERS[1:5])
obj
print(obj, raw = TRUE)

}
