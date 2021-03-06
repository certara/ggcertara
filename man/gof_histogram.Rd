% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gof.R
\name{gof_histogram}
\alias{gof_histogram}
\title{A generic function for histograms}
\usage{
gof_histogram(
  data,
  x,
  bins = 20,
  labels = gof_labels(),
  symm_x = if (isTRUE(log_x)) 1 else 0,
  log_x = F
)
}
\arguments{
\item{data}{A \code{data.frame}.}

\item{x}{A numeric vector, evaulated within \code{data}.}

\item{bins}{Number of bins to use for the histogram.}

\item{labels}{A named \code{list} of labels.}

\item{symm_x}{A number of \code{NULL}. If a number, the x-axis will be symmetric, centered around this number.}

\item{log_x}{If \code{TRUE} then log-scale will be used for the x-axis.}
}
\value{
A \code{ggplot} object.
}
\description{
A generic function for histograms
}
