% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficiencyModels.R
\name{BBC_in}
\alias{BBC_in}
\title{Linear programming model for radial input measure}
\usage{
BBC_in(
  data,
  x,
  y,
  dataOriginal = data,
  xOriginal = x,
  yOriginal = y,
  FDH = FALSE
)
}
\arguments{
\item{data}{\code{data.frame} or \code{matrix} containing the new variables
in the model.}

\item{x}{Vector. Column input indexes in data.}

\item{y}{Vector. Column output indexes in data.}

\item{dataOriginal}{\code{data.frame} or \code{matrix} containing the
original
variables used to create the model.}

\item{xOriginal}{Vector. Column input indexes in original data.}

\item{yOriginal}{Vector. Column output indexes in original data.}

\item{FDH}{Binary decision variables}
}
\value{
\code{matrix} with the the predicted score
}
\description{
This function predicts the expected output through a DEA model.
}
