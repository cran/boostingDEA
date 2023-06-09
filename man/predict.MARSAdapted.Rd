% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Predictions.R
\name{predict.MARSAdapted}
\alias{predict.MARSAdapted}
\title{Model Prediction for Adapted Multivariate Adaptive Frontier Splines.}
\usage{
\method{predict}{MARSAdapted}(object, newdata, x, class = 1, ...)
}
\arguments{
\item{object}{A \code{MARSAdapted} object.}

\item{newdata}{\code{data.frame}. Set of input variables to predict on.}

\item{x}{Inputs index.}

\item{class}{Model for prediction. \code{1} MARS Boost without smoothing
procedure.\code{2} MARS Boost with smoothing procedure..}

\item{...}{further arguments passed to or from other methods.}
}
\value{
\code{data.frame} with the predicted values.
}
\description{
This function predicts the expected output by a \code{MARS}
object.
}
