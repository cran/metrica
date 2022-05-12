% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PLA.R
\name{PLA}
\alias{PLA}
\title{Percentage Lack of Accuracy (PLA)}
\usage{
PLA(data = NULL, obs, pred, na.rm = TRUE)
}
\arguments{
\item{data}{(Optional) argument to call an existing data frame containing the data.}

\item{obs}{Vector with observed values (numeric).}

\item{pred}{Vector with predicted values (numeric).}

\item{na.rm}{Logic argument to remove rows with missing values
(NA). Default is na.rm = TRUE.}
}
\value{
an object of class \code{numeric}.
}
\description{
It estimates the PLA, the contribution of the systematic error to
the Mean Squared Error (MSE) for a continuous predicted-observed dataset
following Correndo et al. (2021).
}
\details{
The PLA (\%, 0-100) represents the  contribution of the Mean Lack of Accuracy (MLA),
the systematic (bias) component of the MSE. It is obtained via a symmetric decomposition of
the MSE (invariant to predicted-observed orientation). The PLA can be further segregated
into percentage additive bias (PAB) and percentage proportional bias (PPB).
The greater the value the greater the contribution of systematic error to the MSE.
For the formula and more details, see \href{https://adriancorrendo.github.io/metrica/articles/available_metrics.html}{online-documentation}
}
\examples{
\donttest{
set.seed(1)
X <- rnorm(n = 100, mean = 0, sd = 10)
Y <- X + rnorm(n=100, mean = 0, sd = 3)
PLA(obs = X, pred = Y)
}
}
\references{
Correndo et al. (2021).
Revisiting linear regression to test agreement in continuous predicted-observed datasets.
\emph{Agric. Syst. 192, 103194.} \doi{10.1016/j.agsy.2021.103194}
}