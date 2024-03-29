% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reg_var_u.R
\name{var_u}
\alias{var_u}
\title{Uncorrected Variance (var_u)}
\usage{
var_u(data = NULL, x, tidy = FALSE, na.rm = TRUE)
}
\arguments{
\item{data}{(Optional) argument to call an existing data frame containing the data.}

\item{x}{Vector with numeric elements.}

\item{tidy}{Logical operator (TRUE/FALSE) to decide the type of return. TRUE
returns a data.frame, FALSE returns a list; Default : FALSE.}

\item{na.rm}{Logic argument to remove rows with missing values
(NA). Default is na.rm = TRUE.}
}
\value{
an object of class \code{numeric} within a \code{list} (if tidy = FALSE) or within a
\verb{data frame} (if tidy = TRUE).
}
\description{
It estimates the var_u of observed or predicted values.
}
\details{
The var_u is the sample, uncorrected variance. It is calculated as the mean
of sum of squared differences between values of an x and its mean, divided by the sample size (n).
It is uncorrected because it is divided by n, and by not n-1 (traditional variance).
For the formula and more details, see \href{https://adriancorrendo.github.io/metrica/articles/available_metrics_regression.html}{online-documentation}
}
\examples{
\donttest{
set.seed(1)
X <- rnorm(n = 100, mean = 0, sd = 10)
var_u(x = X)
}
}
