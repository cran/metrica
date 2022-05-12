% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tiles_plot.R
\name{tiles_plot}
\alias{tiles_plot}
\title{Tiles plot of predicted and observed values}
\usage{
tiles_plot(
  data = NULL,
  obs,
  pred,
  bins = 10,
  orientation = "PO",
  print_metrics = FALSE,
  metrics_list = NULL,
  na.rm = TRUE
)
}
\arguments{
\item{data}{(Optional) argument to call an existing data frame containing the data.}

\item{obs}{Vector with observed values (numeric).}

\item{pred}{Vector with predicted values (numeric).}

\item{bins}{Argument of class numeric specifying the number of bins to create the tiles.}

\item{orientation}{Argument of class string specifying the axis
orientation, PO for predicted vs observed, and OP for
observed vs predicted. Default is orientation = "PO".}

\item{print_metrics}{boolean TRUE/FALSE to embed metrics in the plot. Default is FALSE.}

\item{metrics_list}{vector or list of selected metrics to print on the plot.}

\item{na.rm}{Logic argument to remove rows with missing values
(NA). Default is na.rm = TRUE.}
}
\value{
Object of class \code{ggplot}.
}
\description{
It draws a tiles plot of predictions and observations with alternative
axis orientation (P vs. O; O vs. P).
}
\details{
It creates a tiles plot of predicted vs. observed values. The plot also includes
the 1:1 line (solid line) and the linear regression line (dashed line). By default,
it places the observed on the x-axis and the predicted on the y-axis (orientation = "PO").
This can be inverted by changing the argument orientation = “OP".
For more details, see \href{https://adriancorrendo.github.io/metrica/articles/vignette1.html}{online-documentation}
}
\examples{
\donttest{
X <- rnorm(n = 100, mean = 0, sd = 10)
Y <- rnorm(n = 100, mean = 0, sd = 10)
tiles_plot(obs = X, pred = Y)
}
}
\seealso{
\code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{aes}}
}