% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_fscore.R
\name{fscore}
\alias{fscore}
\title{F-score}
\usage{
fscore(
  data = NULL,
  obs,
  pred,
  B = 1,
  pos_level = 2,
  atom = FALSE,
  tidy = FALSE,
  na.rm = TRUE
)
}
\arguments{
\item{data}{(Optional) argument to call an existing data frame containing the data.}

\item{obs}{Vector with observed values (character | factor).}

\item{pred}{Vector with predicted values (character | factor).}

\item{B}{Numeric value indicating the weight (a.k.a. B or beta) to be applied to the
relationship between recall and precision. B < 1 weights more precision than recall.
B > 1 gives B times more importance to recall than precision. Default: 1.}

\item{pos_level}{Integer, for binary cases, indicating the order (1|2) of the level
corresponding to the positive. Generally, the positive level is the second (2)
since following an alpha-numeric order, the most common pairs are
\code{(Negative | Positive)}, \code{(0 | 1)}, \code{(FALSE | TRUE)}. Default : 2.}

\item{atom}{Logical operator (TRUE/FALSE) to decide if the estimate is made for
each class (atom = TRUE) or at a global level (atom = FALSE); Default : FALSE.
When dataset is "binomial" atom does not apply.}

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
It estimates the F-score for a nominal/categorical predicted-observed dataset.
}
\details{
The F-score (or F-measure) it is a more robust metric than the classic accuracy,
especially when the number of cases for each class is uneven. It represents the harmonic
mean of precision and recall. Thus, to achieve high values of F-score it is necessary
to have both high precision and high recall.

The universal version of F-score employs a coefficient B, by which we can tune the
precision-recall ratio. Values of B > 1 give more weight to recall, and B < 1 give more
weight to precision.

For binomial/binary cases, fscore  =  TP / (TP + 0.5*(FP + FN))

The generalized formula applied to multiclass cases is:

\eqn{fscore = \frac{(1 + B ^ 2) * (precision * recall)} {((B ^ 2 * precision) + recall)} }

It is bounded between 0 and 1.
The closer to 1 the better. Values towards zero indicate low performance.
For the formula and more details, see
\href{https://adriancorrendo.github.io/metrica/articles/available_metrics_classification.html}{online-documentation}
}
\examples{
\donttest{
set.seed(123)
# Two-class
binomial_case <- data.frame(labels = sample(c("True","False"), 100, replace = TRUE), 
predictions = sample(c("True","False"), 100, replace = TRUE))
# Multi-class
multinomial_case <- data.frame(labels = sample(c("Red","Blue", "Green"), 100, replace = TRUE),
predictions = sample(c("Red","Blue", "Green"), 100, replace = TRUE)    )

# Get F-score estimate for two-class case
fscore(data = binomial_case, obs = labels, pred = predictions, tidy = TRUE)

# Get F-score estimate for each class for the multi-class case
fscore(data = multinomial_case, obs = labels, pred = predictions, tidy = TRUE)

# Get F-score estimate for the multi-class case at a global level
fscore(data = multinomial_case, obs = labels, pred = predictions, tidy = TRUE)
}
}
\references{
Goutte, C., Gaussier, E. (2005).
A Probabilistic Interpretation of Precision, Recall and F-Score, with Implication for Evaluation.
\emph{In: D.E. Losada and J.M. Fernandez-Luna (Eds.): ECIR 2005}
\emph{. Advances in Information Retrieval LNCS 3408, pp. 345–359, 2.}
\emph{Springer-Verlag Berlin Heidelberg.} \doi{10.1007/978-3-540-31865-1_25}
}
