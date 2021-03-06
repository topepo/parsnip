% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svm_poly.R
\name{svm_poly}
\alias{svm_poly}
\title{Polynomial support vector machines}
\usage{
svm_poly(
  mode = "unknown",
  engine = "kernlab",
  cost = NULL,
  degree = NULL,
  scale_factor = NULL,
  margin = NULL
)
}
\arguments{
\item{mode}{A single character string for the prediction outcome mode.
Possible values for this model are "unknown", "regression", or
"classification".}

\item{engine}{A single character string specifying what computational engine
to use for fitting.}

\item{cost}{A positive number for the cost of predicting a sample within
or on the wrong side of the margin}

\item{degree}{A positive number for polynomial degree.}

\item{scale_factor}{A positive number for the polynomial scaling factor.}

\item{margin}{A positive number for the epsilon in the SVM insensitive
loss function (regression only)}
}
\description{
\code{svm_poly()} defines a support vector machine model. For classification,
the model tries to maximize the width of the margin between classes.
For regression, the model optimizes a robust loss function that is only
affected by very large model residuals.

This SVM model uses a nonlinear function, specifically a polynomial function,
to create the decision boundary or regression line.

There are different ways to fit this model. See the engine-specific pages
for more details:

\Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("svm_poly")}

More information on how \pkg{parsnip} is used for modeling is at
\url{https://www.tidymodels.org/}.
}
\details{
This function only defines what \emph{type} of model is being fit. Once an engine
is specified, the \emph{method} to fit the model is also defined.

The model is not trained or fit until the \code{\link[=fit.model_spec]{fit.model_spec()}} function is used
with the data.
}
\examples{
show_engines("svm_poly")

svm_poly(mode = "classification", degree = 1.2)
}
\references{
\url{https://www.tidymodels.org}, \href{https://tmwr.org}{\emph{Tidy Models with R}}
}
\seealso{
\Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("svm_poly")}
}
