% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mars_earth.R
\name{details_mars_earth}
\alias{details_mars_earth}
\title{Multivariate adaptive regression splines (MARS) via earth}
\description{
\code{\link[earth:earth]{earth::earth()}} fits a generalized linear model that uses artificial features for
some predictors. These features resemble hinge functions and the result is
a model that is a segmented regression in small dimensions.
}
\details{
For this engine, there are multiple modes: classification and regression
\subsection{Tuning Parameters}{

This model has 3 tuning parameters:
\itemize{
\item \code{num_terms}: # Model Terms (type: integer, default: see below)
\item \code{prod_degree}: Degree of Interaction (type: integer, default: 1L)
\item \code{prune_method}: Pruning Method (type: character, default:
‘backward’)
}

The default value of \code{num_terms} depends on the number of predictor
columns. For a data frame \code{x}, the default is
\code{min(200, max(20, 2 * ncol(x))) + 1} (see
\code{\link[earth:earth]{earth::earth()}} and the reference below).
}

\subsection{Translation from parsnip to the original package (regression)}{\if{html}{\out{<div class="r">}}\preformatted{mars(num_terms = integer(1), prod_degree = integer(1), prune_method = character(1)) \%>\% 
  set_engine("earth") \%>\% 
  set_mode("regression") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## MARS Model Specification (regression)
## 
## Main Arguments:
##   num_terms = integer(1)
##   prod_degree = integer(1)
##   prune_method = character(1)
## 
## Computational engine: earth 
## 
## Model fit template:
## earth::earth(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     nprune = integer(1), degree = integer(1), pmethod = character(1), 
##     keepxy = TRUE)
}
}

\subsection{Translation from parsnip to the original package (classification)}{\if{html}{\out{<div class="r">}}\preformatted{mars(num_terms = integer(1), prod_degree = integer(1), prune_method = character(1)) \%>\% 
  set_engine("earth") \%>\% 
  set_mode("classification") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## MARS Model Specification (classification)
## 
## Main Arguments:
##   num_terms = integer(1)
##   prod_degree = integer(1)
##   prune_method = character(1)
## 
## Engine-Specific Arguments:
##   glm = list(family = stats::binomial)
## 
## Computational engine: earth 
## 
## Model fit template:
## earth::earth(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     nprune = integer(1), degree = integer(1), pmethod = character(1), 
##     glm = list(family = stats::binomial), keepxy = TRUE)
}

An alternate method for using MARs for categorical outcomes can be found
in \code{\link[discrim:discrim_flexible]{discrim::discrim_flexible()}}.
}

\subsection{Preprocessing requirements}{

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
\code{\link[=fit.model_spec]{fit.model_spec()}}, parsnip will
convert factor columns to indicators.
}

\subsection{References}{
\itemize{
\item Friedman, J. 1991. “Multivariate Adaptive Regression Splines.” \emph{The
Annals of Statistics}, vol. 19, no. 1, pp. 1-67.
\item Milborrow, S. \href{http://www.milbo.org/doc/earth-notes.pdf}{“Notes on the earth package.”}
\item Kuhn, M, and K Johnson. 2013. \emph{Applied Predictive Modeling}.
Springer.
}
}
}
\keyword{internal}
