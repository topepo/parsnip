% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_reg_lm.R
\name{details_linear_reg_lm}
\alias{details_linear_reg_lm}
\title{Linear regression via lm}
\description{
\code{\link[stats:lm]{stats::lm()}} uses ordinary least squares to fit models with numeric outcomes.
}
\details{
For this engine, there is a single mode: regression
\subsection{Tuning Parameters}{

This engine has no tuning parameters.
}

\subsection{Translation from parsnip to the original package}{\if{html}{\out{<div class="r">}}\preformatted{linear_reg() \%>\% 
  set_engine("lm") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## Linear Regression Model Specification (regression)
## 
## Computational engine: lm 
## 
## Model fit template:
## stats::lm(formula = missing_arg(), data = missing_arg(), weights = missing_arg())
}
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
\item Kuhn, M, and K Johnson. 2013. \emph{Applied Predictive Modeling}.
Springer.
}
}
}
\keyword{internal}
