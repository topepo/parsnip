% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment.R
\name{augment.model_fit}
\alias{augment.model_fit}
\title{Augment data with predictions}
\usage{
\method{augment}{model_fit}(x, new_data, ...)
}
\arguments{
\item{x}{A \code{model_fit} object produced by \code{\link[=fit.model_spec]{fit.model_spec()}} or
\code{\link[=fit_xy.model_spec]{fit_xy.model_spec()}} .}

\item{new_data}{A data frame or matrix.}

\item{...}{Not currently used.}
}
\description{
\code{augment()} will add column(s) for predictions to the given data.
}
\details{
For regression models, a \code{.pred} column is added. If \code{x} was created using
\code{\link[=fit.model_spec]{fit.model_spec()}} and \code{new_data} contains the outcome column, a \code{.resid} column is
also added.

For classification models, the results can include a column called
\code{.pred_class} as well as class probability columns named \verb{.pred_\{level\}}.
This depends on what type of prediction types are available for the model.
}
\examples{
car_trn <- mtcars[11:32,]
car_tst <- mtcars[ 1:10,]

reg_form <-
  linear_reg() \%>\%
  set_engine("lm") \%>\%
  fit(mpg ~ ., data = car_trn)
reg_xy <-
  linear_reg() \%>\%
  set_engine("lm") \%>\%
  fit_xy(car_trn[, -1], car_trn$mpg)

augment(reg_form, car_tst)
augment(reg_form, car_tst[, -1])

augment(reg_xy, car_tst)
augment(reg_xy, car_tst[, -1])

# ------------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")
cls_trn <- two_class_dat[-(1:10), ]
cls_tst <- two_class_dat[  1:10 , ]

cls_form <-
  logistic_reg() \%>\%
  set_engine("glm") \%>\%
  fit(Class ~ ., data = cls_trn)
cls_xy <-
  logistic_reg() \%>\%
  set_engine("glm") \%>\%
  fit_xy(cls_trn[, -3],
  cls_trn$Class)

augment(cls_form, cls_tst)
augment(cls_form, cls_tst[, -3])

augment(cls_xy, cls_tst)
augment(cls_xy, cls_tst[, -3])

}
