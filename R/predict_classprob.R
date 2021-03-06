#' @keywords internal
#' @rdname other_predict
#' @inheritParams predict.model_fit
#' @method predict_classprob model_fit
#' @export predict_classprob.model_fit
#' @export
#' @importFrom tibble as_tibble is_tibble tibble
predict_classprob.model_fit <- function(object, new_data, ...) {
  if (object$spec$mode != "classification")
    rlang::abort("`predict.model_fit()` is for predicting factor outcomes.")

  check_spec_pred_type(object, "prob")


  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  new_data <- prepare_data(object, new_data)

  # preprocess data
  if (!is.null(object$spec$method$pred$prob$pre))
    new_data <- object$spec$method$pred$prob$pre(new_data, object)

  # create prediction call
  pred_call <- make_pred_call(object$spec$method$pred$prob)

  res <- eval_tidy(pred_call)

  # post-process the predictions
  if (!is.null(object$spec$method$pred$prob$post)) {
    res <- object$spec$method$pred$prob$post(res, object)
  }

  # check and sort names
  if (!is.data.frame(res) & !inherits(res, "tbl_spark"))
    rlang::abort("The was a problem with the probability predictions.")

  if (!is_tibble(res) & !inherits(res, "tbl_spark"))
    res <- as_tibble(res)

  res
}

# @export
# @keywords internal
# @rdname other_predict
# @inheritParams predict.model_fit
predict_classprob <- function(object, ...)
  UseMethod("predict_classprob")
