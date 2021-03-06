#' Model predictions
#'
#' Apply a model to create different types of predictions.
#'  `predict()` can be used for all types of models and uses the
#'  "type" argument for more specificity.
#'
#' @param object An object of class `model_fit`
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'   are "numeric", "class", "prob", "conf_int", "pred_int", "quantile", "time",
#'  "hazard", "survival", or "raw". When `NULL`, `predict()` will choose an
#'  appropriate value based on the model's mode.
#' @param opts A list of optional arguments to the underlying
#'  predict function that will be used when `type = "raw"`. The
#'  list should not include options for the model object or the
#'  new data being predicted.
#' @param ... Arguments to the underlying model's prediction
#'  function cannot be passed here (see `opts`). There are some
#'  `parsnip` related options that can be passed, depending on the
#'  value of `type`. Possible arguments are:
#'  \itemize{
#'     \item `level`: for `type`s of "conf_int" and "pred_int" this
#'            is the parameter for the tail area of the intervals
#'            (e.g. confidence level for confidence intervals).
#'            Default value is 0.95.
#'     \item `std_error`: add the standard error of fit or prediction (on
#'            the scale of the linear predictors) for `type`s of "conf_int"
#'            and "pred_int". Default value is `FALSE`.
#'     \item `quantile`: the quantile(s) for quantile regression
#'            (not implemented yet)
#'     \item `time`: the time(s) for hazard and survival probability estimates.
#'  }
#' @details If "type" is not supplied to `predict()`, then a choice
#'  is made:
#'
#'   * `type = "numeric"` for regression models,
#'   * `type = "class"` for classification, and
#'   * `type = "time"` for censored regression.
#'
#' `predict()` is designed to provide a tidy result (see "Value"
#'  section below) in a tibble output format.
#'
#'  ## Interval predictions
#'
#'  When using `type = "conf_int"` and `type = "pred_int"`, the options
#'   `level` and `std_error` can be used. The latter is a logical for an
#'   extra column of standard error values (if available).
#'
#'  ## Censored regression predictions
#'
#' For censored regression, a numeric vector for `time` is required when
#' survival or hazard probabilities are requested. Also, when
#' `type = "linear_pred"`, censored regression models will be formatted such
#' that the linear predictor _increases_ with time. This may have the opposite
#' sign as what the underlying model's `predict()` method produces.
#'
#' @return With the exception of `type = "raw"`, the results of
#'  `predict.model_fit()` will be a tibble as many rows in the output
#'  as there are rows in `new_data` and the column names will be
#'  predictable.
#'
#' For numeric results with a single outcome, the tibble will have
#'  a `.pred` column and `.pred_Yname` for multivariate results.
#'
#' For hard class predictions, the column is named `.pred_class`
#'  and, when `type = "prob"`, the columns are `.pred_classlevel`.
#'
#' `type = "conf_int"` and `type = "pred_int"` return tibbles with
#'  columns `.pred_lower` and `.pred_upper` with an attribute for
#'  the confidence level. In the case where intervals can be
#'  produces for class probabilities (or other non-scalar outputs),
#'  the columns will be named `.pred_lower_classlevel` and so on.
#'
#' Quantile predictions return a tibble with a column `.pred`, which is
#'  a list-column. Each list element contains a tibble with columns
#'  `.pred` and `.quantile` (and perhaps other columns).
#'
#' Using `type = "raw"` with `predict.model_fit()` will return
#'  the unadulterated results of the prediction function.
#'
#' For censored regression:
#'
#'  * `type = "time"` produces a column `.pred_time`.
#'  * `type = "hazard"` results in a column `.pred_hazard`.
#'  * `type = "survival"` results in a column `.pred_survival`.
#'
#'  For the last two types, the results are a nested tibble with an overall
#'  column called `.pred` with sub-tibbles with the above format.
#'
#' In the case of Spark-based models, since table columns cannot
#'  contain dots, the same convention is used except 1) no dots
#'  appear in names and 2) vectors are never returned but
#'  type-specific prediction functions.
#'
#' When the model fit failed and the error was captured, the
#'  `predict()` function will return the same structure as above but
#'  filled with missing values. This does not currently work for
#'  multivariate models.
#' @examples
#' library(dplyr)
#'
#' lm_model <-
#'   linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ ., data = mtcars %>% dplyr::slice(11:32))
#'
#' pred_cars <-
#'   mtcars %>%
#'   dplyr::slice(1:10) %>%
#'   dplyr::select(-mpg)
#'
#' predict(lm_model, pred_cars)
#'
#' predict(
#'   lm_model,
#'   pred_cars,
#'   type = "conf_int",
#'   level = 0.90
#' )
#'
#' predict(
#'   lm_model,
#'   pred_cars,
#'   type = "raw",
#'   opts = list(type = "terms")
#' )
#' @importFrom stats predict
#' @method predict model_fit
#' @export predict.model_fit
#' @export
predict.model_fit <- function(object, new_data, type = NULL, opts = list(), ...) {
  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  check_installs(object$spec)
  load_libs(object$spec, quiet = TRUE)

  type <- check_pred_type(object, type)
  if (type != "raw" && length(opts) > 0) {
    rlang::warn("`opts` is only used with `type = 'raw'` and was ignored.")
  }
  check_pred_type_dots(type, ...)

  res <- switch(
    type,
    numeric     = predict_numeric(object = object, new_data = new_data, ...),
    class       = predict_class(object = object, new_data = new_data, ...),
    prob        = predict_classprob(object = object, new_data = new_data, ...),
    conf_int    = predict_confint(object = object, new_data = new_data, ...),
    pred_int    = predict_predint(object = object, new_data = new_data, ...),
    quantile    = predict_quantile(object = object, new_data = new_data, ...),
    time        = predict_time(object = object, new_data = new_data, ...),
    survival    = predict_survival(object = object, new_data = new_data, ...),
    linear_pred = predict_linear_pred(object = object, new_data = new_data, ...),
    hazard      = predict_hazard(object = object, new_data = new_data, ...),
    raw         = predict_raw(object = object, new_data = new_data, opts = opts, ...),
    rlang::abort(glue::glue("I don't know about type = '{type}'"))
  )
  if (!inherits(res, "tbl_spark")) {
    res <- switch(
      type,
      numeric     = format_num(res),
      class       = format_class(res),
      prob        = format_classprobs(res),
      time        = format_time(res),
      survival    = format_survival(res),
      hazard      = format_hazard(res),
      linear_pred = format_linear_pred(res),
      res
    )
  }
  res
}

surv_types <- c("time", "survival", "hazard")

#' @importFrom glue glue_collapse
check_pred_type <- function(object, type, ...) {
  if (is.null(type)) {
    type <-
      switch(object$spec$mode,
             regression = "numeric",
             classification = "class",
             "censored regression" = "time",
             rlang::abort("`type` should be 'regression', 'censored regression', or 'classification'."))
  }
  if (!(type %in% pred_types))
    rlang::abort(
      glue::glue(
        "`type` should be one of: ",
        glue_collapse(pred_types, sep = ", ", last = " and ")
      )
    )
  if (type == "numeric" & object$spec$mode != "regression")
    rlang::abort("For numeric predictions, the object should be a regression model.")
  if (type == "class" & object$spec$mode != "classification")
    rlang::abort("For class predictions, the object should be a classification model.")
  if (type == "prob" & object$spec$mode != "classification")
    rlang::abort("For probability predictions, the object should be a classification model.")
  if (type %in% surv_types & object$spec$mode != "censored regression")
    rlang::abort("For event time predictions, the object should be a censored regression.")

  # TODO check for ... options when not the correct type
  type
}

format_num <- function(x) {
  if (inherits(x, "tbl_spark"))
    return(x)

  if (isTRUE(ncol(x) > 1) | is.data.frame(x)) {
    x <- as_tibble(x, .name_repair = "minimal")
    if (!any(grepl("^\\.pred", names(x)))) {
      names(x) <- paste0(".pred_", names(x))
    }
  } else {
    x <- tibble(.pred = unname(x))
  }

  x
}

format_class <- function(x) {
  if (inherits(x, "tbl_spark"))
    return(x)

  tibble(.pred_class = unname(x))
}

format_classprobs <- function(x) {
  if (!any(grepl("^\\.pred_", names(x)))) {
    names(x) <- paste0(".pred_", names(x))
  }
  x <- as_tibble(x)
  x <- purrr::map_dfr(x, rlang::set_names, NULL)
  x
}

format_time <- function(x) {
  if (isTRUE(ncol(x) > 1) | is.data.frame(x)) {
    x <- as_tibble(x, .name_repair = "minimal")
    if (!any(grepl("^\\.time", names(x)))) {
      names(x) <- paste0(".time_", names(x))
    }
  } else {
    x <- tibble(.pred_time = unname(x))
  }

  x
}

format_survival <- function(x) {
  if (isTRUE(ncol(x) > 1) | is.data.frame(x)) {
    x <- as_tibble(x, .name_repair = "minimal")
    names(x) <- ".pred"
  } else {
    x <- tibble(.pred_survival = unname(x))
  }

  x
}

format_linear_pred <- function(x) {
  if (inherits(x, "tbl_spark"))
    return(x)

  if (isTRUE(ncol(x) > 1) | is.data.frame(x)) {
    x <- as_tibble(x, .name_repair = "minimal")
    names(x) <- ".pred_linear_pred"
  } else {
    x <- tibble(.pred_linear_pred = unname(x))
  }

  x
}

format_hazard <- function(x) {
  if (isTRUE(ncol(x) > 1) | is.data.frame(x)) {
    x <- as_tibble(x, .name_repair = "minimal")
    names(x) <- ".pred"
  } else {
    x <- tibble(.pred_hazard = unname(x))
  }

  x
}

make_pred_call <- function(x) {
  if ("pkg" %in% names(x$func))
    cl <-
      call2(x$func["fun"],!!!x$args, .ns = x$func["pkg"])
  else
    cl <-   call2(x$func["fun"],!!!x$args)

  cl
}

check_pred_type_dots <- function(type, ...) {
  the_dots <- list(...)
  nms <- names(the_dots)

  # ----------------------------------------------------------------------------

  if (any(names(the_dots) == "newdata")) {
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }

  # ----------------------------------------------------------------------------

  other_args <- c("level", "std_error", "quantile", "time")
  is_pred_arg <- names(the_dots) %in% other_args
  if (any(!is_pred_arg)) {
    bad_args <- names(the_dots)[!is_pred_arg]
    bad_args <- paste0("`", bad_args, "`", collapse = ", ")
    rlang::abort(
      glue::glue(
        "The ellipses are not used to pass args to the model function's ",
        "predict function. These arguments cannot be used: {bad_args}",
      )
    )
  }

  # ----------------------------------------------------------------------------
  # places where time should not be given
  if (any(nms == "time") & !type %in% c("survival", "hazard")) {
    rlang::abort(
      paste(
        "'time' should only be passed to `predict()` when 'type' is one of:",
        paste0("'", c("survival", "hazard"), "'", collapse = ", ")
      )
    )
  }
  # when time should be passed
  if (!any(nms == "time") & type %in% c("survival", "hazard")) {
    rlang::abort(
      paste(
        "When using 'type' values of 'survival' or 'hazard' are given,",
        "a numeric vector 'time' should also be given."
      )
    )
  }
  invisible(TRUE)
}


#' Prepare data based on parsnip encoding information
#' @param object A parsnip model object
#' @param new_data A data frame
#' @return A data frame or matrix
#' @keywords internal
#' @export
prepare_data <- function(object, new_data) {
  fit_interface <- object$spec$method$fit$interface

  pp_names <- names(object$preproc)
  if (any(pp_names == "terms") | any(pp_names == "x_var")) {
    # Translation code
    if (fit_interface == "formula") {
      new_data <- .convert_xy_to_form_new(object$preproc, new_data)
    } else {
      new_data <- .convert_form_to_xy_new(object$preproc, new_data)$x
    }
  }

  remove_intercept <-
    get_encoding(class(object$spec)[1]) %>%
    dplyr::filter(mode == object$spec$mode, engine == object$spec$engine) %>%
    dplyr::pull(remove_intercept)
  if (remove_intercept & any(grepl("Intercept", names(new_data)))) {
    new_data <- new_data %>% dplyr::select(-dplyr::one_of("(Intercept)"))
  }

  switch(
    fit_interface,
    none = new_data,
    data.frame = as.data.frame(new_data),
    matrix = as.matrix(new_data),
    new_data
  )
}

