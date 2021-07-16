#' Prepend a new class
#'
#' This adds an extra class to a base class of "model_spec".
#'
#' @param prefix A character string for a class.
#' @return A character vector.
#' @keywords internal
#' @importFrom rlang is_empty
#' @export
make_classes <- function(prefix) {
  c(prefix, "model_spec")
}

#' Check to ensure that ellipses are empty
#' @param ... Extra arguments.
#' @return If an error is not thrown (from non-empty ellipses), a NULL list.
#' @keywords internal
#' @export
check_empty_ellipse <- function (...)  {
  terms <- quos(...)
  if (!is_empty(terms))
    rlang::abort("Please pass other arguments to the model function via `set_engine()`.")
  terms
}

deparserizer <- function(x, limit = options()$width - 10) {
  x <- deparse(x, width.cutoff = limit)
  x <- gsub("^    ", "", x)
  x <- paste0(x, collapse = "")
  if (nchar(x) > limit)
    x <- paste0(substring(x, first = 1, last = limit - 7), "<snip>")
  x
}

print_arg_list <- function(x, ...) {
  atomic <- vapply(x, is.atomic, logical(1))
  x2 <- x
  x2[!atomic] <-  lapply(x2[!atomic], deparserizer, ...)
  res <- paste0("  ", names(x2), " = ", x2, collaspe = "\n")
  cat(res, sep = "")
}

#' Print helper for model objects
#'
#' A common format function that prints information about the model object (e.g.
#' arguments, calls, packages, etc).
#'
#' @param x A model object.
#' @param ... Not currently used.
#' @keywords internal
#' @importFrom rlang lgl
#' @export
model_printer <- function(x, ...) {
  non_null_args <- x$args[!vapply(x$args, null_value, lgl(1))]
  if (length(non_null_args) > 0) {
    cat("Main Arguments:\n")
    non_null_args <- map(non_null_args, convert_arg)
    cat(print_arg_list(non_null_args), "\n", sep = "")
  }
  if (length(x$eng_args) > 0) {
    cat("Engine-Specific Arguments:\n")
    x$eng_args <- map(x$eng_args, convert_arg)
    cat(print_arg_list(x$eng_args), "\n", sep = "")
  }
  if (!is.null(x$engine)) {
    cat("Computational engine:", x$engine, "\n\n")
    if (!is.null(x$method$fit_call)) {
      cat("Fit function:\n")
      print(x$method$fit_call)
      if (length(x$method$libs) > 0) {
        if (length(x$method$libs) > 1)
          cat("\nRequired packages:\n")
        else
          cat("\nRequired package: ")
        cat(paste0(x$method$libs, collapse = ", "), "\n")
      }
    }
  }
}

is_missing_arg <- function(x)
  identical(x, quote(missing_arg()))


#' Print the model call
#'
#' @param x A "model_spec" object.
#' @return A character string.
#' @keywords internal
#' @export
show_call <- function(object) {
  object$method$fit$args <-
    map(object$method$fit$args, convert_arg)
  if (
    is.null(object$method$fit$func["pkg"]) ||
    is.na(object$method$fit$func["pkg"])
  ) {
    res <- call2(object$method$fit$func["fun"], !!!object$method$fit$args)
  } else {
    res <-
      call2(object$method$fit$func["fun"],
            !!!object$method$fit$args,
            .ns = object$method$fit$func["pkg"])
  }
  res
}

convert_arg <- function(x) {
  if (is_quosure(x))
    quo_get_expr(x)
  else
    x
}


levels_from_formula <- function(f, dat) {
  if (inherits(dat, "tbl_spark"))
    res <- NULL
  else
    res <- levels(eval_tidy(f[[2]], dat))
  res
}

is_spark <- function(x)
  isTRUE(unname(x$method$fit$func["pkg"] == "sparklyr"))


#' @export
#' @keywords internal
#' @rdname add_on_exports
show_fit <- function(model, eng) {
  mod <- translate(x = model, engine = eng)
  fit_call <- show_call(mod)
  call_text <-  deparse(fit_call)
  call_text <- paste0(call_text, collapse = "\n")
  paste0(
    "\\preformatted{\n",
    call_text,
    "\n}\n\n"
  )
}

# Check non-translated core arguments
# Each model has its own definition of this
check_args <- function(object) {
  UseMethod("check_args")
}

check_args.default <- function(object) {
  invisible(object)
}

# ------------------------------------------------------------------------------

# copied form recipes

names0 <- function (num, prefix = "x") {
  if (num < 1)
    rlang::abort("`num` should be > 0.")
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}


# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname add_on_exports
update_dot_check <- function(...) {

  dots <- enquos(...)

  if (length(dots) > 0)
    rlang::abort(
      glue::glue(
        "Extra arguments will be ignored: ",
        glue::glue_collapse(glue::glue("`{names(dots)}`"), sep = ", ")
      )
    )
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname add_on_exports
new_model_spec <- function(cls, args, eng_args, mode, method, engine) {

  check_spec_mode_engine_val(cls, engine, mode)

  out <- list(args = args, eng_args = eng_args,
              mode = mode, method = method, engine = engine)
  class(out) <- make_classes(cls)
  out
}

# ------------------------------------------------------------------------------

check_outcome <- function(y, spec) {
  if (spec$mode == "unknown") {
    return(invisible(NULL))
  } else if (spec$mode == "regression") {
    if (!all(map_lgl(y, is.numeric)))
      rlang::abort("For a regression model, the outcome should be numeric.")
  } else if (spec$mode == "classification") {
    if (!all(map_lgl(y, is.factor))) {
      rlang::abort("For a classification model, the outcome should be a factor.")
    }
  }
  invisible(NULL)
}


# Get's a character string of varible names used as the outcome
# in a terms object
terms_y <- function(x) {
  att <- attributes(x)
  resp_ind <- att$response
  y_expr <- att$predvars[[resp_ind + 1]]
  all.vars(y_expr)
}


# ------------------------------------------------------------------------------

#' @export
#' @keywords internal
#' @rdname add_on_exports
check_final_param <- function(x) {
  if (is.null(x)) {
    return(invisible(x))
  }
  if (!is.list(x) & !tibble::is_tibble(x)) {
    rlang::abort("The parameter object should be a list or tibble")
  }
  if (tibble::is_tibble(x) && nrow(x) > 1) {
    rlang::abort("The parameter tibble should have a single row.")
  }
  if (tibble::is_tibble(x)) {
    x <- as.list(x)
  }
  if (length(names) == 0 || any(names(x) == "")) {
    rlang::abort("All values in `parameters` should have a name.")
  }

  invisible(x)
}

#' @export
#' @keywords internal
#' @rdname add_on_exports
update_main_parameters <- function(args, param) {

  if (length(param) == 0) {
    return(args)
  }
  if (length(args) == 0) {
    return(param)
  }

  # In case an engine argument is included:
  has_extra_args <- !(names(param) %in% names(args))
  extra_args <- names(param)[has_extra_args]
  if (any(has_extra_args)) {
    rlang::abort(
      paste("At least one argument is not a main argument:",
            paste0("`", extra_args, "`", collapse = ", "))
    )
  }
  param <- param[!has_extra_args]

  args <- utils::modifyList(args, param)
}

#' @export
#' @keywords internal
#' @rdname add_on_exports
update_engine_parameters <- function(eng_args, ...) {

  dots <- enquos(...)

  ## only update from dots when there are eng args in original model spec
  if (is_null(eng_args)) {
    ret <- NULL
  } else {
    ret <- utils::modifyList(eng_args, dots)
  }

  has_extra_dots <- !(names(dots) %in% names(eng_args))
  dots <- dots[has_extra_dots]
  update_dot_check(!!!dots)

  ret
}

# ------------------------------------------------------------------------------
# Since stan changed the function interface
#' Wrapper for stan confidence intervals
#' @param object A stan model fit
#' @param newdata A data set.
#' @export
#' @keywords internal
stan_conf_int <- function(object, newdata) {
  check_installs(list(method = list(libs = "rstanarm")))
  if (utils::packageVersion("rstanarm") >= "2.21.1") {
    fn <- rlang::call2("posterior_epred", .ns = "rstanarm",
                       object = expr(object),
                       newdata = expr(newdata),
                       seed = expr(sample.int(10^5, 1)))
  } else {
    fn <- rlang::call2("posterior_linpred", .ns = "rstanarm",
                       object = expr(object),
                       newdata = expr(newdata),
                       transform = TRUE,
                       seed = expr(sample.int(10^5, 1)))
  }
  rlang::eval_tidy(fn)
}

# ------------------------------------------------------------------------------


#' Helper functions for checking the penalty of glmnet models
#'
#' @description
#' These functions are for developer use.
#'
#' `.check_glmnet_penalty_fit()` checks that the model specification for fitting a
#' glmnet model contains a single value.
#'
#' `.check_glmnet_penalty_predict()` checks that the penalty value used for prediction is valid.
#' If called by `predict()`, it needs to be a single value. Multiple values are
#' allowed for `multi_predict()`.
#'
#' @param x An object of class `model_spec`.
#' @rdname glmnet_helpers
#' @keywords internal
#' @export
.check_glmnet_penalty_fit <- function(x) {
  pen <- rlang::eval_tidy(x$args$penalty)

  if (length(pen) != 1) {
    rlang::abort(c(
      "For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).",
      glue::glue("There are {length(pen)} values for `penalty`."),
      "To try multiple values for total regularization, use the tune package.",
      "To predict multiple penalties, use `multi_predict()`"
    ))
  }
}

#' @param penalty A penalty value to check.
#' @param object An object of class `model_fit`.
#' @param multi A logical indicating if multiple values are allowed.
#'
#' @rdname glmnet_helpers
#' @keywords internal
#' @export
.check_glmnet_penalty_predict <- function(penalty = NULL, object, multi = FALSE) {

  if (is.null(penalty)) {
    penalty <- object$fit$lambda
  }

  # when using `predict()`, allow for a single lambda
  if (!multi) {
    if (length(penalty) != 1)
      rlang::abort(
        glue::glue(
          "`penalty` should be a single numeric value. `multi_predict()` ",
          "can be used to get multiple predictions per row of data.",
        )
      )
  }

  if (length(object$fit$lambda) == 1 && penalty != object$fit$lambda)
    rlang::abort(
      glue::glue(
        "The glmnet model was fit with a single penalty value of ",
        "{object$fit$lambda}. Predicting with a value of {penalty} ",
        "will give incorrect results from `glmnet()`."
      )
    )

  penalty
}
