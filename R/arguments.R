#' Functions required for parsnip-adjacent packages
#'
#' These functions are helpful when creating new packages that will register
#' new model specifications.
#' @export
#' @keywords internal
#' @rdname add_on_exports
#' @importFrom rlang expr enquos enquo quos is_quosure call2 quo_get_expr ll
#' @importFrom rlang abort current_env get_expr is_missing is_null is_symbolic  missing_arg
null_value <- function(x) {
  if (is_quosure(x)) {
    res <- isTRUE(all.equal(rlang::get_expr(x), expr(NULL)))
  } else {
    res <- isTRUE(all.equal(x, NULL))
  }
  res
}

check_eng_args <- function(args, obj, core_args) {
  # Make sure that we are not trying to modify an argument that
  # is explicitly protected in the method metadata or arg_key
  protected_args <- unique(c(obj$protect, core_args))
  common_args <- intersect(protected_args, names(args))
  if (length(common_args) > 0) {
    args <- args[!(names(args) %in% common_args)]
    common_args <- paste0(common_args, collapse = ", ")
    rlang::warn(glue::glue("The following arguments cannot be manually modified ",
                           "and were removed: {common_args}."))
  }
  args
}

#' Change elements of a model specification
#'
#' `set_args()` can be used to modify the arguments of a model specification while
#'  `set_mode()` is used to change the model's mode.
#'
#' @param object A model specification.
#' @param ... One or more named model arguments.
#' @param mode A character string for the model type (e.g. "classification" or
#'  "regression")
#' @return An updated model object.
#' @details `set_args()` will replace existing values of the arguments.
#'
#' @examples
#' rand_forest()
#'
#' rand_forest() %>%
#'   set_args(mtry = 3, importance = TRUE) %>%
#'   set_mode("regression")
#'
#' @export
set_args <- function(object, ...) {
  the_dots <- enquos(...)
  if (length(the_dots) == 0)
    rlang::abort("Please pass at least one named argument.")
  main_args <- names(object$args)
  new_args <- names(the_dots)
  for (i in new_args) {
    if (any(main_args == i)) {
      object$args[[i]] <- the_dots[[i]]
    } else {
      object$eng_args[[i]] <- the_dots[[i]]
    }
  }
  new_model_spec(
    cls = class(object)[1],
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

#' @rdname set_args
#' @export
set_mode <- function(object, mode) {
  cls <- class(object)[1]
  if (rlang::is_missing(mode)) {
    spec_modes <- rlang::env_get(get_model_env(), paste0(cls, "_modes"))
    stop_incompatible_mode(spec_modes)
  }
  check_spec_mode_engine_val(cls, object$engine, mode)
  object$mode <- mode
  object
}

# ------------------------------------------------------------------------------

#' @importFrom rlang eval_tidy
#' @importFrom purrr map
maybe_eval <- function(x) {
  # if descriptors are in `x`, eval fails
  y <- try(rlang::eval_tidy(x), silent = TRUE)
  if (inherits(y, "try-error"))
    y <- x
  y
}

#' Evaluate parsnip model arguments
#' @export
#' @keywords internal
#' @param spec A model specification
#' @param ... Not used.
eval_args <- function(spec, ...) {
  spec$args   <- purrr::map(spec$args,   maybe_eval)
  spec$eng_args <- purrr::map(spec$eng_args, maybe_eval)
  spec
}

# ------------------------------------------------------------------------------

# In some cases, a model function that we are calling has non-standard argument
# names. For example, a function foo() that only has the x/y interface might
# have a signature like `foo(X, Y)`.

# To deal with this, we allow for the `data` element of the model
# as an option to specify these actual argument names
#
#   value = list(
#     interface = "xy",
#     data = c(x = "X", y = "Y"),
#     protect = c("X", "Y"),
#     func = c(pkg = "bar", fun = "foo"),
#     defaults = list()
#   )

#' Make a parsnip call expression
#'
#' @param fun A character string of a function name.
#' @param ns A character string of a package name.
#' @param args A named list of argument values.
#' @details The arguments are spliced into the `ns::fun()` call. If they are
#' missing, null, or a single logical, then are not spliced.
#' @return A call.
#' @keywords internal
#' @export
make_call <- function(fun, ns, args, ...) {
  # remove any null or placeholders (`missing_args`) that remain
  discard <-
    vapply(args, function(x)
      is_missing_arg(x) | is.null(x), logical(1))
  args <- args[!discard]

  if (!is.null(ns) & !is.na(ns)) {
    out <- call2(fun, !!!args, .ns = ns)
  } else
    out <- call2(fun, !!!args)
  out
}


make_form_call <- function(object, env = NULL) {
  fit_args <- object$method$fit$args

  # Get the arguments related to data:
  if (is.null(object$method$fit$data)) {
    data_args <- c(formula = "formula", data = "data")
  } else {
    data_args <- object$method$fit$data
  }

  # add data arguments
  for (i in seq_along(data_args)) {
    fit_args[[ unname(data_args[i]) ]] <- sym(names(data_args)[i])
  }

  # sub in actual formula
  fit_args[[ unname(data_args["formula"]) ]]  <- env$formula

  if (object$engine == "spark") {
    env$x <- env$data
  }

  fit_call <- make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    fit_args
  )
  fit_call
}

make_xy_call <- function(object, target) {
  fit_args <- object$method$fit$args

  # Get the arguments related to data:
  if (is.null(object$method$fit$data)) {
    data_args <- c(x = "x", y = "y")
  } else {
    data_args <- object$method$fit$data
  }

  object$method$fit$args[[ unname(data_args["y"]) ]] <- rlang::expr(y)
  object$method$fit$args[[ unname(data_args["x"]) ]] <-
    switch(
      target,
      none = rlang::expr(x),
      data.frame = rlang::expr(maybe_data_frame(x)),
      matrix = rlang::expr(maybe_matrix(x)),
      rlang::abort(glue::glue("Invalid data type target: {target}."))
    )

  fit_call <- make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    object$method$fit$args
  )

  fit_call
}

## -----------------------------------------------------------------------------
#' Execution-time data dimension checks
#'
#' For some tuning parameters, the range of values depend on the data
#' dimensions (e.g. `mtry`). Some packages will fail if the parameter values are
#' outside of these ranges. Since the model might receive resampled versions of
#' the data, these ranges can't be set prior to the point where the model is
#' fit.  These functions check the possible range of the data and adjust them
#' if needed (with a warning).
#'
#' @param num_cols,num_rows The parameter value requested by the user.
#' @param source A data frame for the data to be used in the fit. If the source
#' is named "data", it is assumed that one column of the data corresponds to
#' an outcome (and is subtracted off).
#' @param offset A number subtracted off of the number of rows available in the
#' data.
#' @return An integer (and perhaps a warning).
#' @examples

#' nearest_neighbor(neighbors= 100) %>%
#'   set_engine("kknn") %>%
#'   set_mode("regression") %>%
#'   translate()
#'
#' library(ranger)
#' rand_forest(mtry = 2, min_n = 100, trees = 3) %>%
#'   set_engine("ranger") %>%
#'   set_mode("regression") %>%
#'   fit(mpg ~ ., data = mtcars)
#' @export
min_cols <- function(num_cols, source) {
  cl <- match.call()
  src_name <- rlang::expr_text(cl$source)
  if (cl$source == "data") {
    p <- ncol(source) - 1
  } else {
    p <- ncol(source)
  }
  if (num_cols > p) {
    msg <- paste0(num_cols, " columns were requested but there were ", p,
                 " predictors in the data. ", p, " will be used.")
    rlang::warn(msg)
    num_cols <- p
  }

  as.integer(num_cols)
}

#' @export
#' @rdname min_cols
min_rows <- function(num_rows, source, offset = 0) {
  n <- nrow(source)

  if (num_rows > n - offset) {
    msg <- paste0(num_rows, " samples were requested but there were ", n,
                  " rows in the data. ", n - offset, " will be used.")
    rlang::warn(msg)
    num_rows <- n - offset
  }

  as.integer(num_rows)
}
