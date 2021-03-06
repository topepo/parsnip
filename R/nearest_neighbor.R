#' K-nearest neighbors
#'
#' @description
#'
#' `nearest_neighbor()` defines a model that uses the `K` most similar data
#' points from the training set to predict new samples.
#'
#' There are different ways to fit this model. See the engine-specific pages
#' for more details:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("nearest_neighbor")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param neighbors A single integer for the number of neighbors
#' to consider (often called `k`). For \pkg{kknn}, a value of 5
#' is used if `neighbors` is not specified.
#' @param weight_func A *single* character for the type of kernel function used
#' to weight distances between samples. Valid choices are: `"rectangular"`,
#' `"triangular"`, `"epanechnikov"`, `"biweight"`, `"triweight"`,
#' `"cos"`, `"inv"`, `"gaussian"`, `"rank"`, or `"optimal"`.
#' @param dist_power A single number for the parameter used in
#' calculating Minkowski distance.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("nearest_neighbor")}
#'
#' @examples
#' show_engines("nearest_neighbor")
#'
#' nearest_neighbor(neighbors = 11)
#'
#' @export
nearest_neighbor <- function(mode = "unknown",
                             engine = "kknn",
                             neighbors = NULL,
                             weight_func = NULL,
                             dist_power = NULL) {
  args <- list(
    neighbors   = enquo(neighbors),
    weight_func = enquo(weight_func),
    dist_power  = enquo(dist_power)
  )

  new_model_spec(
    "nearest_neighbor",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

#' @export
print.nearest_neighbor <- function(x, ...) {
  cat("K-Nearest Neighbor Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update nearest_neighbor
#' @export
#' @rdname parsnip_update
update.nearest_neighbor <- function(object,
                                    parameters = NULL,
                                    neighbors = NULL,
                                    weight_func = NULL,
                                    dist_power = NULL,
                                    fresh = FALSE, ...) {

  eng_args <- update_engine_parameters(object$eng_args, ...)

  if (!is.null(parameters)) {
    parameters <- check_final_param(parameters)
  }

  args <- list(
    neighbors   = enquo(neighbors),
    weight_func = enquo(weight_func),
    dist_power  = enquo(dist_power)
  )

  args <- update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
    if (length(eng_args) > 0)
      object$eng_args[names(eng_args)] <- eng_args
  }

  new_model_spec(
    "nearest_neighbor",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}


positive_int_scalar <- function(x) {
  (length(x) == 1) && (x > 0) && (x %% 1 == 0)
}

# ------------------------------------------------------------------------------

check_args.nearest_neighbor <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$neighbors) && !positive_int_scalar(args$neighbors)) {
    rlang::abort("`neighbors` must be a length 1 positive integer.")
  }

  if (is.character(args$weight_func) && length(args$weight_func) > 1) {
    rlang::abort("The length of `weight_func` must be 1.")
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

#' @export
translate.nearest_neighbor <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'kknn'` for translation.")
    engine <- "kknn"
  }
  x <- translate.default(x, engine, ...)

  arg_vals <- x$method$fit$args

  if (engine == "kknn") {

    if (!any(names(arg_vals) == "ks") || is_missing_arg(arg_vals$ks)) {
      arg_vals$ks <- 5
    }

    ## -----------------------------------------------------------------------------
    # Protect some arguments based on data dimensions

    if (any(names(arg_vals) == "ks")) {
      arg_vals$ks <-
        rlang::call2("min_rows", rlang::eval_tidy(arg_vals$ks), expr(data), 5)
    }
  }

  x$method$fit$args <- arg_vals

  x
}


# ------------------------------------------------------------------------------

#' @importFrom purrr map_df
#' @importFrom dplyr starts_with
#' @rdname multi_predict
#' @param neighbors An integer vector for the number of nearest neighbors.
#' @export
multi_predict._train.kknn <-
  function(object, new_data, type = NULL, neighbors = NULL, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    if (is.null(neighbors))
      neighbors <- rlang::eval_tidy(object$fit$call$ks)
    neighbors <- sort(neighbors)

    if (is.null(type)) {
      if (object$spec$mode == "classification")
        type <- "class"
      else
        type <- "numeric"
    }

    res <-
      purrr::map_df(neighbors, knn_by_k, object = object,
                    new_data = new_data, type = type, ...)
    res <- dplyr::arrange(res, .row, neighbors)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    dplyr::tibble(.pred = res)
  }

knn_by_k <- function(k, object, new_data, type, ...) {
  object$fit$best.parameters$k <- k

  predict(object, new_data = new_data, type = type, ...) %>%
    dplyr::mutate(neighbors = k, .row = dplyr::row_number()) %>%
    dplyr::select(.row, neighbors, dplyr::starts_with(".pred"))
}
