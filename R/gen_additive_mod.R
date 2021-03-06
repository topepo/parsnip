#' Generalized additive models (GAMs)
#'
#' @description
#' `gen_additive_mod()` defines a model that can use smoothed functions of
#' numeric predictors in a generalized linear model.
#'
#' There are different ways to fit this model. See the engine-specific pages
#' for more details
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("gen_additive_mod")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param select_features `TRUE` or `FALSE.` If `TRUE`, the model has the
#'  ability to eliminate a predictor (via penalization). Increasing
#'  `adjust_deg_free` will increase the likelihood of removing predictors.
#' @param adjust_deg_free If `select_features = TRUE`, then acts as a multiplier
#'  for smoothness. Increase this beyond 1 to produce smoother models.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("gen_additive_mod")}
#'
#' @examples
#' show_engines("gen_additive_mod")
#'
#' gen_additive_mod()
#'
#' @export
gen_additive_mod <- function(mode = "unknown",
                             select_features = NULL,
                             adjust_deg_free = NULL,
                             engine = "mgcv") {

  args <- list(
    select_features = rlang::enquo(select_features),
    adjust_deg_free = rlang::enquo(adjust_deg_free)
  )

  new_model_spec(
    "gen_additive_mod",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = engine
  )

}

#' @export
print.gen_additive_mod <- function(x, ...) {
  cat("GAM Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

#' @export
#' @rdname parsnip_update
#' @importFrom stats update
#' @inheritParams gen_additive_mod
update.gen_additive_mod <- function(object,
                                    select_features = NULL,
                                    adjust_deg_free = NULL,
                                    parameters = NULL,
                                    fresh = FALSE, ...) {

  update_dot_check(...)

  if (!is.null(parameters)) {
    parameters <- check_final_param(parameters)
  }

  args <- list(
    select_features = rlang::enquo(select_features),
    adjust_deg_free = rlang::enquo(adjust_deg_free)
  )

  args <- update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
  } else {
    null_args <- purrr::map_lgl(args, null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
  }

  new_model_spec(
    "gen_additive_mod",
    args     = object$args,
    eng_args = object$eng_args,
    mode     = object$mode,
    method   = NULL,
    engine   = object$engine
  )
}


#' @export
translate.gen_additive_mod <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'mgcv'` for translation.")
    engine <- "gam"
  }
  x <- translate.default(x, engine, ...)

  x
}

#' @export
#' @keywords internal
fit_xy.gen_additive_mod <- function(object, ...) {
  rlang::abort("`fit()` must be used with GAM models (due to its use of formulas).")
}
