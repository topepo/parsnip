#' Resolve a Model Specification for a Computational Engine
#'
#' `translate()` will translate a model specification into a code
#'  object that is specific to a particular engine (e.g. R package).
#'  It translates generic parameters to their counterparts.
#'
#' @param x A model specification.
#' @param engine The computational engine for the model (see `?set_engine`).
#' @param ... Not currently used.
#' @details
#' `translate()` produces a _template_ call that lacks the specific
#'  argument values (such as `data`, etc). These are filled in once
#'  `fit()` is called with the specifics of the data for the model.
#'  The call may also include `varying` arguments if these are in
#'  the specification.
#'
#' It does contain the resolved argument names that are specific to
#'  the model fitting function/engine.
#'
#' This function can be useful when you need to understand how
#'  `parsnip` goes from a generic model specific to a model fitting
#'  function.
#'
#' **Note**: this function is used internally and users should only use it
#'  to understand what the underlying syntax would be. It should not be used
#'  to modify the model specification.
#'
#' @examples
#' lm_spec <- linear_reg(penalty = 0.01)
#'
#' # `penalty` is tranlsated to `lambda`
#' translate(lm_spec, engine = "glmnet")
#'
#' # `penalty` not applicable for this model.
#' translate(lm_spec, engine = "lm")
#'
#' # `penalty` is tranlsated to `reg_param`
#' translate(lm_spec, engine = "spark")
#'
#' # with a placeholder for an unknown argument value:
#' translate(linear_reg(penalty = varying(), mixture = varying()), engine = "glmnet")
#'
#' @export

translate <- function(x, ...)
  UseMethod("translate")

#' @importFrom utils getFromNamespace
#' @importFrom purrr list_modify
#' @importFrom rlang lgl
#' @rdname translate
#' @export
#' @export translate.default
translate.default <- function(x, engine = x$engine, ...) {
  check_empty_ellipse(...)
  if (is.null(engine))
    rlang::abort("Please set an engine.")

  mod_name <- specific_model(x)

  x$engine <- engine
  if (x$mode == "unknown") {
    rlang::abort("Model code depends on the mode; please specify one.")
  }

  check_spec_mode_engine_val(class(x)[1], x$engine, x$mode)

  if (is.null(x$method)) {
    x$method <- get_model_spec(mod_name, x$mode, engine)
  }

  arg_key <- get_args(mod_name, engine)

  # deharmonize primary arguments
  actual_args <- deharmonize(x$args, arg_key)

  # check secondary arguments to see if they are in the final
  # expression unless there are dots, warn if protected args are
  # being altered
  x$eng_args <- check_eng_args(x$eng_args, x$method$fit, arg_key$original)

  # keep only modified args
  modifed_args <- !purrr::map_lgl(actual_args, null_value)
  actual_args <- actual_args[modifed_args]

  # look for defaults if not modified in other
  if (length(x$method$fit$defaults) > 0) {
    in_other <- names(x$method$fit$defaults) %in% names(x$eng_args)
    x$defaults <- x$method$fit$defaults[!in_other]
  }

  # combine primary, eng_args, and defaults
  protected <- lapply(x$method$fit$protect, function(x) expr(missing_arg()))
  names(protected) <- x$method$fit$protect

  x$method$fit$args <- c(protected, actual_args, x$eng_args, x$defaults)

  x
}

#' @export
print.model_spec <- function(x, ...) {
  cat("Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)
  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

check_mode <- function(object, lvl) {
  if (object$mode == "unknown") {
    if (!is.null(lvl)) {
      object$mode <- "classification"
    } else {
      object$mode <- "regression"
    }
  }
  object
}

# ------------------------------------------------------------------------------
# new code for revised model data structures

get_model_spec <- function(model, mode, engine) {
  m_env <- get_model_env()
  env_obj <- rlang::env_names(m_env)
  env_obj <- grep(model, env_obj, value = TRUE)

  res <- list()
  res$libs <-
    rlang::env_get(m_env, paste0(model, "_pkgs")) %>%
    dplyr::filter(engine == !!engine) %>%
    purrr::pluck("pkg") %>%
    purrr::pluck(1)

  res$fit <-
    rlang::env_get(m_env, paste0(model, "_fit")) %>%
    dplyr::filter(mode == !!mode & engine == !!engine) %>%
    dplyr::pull(value) %>%
    purrr::pluck(1)

  pred_code <-
    rlang::env_get(m_env, paste0(model, "_predict")) %>%
    dplyr::filter(mode == !!mode & engine == !!engine) %>%
    dplyr::select(-engine, -mode)

  res$pred <- pred_code[["value"]]
  names(res$pred) <- pred_code$type

  res
}

get_args <- function(model, engine) {
  m_env <- get_model_env()
  rlang::env_get(m_env, paste0(model, "_args")) %>%
    dplyr::filter(engine == !!engine) %>%
    dplyr::select(-engine)
}

# to replace harmonize
deharmonize <- function(args, key) {
  if (length(args) == 0)
    return(args)
  parsn <- tibble(parsnip = names(args), order = seq_along(args))
  merged <-
    dplyr::left_join(parsn, key, by = "parsnip") %>%
    dplyr::arrange(order)
  # TODO correct for bad merge?

  names(args) <- merged$original
  args[!is.na(merged$original)]
}

add_methods <- function(x, engine) {
  x$engine <- engine
  check_spec_mode_engine_val(class(x)[1], x$engine, x$mode)
  x$method <- get_model_spec(specific_model(x), x$mode, x$engine)
  x
}
