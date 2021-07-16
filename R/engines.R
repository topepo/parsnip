
specific_model <- function(x) {
  cls <- class(x)
  cls[cls != "model_spec"]
}

possible_engines <- function(object, ...) {
  m_env <- get_model_env()
  engs <- rlang::env_get(m_env, specific_model(object))
  unique(engs$engine)
}

# ------------------------------------------------------------------------------

shhhh <- function(x)
  suppressPackageStartupMessages(requireNamespace(x, quietly = TRUE))

is_installed <- function(pkg) {
  res <- try(shhhh(pkg), silent = TRUE)
  res
}

#' @importFrom purrr map_lgl
check_installs <- function(x) {
  if (length(x$method$libs) > 0) {
    is_inst <- map_lgl(x$method$libs, is_installed)
    if (any(!is_inst)) {
      missing_pkg <- x$method$libs[!is_inst]
      missing_pkg <- paste0(missing_pkg, collapse = ", ")
      rlang::abort(
        glue::glue(
          "This engine requires some package installs: ",
          glue::glue_collapse(glue::glue("'{missing_pkg}'"), sep = ", ")
        )
      )
    }
  }
}

load_libs <- function(x, quiet, attach = FALSE) {
  for (pkg in x$method$libs) {
    if (!attach) {
      suppressPackageStartupMessages(requireNamespace(pkg, quietly = quiet))
    } else {
      library(pkg, character.only = TRUE)
    }
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' Declare a computational engine and specific arguments
#'
#' `set_engine()` is used to specify which package or system will be used
#'  to fit the model, along with any arguments specific to that software.
#'
#' @param object A model specification.
#' @param engine A character string for the software that should
#'  be used to fit the model. This is highly dependent on the type
#'  of model (e.g. linear regression, random forest, etc.).
#' @param ... Any optional arguments associated with the chosen computational
#'  engine. These are captured as quosures and can be `varying()`.
#' @return An updated model specification.
#' @examples
#' # First, set general arguments using the standardized names
#' mod <-
#'   logistic_reg(penalty = 0.01, mixture = 1/3) %>%
#'   # now say how you want to fit the model and another other options
#'   set_engine("glmnet", nlambda = 10)
#' translate(mod, engine = "glmnet")
#' @export
set_engine <- function(object, engine, ...) {
  if (!inherits(object, "model_spec")) {
    rlang::abort("`object` should have class 'model_spec'.")
  }

  if (rlang::is_missing(engine)) {
    avail_eng <- possible_engines(object)
    stop_incompatible_engine(avail_eng)
  }
  object$engine <- engine
  check_spec_mode_engine_val(class(object)[1], object$engine, object$mode)

  if (object$engine == "liquidSVM") {
    lifecycle::deprecate_soft(
      "0.1.6",
      "set_engine(engine = 'cannot be liquidSVM')",
      details = "The liquidSVM package is no longer available on CRAN.")
  }

  new_model_spec(
    cls = class(object)[1],
    args = object$args,
    eng_args = enquos(...),
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

#' Display currently available engines for a model
#'
#' The possible engines for a model can depend on what packages are loaded.
#' Some `parsnip`-adjacent packages add engines to existing models. For example,
#' the `multilevelmod` package adds additional engines for the [linear_reg()]
#' model and these are not available unless `multilevelmod` is loaded.
#' @param x The name of a `parsnip` model (e.g., "linear_reg", "mars", etc.)
#' @return A tibble.
#' @examples
#' show_engines("linear_reg")
#' @export
show_engines <- function(x) {
  if (!is.character(x) || length(x) > 1) {
    rlang::abort("`show_engines()` takes a single character string as input.")
  }
  res <- try(get_from_env(x), silent = TRUE)
  if (inherits(res, "try-error") | is.null(res)) {
    rlang::abort(
      paste0("No results found for model function '", x, "'.")
    )
  }
  res
}
