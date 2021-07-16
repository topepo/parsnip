# Initialize model environments

all_modes <- c("classification", "regression", "censored regression")

# ------------------------------------------------------------------------------

## Rules about model-related information

### Definitions:

# - the model is the model type (e.g. "rand_forest", "linear_reg", etc)
# - the model's mode is the species of model such as "classification" or "regression"
# - the engines are within a model and mode and describe the method/implementation
#   of the model in question. These are often R package names.

### The package dependencies are model- and engine-specific. They are used across modes

### The `fit` information is a list of data that is needed to fit the model. This
### information is specific to an engine and mode.

### The `predict` information is also list of data that is needed to make some sort
### of prediction on the model object. The possible types are contained in `pred_types`
### and this information is specific to the engine, mode, and type (although there
### are no types across different modes).

# ------------------------------------------------------------------------------


parsnip <- rlang::new_environment()
parsnip$models <- NULL
parsnip$modes <- c("regression", "classification", "unknown")

# ------------------------------------------------------------------------------

pred_types <-
  c("raw", "numeric", "class", "prob", "conf_int", "pred_int", "quantile",
    "time", "survival", "linear_pred", "hazard")

# ------------------------------------------------------------------------------

#' Working with the parsnip model environment
#'
#' These functions read and write to the environment where the package stores
#'  information about model specifications.
#'
#' @param items A character string of objects in the model environment.
#' @param ... Named values that will be assigned to the model environment.
#' @param name A single character value for a new symbol in the model environment.
#' @param value A single value for a new value in the model environment.
#' @keywords internal
#' @references "How to build a parsnip model"
#'  \url{https://www.tidymodels.org/learn/develop/models/}
#' @examples
#' # Access the model data:
#' current_code <- get_model_env()
#' ls(envir = current_code)
#'
#' @keywords internal
#' @export
get_model_env <- function() {
  current <- utils::getFromNamespace("parsnip", ns = "parsnip")
  current
}

#' @rdname get_model_env
#' @keywords internal
#' @export
get_from_env <- function(items) {
  mod_env <- get_model_env()
  rlang::env_get(mod_env, items, default = NULL)
}

#' @rdname get_model_env
#' @keywords internal
#' @export
set_in_env <- function(...) {
  mod_env <- get_model_env()
  rlang::env_bind(mod_env, ...)
}

#' @rdname get_model_env
#' @keywords internal
#' @export
set_env_val <- function(name, value) {
  if (length(name) != 1 || !is.character(name)) {
    rlang::abort("`name` should be a single character value.")
  }
  mod_env <- get_model_env()
  x <- list(value)
  names(x) <- name
  rlang::env_bind(mod_env, !!!x)
}

# ------------------------------------------------------------------------------

check_eng_val <- function(eng) {
  if (rlang::is_missing(eng) || length(eng) != 1 || !is.character(eng))
    rlang::abort("Please supply a character string for an engine name (e.g. `'lm'`)")
  invisible(NULL)
}


check_model_exists <- function(model) {
  if (rlang::is_missing(model) || length(model) != 1 || !is.character(model)) {
    rlang::abort("Please supply a character string for a model name (e.g. `'linear_reg'`)")
  }

  current <- get_model_env()

  if (!any(current$models == model)) {
    rlang::abort(glue::glue("Model `{model}` has not been registered."))
  }

  invisible(NULL)
}

check_model_doesnt_exist <- function(model) {
  if (rlang::is_missing(model) || length(model) != 1 || !is.character(model)) {
    rlang::abort("Please supply a character string for a model name (e.g. `'linear_reg'`)")
  }

  current <- get_model_env()

  if (any(current$models == model)) {
    rlang::abort(glue::glue("Model `{model}` already exists"))
  }

  invisible(NULL)
}

check_mode_val <- function(mode) {
  if (rlang::is_missing(mode) || length(mode) != 1 || !is.character(mode)) {
    rlang::abort("Please supply a character string for a mode (e.g. `'regression'`).")
  }
  invisible(NULL)
}


stop_incompatible_mode <- function(spec_modes, eng) {
  if (is.null(eng)) {
    msg <- glue::glue(
      "Available modes are: ",
      glue::glue_collapse(glue::glue("'{spec_modes}'"), sep = ", ")
    )
  } else {
    msg <- glue::glue(
      "Available modes for engine {eng} are: ",
      glue::glue_collapse(glue::glue("'{spec_modes}'"), sep = ", ")
    )
  }

  rlang::abort(msg)
}

stop_incompatible_engine <- function(spec_engs, mode) {
  msg <- glue::glue(
    "Available engines for mode {mode} are: ",
    glue::glue_collapse(glue::glue("'{spec_engs}'"), sep = ", ")
  )
  rlang::abort(msg)
}

# check if class and mode and engine are compatible
check_spec_mode_engine_val <- function(cls, eng, mode) {
  all_modes <- c("unknown", all_modes)
  if (!(mode %in% all_modes)) {
    rlang::abort(paste0("'", mode, "' is not a known mode."))
  }

  model_info <- rlang::env_get(get_model_env(), cls)

  # Cases where the model definition is in parsnip but all of the engines
  # are contained in a different package
  if (nrow(model_info) == 0) {
    return(invisible(NULL))
  }

  # ------------------------------------------------------------------------------
  # First check engine against any mode

  spec_engs <- model_info$engine
  # engine is allowed to be NULL
  if (!is.null(eng) && !(eng %in% spec_engs)) {
    rlang::abort(
      paste0(
        "Engine '", eng, "' is not supported for `", cls, "()`. See ",
        "`show_engines('", cls, "')`."
      )
    )
  }

  # ----------------------------------------------------------------------------
  # Check modes based on model and engine

  spec_modes <- model_info$mode
  if (!is.null(eng)) {
    spec_modes <- spec_modes[model_info$engine == eng]
  }
  spec_modes <- unique(c("unknown", spec_modes))

  if (is.null(mode) || length(mode) > 1) {
    stop_incompatible_mode(spec_modes, eng)
  } else if (!(mode %in% spec_modes)) {
    stop_incompatible_mode(spec_modes, eng)
  }

  # ----------------------------------------------------------------------------
  # Check engine based on model and model

  # How check for compatibility with the chosen mode (if any)
  if (!is.null(mode) && mode != "unknown") {
    spec_engs <- spec_engs[model_info$mode == mode]
  }
  spec_engs <- unique(spec_engs)
  if (!is.null(eng) && !(eng %in% spec_engs)) {
    stop_incompatible_engine(spec_engs, mode)
  }

  invisible(NULL)
}


check_engine_val <- function(eng) {
  if (rlang::is_missing(eng) || length(eng) != 1 || !is.character(eng))
    rlang::abort("Please supply a character string for an engine (e.g. `'lm'`).")
  invisible(NULL)
}

check_arg_val <- function(arg) {
  if (rlang::is_missing(arg) || length(arg) != 1 || !is.character(arg))
    rlang::abort("Please supply a character string for the argument.")
  invisible(NULL)
}

check_submodels_val <- function(has_submodel) {
  if (!is.logical(has_submodel) || length(has_submodel) != 1) {
    rlang::abort("The `submodels` argument should be a single logical.")
  }
  invisible(NULL)
}

check_func_val <- function(func) {
  msg <-
    paste(
      "`func` should be a named vector with element 'fun' and the optional ",
      "elements 'pkg', 'range', 'trans', and 'values'.",
      "`func` and 'pkg' should both be single character strings."
    )

  if (rlang::is_missing(func) || !is.vector(func))
    rlang::abort(msg)

  nms <- sort(names(func))

  if (all(is.null(nms)))  {
    rlang::abort(msg)
  }

  if (length(func) == 1) {
    if (isTRUE(any(nms != "fun"))) {
      rlang::abort(msg)
    }
  } else {
    # check for extra names:
    allow_nms <- c("fun", "pkg", "range", "trans", "values")
    nm_check <- nms %in% c("fun", "pkg", "range", "trans", "values")
    not_allowed <- nms[!(nms %in% allow_nms)]
    if (length(not_allowed) > 0) {
      rlang::abort(msg)
    }
  }

  if (!is.character(func[["fun"]])) {
    rlang::abort(msg)
  }
  if (any(nms == "pkg") && !is.character(func[["pkg"]])) {
    rlang::abort(msg)
  }

  invisible(NULL)
}

check_fit_info <- function(fit_obj) {
  if (is.null(fit_obj)) {
    rlang::abort("The `fit` module cannot be NULL.")
  }

  # check required data elements
  exp_nms <- c("defaults", "func", "interface", "protect")
  has_req_nms <- exp_nms %in% names(fit_obj)

  if (!all(has_req_nms)) {
    rlang::abort(
      glue::glue("The `fit` module should have elements: ",
      glue::glue_collapse(glue::glue("`{exp_nms}`"), sep = ", "))
      )
  }

  # check optional data elements
  opt_nms <- c("data")
  other_nms <- setdiff(exp_nms, names(fit_obj))
  has_opt_nms <- other_nms %in% opt_nms
  if (any(!has_opt_nms)) {
    msg <- glue::glue("The `fit` module can only have optional elements: ",
                      glue::glue_collapse(glue::glue("`{exp_nms}`"), sep = ", "))

    rlang::abort(msg)
  }
  if (any(other_nms == "data")) {
    data_nms <- names(fit_obj$data)
    if (length(data_nms == 0) || any(data_nms == "")) {
      rlang::abort("All elements of the `data` argument vector must be named.")
    }
  }

  check_interface_val(fit_obj$interface)
  check_func_val(fit_obj$func)

  if (!is.list(fit_obj$defaults)) {
    rlang::abort("The `defaults` element should be a list: ")
  }

  invisible(NULL)
}

check_pred_info <- function(pred_obj, type) {
  if (all(type != pred_types)) {
    rlang::abort(
      glue::glue("The prediction type should be one of: ",
      glue::glue_collapse(glue::glue("'{pred_types}'"), sep = ", "))
      )
  }

  exp_nms <- c("args", "func", "post", "pre")
  if (!isTRUE(all.equal(sort(names(pred_obj)), exp_nms))) {
    rlang::abort(
      glue::glue("The `predict` module should have elements: ",
      glue::glue_collapse(glue::glue("`{exp_nms}`"), sep = ", "))
      )
  }

  if (!is.null(pred_obj$pre) & !is.function(pred_obj$pre)) {
    rlang::abort("The `pre` module should be null or a function: ")
  }
  if (!is.null(pred_obj$post) & !is.function(pred_obj$post)) {
    rlang::abort("The `post` module should be null or a function: ")
  }

  check_func_val(pred_obj$func)

  if (!is.list(pred_obj$args)) {
    rlang::abort("The `args` element should be a list. ")
  }

  invisible(NULL)
}

spec_has_pred_type <- function(object, type) {
  possible_preds <- names(object$spec$method$pred)
  any(possible_preds == type)
}
check_spec_pred_type <- function(object, type) {
  if (!spec_has_pred_type(object, type)) {
    possible_preds <- names(object$spec$method$pred)
    rlang::abort(c(
      glue::glue("No {type} prediction method available for this model."),
      glue::glue("Value for `type` should be one of: ",
                 glue::glue_collapse(glue::glue("'{possible_preds}'"), sep = ", "))
    ))
  }
  invisible(NULL)
}


check_pkg_val <- function(pkg) {
  if (rlang::is_missing(pkg) || length(pkg) != 1 || !is.character(pkg)) {
    rlang::abort("Please supply a single character value for the package name.")
  }
  invisible(NULL)
}


check_interface_val <- function(x) {
  exp_interf <- c("data.frame", "formula", "matrix")
  if (length(x) != 1 || !(x %in% exp_interf)) {
    rlang::abort(
      glue::glue("The `interface` element should have a single value of: ",
                 glue::glue_collapse(glue::glue("`{exp_interf}`"), sep = ", "))
    )
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------------

#' Tools to Register Models
#'
#' These functions are similar to constructors and can be used to validate
#'  that there are no conflicts with the underlying model structures used by the
#'  package.
#'
#' @param model A single character string for the model type (e.g.
#'  `"rand_forest"`, etc).
#' @param mode A single character string for the model mode (e.g. "regression").
#' @param eng A single character string for the model engine.
#' @param arg A single character string for the model argument name.
#' @param has_submodel A single logical for whether the argument
#'  can make predictions on multiple submodels at once.
#' @param func A named character vector that describes how to call
#'  a function. `func` should have elements `pkg` and `fun`. The
#'  former is optional but is recommended and the latter is
#'  required. For example, `c(pkg = "stats", fun = "lm")` would be
#'  used to invoke the usual linear regression function. In some
#'  cases, it is helpful to use `c(fun = "predict")` when using a
#'  package's `predict` method.
#' @param fit_obj A list with elements `interface`, `protect`,
#'  `func` and `defaults`. See the package vignette "Making a
#'  `parsnip` model from scratch".
#' @param pred_obj A list with elements `pre`, `post`, `func`, and `args`.
#' @param type A single character value for the type of prediction. Possible
#'  values are: `class`, `conf_int`, `numeric`, `pred_int`, `prob`, `quantile`,
#'   and `raw`.
#' @param pkg An options character string for a package name.
#' @param parsnip A single character string for the "harmonized" argument name
#'  that `parsnip` exposes.
#' @param original A single character string for the argument name that
#'  underlying model function uses.
#' @param value A list that conforms to the `fit_obj` or `pred_obj` description
#'  below, depending on context.
#' @param pre,post Optional functions for pre- and post-processing of prediction
#'  results.
#' @param options A list of options for engine-specific preprocessing encodings.
#'  See Details below.
#' @param ... Optional arguments that should be passed into the `args` slot for
#'  prediction objects.
#' @keywords internal
#' @details These functions are available for users to add their
#'  own models or engines (in package or otherwise) so that they can
#'  be accessed using `parsnip`. This is more thoroughly documented
#'  on the package web site (see references below).
#'
#' In short, `parsnip` stores an environment object that contains
#'  all of the information and code about how models are used (e.g.
#'  fitting, predicting, etc). These functions can be used to add
#'  models to that environment as well as helper functions that can
#'  be used to makes sure that the model data is in the right
#'  format.
#'
#' `check_model_exists()` checks the model value and ensures that the model has
#'  already been registered. `check_model_doesnt_exist()` checks the model value
#'  and also checks to see if it is novel in the environment.
#'
#'  The options for engine-specific encodings dictate how the predictors should be
#'  handled. These options ensure that the data
#'  that `parsnip` gives to the underlying model allows for a model fit that is
#'  as similar as possible to what it would have produced directly.
#'
#'  For example, if `fit()` is used to fit a model that does not have
#'  a formula interface, typically some predictor preprocessing must
#'  be conducted. `glmnet` is a good example of this.
#'
#'   There are four options that can be used for the encodings:
#'
#'  `predictor_indicators` describes whether and how to create indicator/dummy
#'  variables from factor predictors. There are three options: `"none"` (do not
#'  expand factor predictors), `"traditional"` (apply the standard
#'  `model.matrix()` encodings), and `"one_hot"` (create the complete set
#'  including the baseline level for all factors). This encoding only affects
#'  cases when [fit.model_spec()] is used and the underlying model has an x/y
#'  interface.
#'
#' Another option is `compute_intercept`; this controls whether `model.matrix()`
#'  should include the intercept in its formula. This affects more than the
#'  inclusion of an intercept column. With an intercept, `model.matrix()`
#'  computes dummy variables for all but one factor levels. Without an
#'  intercept, `model.matrix()` computes a full set of indicators for the
#'  _first_ factor variable, but an incomplete set for the remainder.
#'
#'  Next, the option `remove_intercept` will remove the intercept column
#'  _after_ `model.matrix()` is finished. This can be useful if the model
#'  function (e.g. `lm()`) automatically generates an intercept.
#'
#' Finally, `allow_sparse_x` specifies whether the model function can natively
#'  accommodate a sparse matrix representation for predictors during fitting
#'  and tuning.
#'
#'
#' @references "How to build a parsnip model"
#'  \url{https://www.tidymodels.org/learn/develop/models/}
#' @examples
#' # set_new_model("shallow_learning_model")
#'
#' # Show the information about a model:
#' show_model_info("rand_forest")
#' @keywords internal
#' @export
set_new_model <- function(model) {
  check_model_doesnt_exist(model)

  current <- get_model_env()

  set_env_val("models", c(current$models, model))
  set_env_val(model, dplyr::tibble(engine = character(0), mode = character(0)))
  set_env_val(
    paste0(model, "_pkgs"),
    dplyr::tibble(engine = character(0), pkg = list())
  )
  set_env_val(paste0(model, "_modes"), "unknown")
  set_env_val(
    paste0(model, "_args"),
    dplyr::tibble(
      engine = character(0),
      parsnip = character(0),
      original = character(0),
      func = list(),
      has_submodel = logical(0)
    )
  )
  set_env_val(
    paste0(model, "_fit"),
    dplyr::tibble(
      engine = character(0),
      mode = character(0),
      value = list()
    )
  )
  set_env_val(
    paste0(model, "_predict"),
    dplyr::tibble(
      engine = character(0),
      mode = character(0),
      type = character(0),
      value = list()
    )
  )

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_model_mode <- function(model, mode) {
  check_model_exists(model)
  check_mode_val(mode)

  current <- get_model_env()

  if (!any(current$modes == mode)) {
    current$modes <- unique(c(current$modes, mode))
  }

  set_env_val(
    paste0(model, "_modes"),
    unique(c(get_from_env(paste0(model, "_modes")), mode))
  )
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @importFrom dplyr %>%
#' @export
set_model_engine <- function(model, mode, eng) {
  check_model_exists(model)
  check_mode_val(mode)
  check_eng_val(eng)
  check_mode_val(eng)

  current <- get_model_env()

  new_eng <- dplyr::tibble(engine = eng, mode = mode)
  old_eng <- get_from_env(model)

  engs <-
    old_eng %>%
    dplyr::bind_rows(new_eng) %>%
    dplyr::distinct()

  set_env_val(model, engs)
  set_model_mode(model, mode)
  invisible(NULL)
}


# ------------------------------------------------------------------------------
#' @importFrom vctrs vec_unique
#' @rdname set_new_model
#' @keywords internal
#' @export
set_model_arg <- function(model, eng, parsnip, original, func, has_submodel) {
  check_model_exists(model)
  check_eng_val(eng)
  check_arg_val(parsnip)
  check_arg_val(original)
  check_func_val(func)
  check_submodels_val(has_submodel)

  current <- get_model_env()
  old_args <- get_from_env(paste0(model, "_args"))

  new_arg <-
    dplyr::tibble(
      engine = eng,
      parsnip = parsnip,
      original = original,
      func = list(func),
      has_submodel = has_submodel
    )

  updated <- try(dplyr::bind_rows(old_args, new_arg), silent = TRUE)
  if (inherits(updated, "try-error")) {
    rlang::abort("An error occured when adding the new argument.")
  }

  updated <- vctrs::vec_unique(updated)
  set_env_val(paste0(model, "_args"), updated)

  invisible(NULL)
}


# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_dependency <- function(model, eng, pkg) {
  check_model_exists(model)
  check_eng_val(eng)
  check_pkg_val(pkg)

  current <- get_model_env()
  model_info <- get_from_env(model)
  pkg_info <- get_from_env(paste0(model, "_pkgs"))

  has_engine <-
    model_info %>%
    dplyr::distinct(engine) %>%
    dplyr::filter(engine == eng) %>%
    nrow()
  if (has_engine != 1) {
    rlang::abort("The engine '{eng}' has not been registered for model '{model}'.")
  }

  existing_pkgs <-
    pkg_info %>%
    dplyr::filter(engine == eng)

  if (nrow(existing_pkgs) == 0) {
    pkg_info <-
      pkg_info %>%
      dplyr::bind_rows(tibble(engine = eng, pkg = list(pkg)))

  } else {
    old_pkgs <- existing_pkgs
    existing_pkgs$pkg[[1]] <- c(pkg, existing_pkgs$pkg[[1]])
    pkg_info <-
      pkg_info %>%
      dplyr::filter(engine != eng) %>%
      dplyr::bind_rows(existing_pkgs)
  }

  set_env_val(paste0(model, "_pkgs"), pkg_info)

  invisible(NULL)
}

#' @rdname set_new_model
#' @keywords internal
#' @export
get_dependency <- function(model) {
  check_model_exists(model)
  pkg_name <- paste0(model, "_pkgs")
  if (!any(pkg_name != rlang::env_names(get_model_env()))) {
    rlang::abort(glue::glue("`{model}` does not have a dependency list in parsnip."))
  }
  rlang::env_get(get_model_env(), pkg_name)
}


# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_fit <- function(model, mode, eng, value) {
  check_model_exists(model)
  check_eng_val(eng)
  check_spec_mode_engine_val(model, eng, mode)
  check_fit_info(value)

  current <- get_model_env()
  model_info <- get_from_env(model)
  old_fits <- get_from_env(paste0(model, "_fit"))

  has_engine <-
    model_info %>%
    dplyr::filter(engine == eng & mode == !!mode) %>%
    nrow()
  if (has_engine != 1) {
    rlang::abort(glue::glue("The combination of '{eng}' and mode '{mode}' has not ",
                            "been registered for model '{model}'."))
  }

  has_fit <-
    old_fits %>%
    dplyr::filter(engine == eng & mode == !!mode) %>%
    nrow()

  if (has_fit > 0) {
    rlang::abort(glue::glue("The combination of '{eng}' and mode '{mode}' ",
                            "already has a fit component for model '{model}'."))
  }

  new_fit <-
    dplyr::tibble(
      engine = eng,
      mode = mode,
      value = list(value)
    )

  updated <- try(dplyr::bind_rows(old_fits, new_fit), silent = TRUE)
  if (inherits(updated, "try-error")) {
    rlang::abort("An error occured when adding the new fit module.")
  }

  set_env_val(
    paste0(model, "_fit"),
    updated
  )

  invisible(NULL)
}

#' @rdname set_new_model
#' @keywords internal
#' @export
get_fit <- function(model) {
  check_model_exists(model)
  fit_name <- paste0(model, "_fit")
  if (!any(fit_name != rlang::env_names(get_model_env()))) {
    rlang::abort(glue::glue("`{model}` does not have a `fit` method in parsnip."))
  }
  rlang::env_get(get_model_env(), fit_name)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
set_pred <- function(model, mode, eng, type, value) {
  check_model_exists(model)
  check_eng_val(eng)
  check_spec_mode_engine_val(model, eng, mode)
  check_pred_info(value, type)

  current <- get_model_env()
  model_info <- get_from_env(model)
  old_fits <- get_from_env(paste0(model, "_predict"))

  has_engine <-
    model_info %>%
    dplyr::filter(engine == eng & mode == !!mode) %>%
    nrow()
  if (has_engine != 1) {
    rlang::abort(glue::glue("The combination of '{eng}' and mode '{mode}'",
                            "has not been registered for model '{model}'."))
  }

  has_pred <-
    old_fits %>%
    dplyr::filter(engine == eng & mode == !!mode & type == !!type) %>%
    nrow()
  if (has_pred > 0) {
    rlang::abort(glue::glue("The combination of '{eng}', mode '{mode}', ",
                            "and type '{type}' already has a prediction component",
                            "for model '{model}'."))
  }

  new_fit <-
    dplyr::tibble(
      engine = eng,
      mode = mode,
      type = type,
      value = list(value)
    )

  updated <- try(dplyr::bind_rows(old_fits, new_fit), silent = TRUE)
  if (inherits(updated, "try-error")) {
    rlang::abort("An error occured when adding the new fit module.")
  }

  set_env_val(paste0(model, "_predict"), updated)

  invisible(NULL)
}

#' @rdname set_new_model
#' @keywords internal
#' @export
get_pred_type <- function(model, type) {
  check_model_exists(model)
  pred_name <- paste0(model, "_predict")
  if (!any(pred_name != rlang::env_names(get_model_env()))) {
    rlang::abort(glue::glue("`{model}` does not have any `pred` methods in parsnip."))
  }
  all_preds <- rlang::env_get(get_model_env(), pred_name)
  if (!any(all_preds$type == type)) {
    rlang::abort(glue::glue("`{model}` does not have any prediction methods in parsnip."))
  }
  dplyr::filter(all_preds, type == !!type)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
show_model_info <- function(model) {
  check_model_exists(model)
  current <- get_model_env()

  cat("Information for `", model, "`\n", sep = "")

  cat(
    " modes:",
    paste0(get_from_env(paste0(model, "_modes")), collapse = ", "),
    "\n\n"
  )

  engines <- get_from_env(model)
  if (nrow(engines) > 0) {
    cat(" engines: \n")
    engines %>%
      dplyr::mutate(
        mode = format(paste0(mode, ": "))
      ) %>%
      dplyr::group_by(mode) %>%
      dplyr::summarize(
        engine = paste0(sort(engine), collapse = ", ")
      ) %>%
      dplyr::mutate(
        lab = paste0("   ", mode, engine, "\n")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::pull(lab) %>%
      cat(sep = "")
    cat("\n")
  } else {
    cat(" no registered engines.\n\n")
  }

  args <- get_from_env(paste0(model, "_args"))
  if (nrow(args) > 0) {
    cat(" arguments: \n")
    args %>%
      dplyr::select(engine, parsnip, original) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        engine = format(paste0("   ", engine, ": ")),
        parsnip = paste0("      ", format(parsnip), " --> ", original, "\n")
      ) %>%
      dplyr::group_by(engine) %>%
      dplyr::mutate(
        engine2 = ifelse(dplyr::row_number() == 1, engine, ""),
        parsnip = ifelse(dplyr::row_number() == 1, paste0("\n", parsnip), parsnip),
        lab = paste0(engine2, parsnip)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::pull(lab) %>%
      cat(sep = "")
    cat("\n")
  } else {
    cat(" no registered arguments.\n\n")
  }

  fits <- get_from_env(paste0(model, "_fit"))
  if (nrow(fits) > 0) {
    cat(" fit modules:\n")
    fits %>%
      dplyr::select(-value) %>%
      mutate(engine = paste0("  ", engine)) %>%
      as.data.frame() %>%
      print(row.names = FALSE)
    cat("\n")
  } else {
    cat(" no registered fit modules.\n\n")
  }

  preds <- get_from_env(paste0(model, "_predict"))
  if (nrow(preds) > 0) {
    cat(" prediction modules:\n")
    preds %>%
      dplyr::group_by(mode, engine) %>%
      dplyr::summarize(methods = paste0(sort(type), collapse = ", ")) %>%
      dplyr::ungroup() %>%
      mutate(mode = paste0("  ", mode)) %>%
      as.data.frame() %>%
      print(row.names = FALSE)
    cat("\n")
  } else {
    cat(" no registered prediction modules.\n\n")
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @rdname set_new_model
#' @keywords internal
#' @export
pred_value_template <-  function(pre = NULL, post = NULL, func, ...) {
  if (rlang::is_missing(func)) {
    rlang::abort("Please supply a value to `func`. See `?set_pred`.")
  }
  list(pre = pre, post = post, func = func, args = list(...))
}

# ------------------------------------------------------------------------------

check_encodings <- function(x) {
  if (!is.list(x)) {
    rlang::abort("`values` should be a list.")
  }
  req_args <- list(predictor_indicators = rlang::na_chr,
                   compute_intercept = rlang::na_lgl,
                   remove_intercept = rlang::na_lgl,
                   allow_sparse_x = rlang::na_lgl)

  missing_args <- setdiff(names(req_args), names(x))
  if (length(missing_args) > 0) {
    rlang::abort(
      glue::glue(
        "The values passed to `set_encoding()` are missing arguments: ",
        paste0("'", missing_args, "'", collapse = ", ")
      )
    )
  }
  extra_args <- setdiff(names(x), names(req_args))
  if (length(extra_args) > 0) {
    rlang::abort(
      glue::glue(
        "The values passed to `set_encoding()` had extra arguments: ",
        paste0("'", extra_args, "'", collapse = ", ")
      )
    )
  }
  invisible(x)
}

#' @export
#' @rdname set_new_model
#' @keywords internal
set_encoding <- function(model, mode, eng, options) {
  check_model_exists(model)
  check_eng_val(eng)
  check_mode_val(mode)
  check_encodings(options)

  keys   <- tibble::tibble(model = model, engine = eng, mode = mode)
  options <- tibble::as_tibble(options)
  new_values <- dplyr::bind_cols(keys, options)


  current_db_list <- ls(envir = get_model_env())
  nm <- paste(model, "encoding", sep = "_")
  if (any(current_db_list == nm)) {
    current <- get_from_env(nm)
    dup_check <-
      current %>%
      dplyr::inner_join(
        new_values,
        by = c("model", "engine", "mode", "predictor_indicators")
      )
    if (nrow(dup_check)) {
      rlang::abort(glue::glue("Engine '{eng}' and mode '{mode}' already have defined encodings for model '{model}'."))
    }

  } else {
    current <- NULL
  }

  db_values <- dplyr::bind_rows(current, new_values)
  set_env_val(nm, db_values)

  invisible(NULL)
}


#' @rdname set_new_model
#' @keywords internal
#' @export
get_encoding <- function(model) {
  check_model_exists(model)
  nm <- paste0(model, "_encoding")
  res <- try(get_from_env(nm), silent = TRUE)
  if (inherits(res, "try-error")) {
    # for objects made before encodings were specified in parsnip
    res <-
      get_from_env(model) %>%
      dplyr::mutate(
        model = model,
        predictor_indicators = "traditional",
        compute_intercept = TRUE,
        remove_intercept = TRUE,
        allow_sparse_x = FALSE
      ) %>%
      dplyr::select(model, engine, mode, predictor_indicators,
                    compute_intercept, remove_intercept)
  }
  res
}

#' Tools for dynamically documenting packages
#'
#' @description
#' These are functions used to create dynamic documentation in Rd files
#' based on which parsnip-related packages are loaded by the user.
#'
#' These functions can be used to make dynamic lists of documentation help
#'  files. \pkg{parsnip} uses these, along with files contained in `man/rmd`
#'  containing expanded documentation, for specific model/engine combinations.
#'  [find_engine_files()] looks for files that have the pattern
#'  `details_{model}_{engine}.Rd` to link to. These files are generated by files
#'  named `man/rmd/details_{model}_{engine}.Rmd`. `make_engine_list()` creates a
#'  list seen at the top of the model Rd files while `make_seealso_list()`
#'  populates the list seen in "See Also" below. See the details section.
#'
#' @param mod A character string for the model file (e.g. "linear_reg")
#' @return
#' `make_engine_list()` returns a character string that creates a
#' bulleted list of links to more specific help files.
#'
#' `make_seealso_list()` returns a formatted character string of links.
#'
#' `find_engine_files()` returns a tibble.
#' @details
#' The \pkg{parsnip} documentation is generated _dynamically_. Part of the Rd
#'  file populates a list of engines that depends on what packages are loaded
#'  *at the time that the man file is loaded*. For example, if
#'  another package has a new engine for `linear_reg()`, the
#'  `parsnip::linear_reg()` help can show a link to a detailed help page in the
#'  other package.
#'
#' To enable this, the process for a package developer is to:
#'
#'   1. Create an engine-specific R file in the `R` directory with the name
#'  `{model}_{engine}.R` (e.g. `boost_tree_C5.0.R`). This has a small amount of
#'  documentation, as well as the directive
#'  "`@includeRmd man/rmd/{model}_{engine}.Rmd details`".
#'
#'   1. Copy the file in \pkg{parsnip} that is in `man/rmd/setup.Rmd` and put
#'  it in the same place in your package.
#'
#'   1. Write your own `man/rmd/{model}_{engine}.Rmd` file. This can include
#'  packages that are not listed in the DESCRIPTION file. Those are only
#'  required when the documentation file is created locally (probably using
#'  [devtools::document()].
#'
#'   1. Run [devtools::document()] so that the Rmd content is included in the
#'  Rd file.
#'
#' The examples in \pkg{parsnip} can provide guidance for how to organize
#' technical information about the models.
#' @name doc-tools
#' @keywords internal
#' @export
#' @examples
#' find_engine_files("linear_reg")
#' cat(make_engine_list("linear_reg"))
find_engine_files <- function(mod) {

  # Get available topics
  topic_names <- search_for_engine_docs(mod)
  if (length(topic_names) == 0) {
    return(character(0))
  }

  # Subset for our model function
  eng <- strsplit(topic_names, "_")
  eng <- purrr::map_chr(eng, ~ .x[length(.x)])
  eng <- tibble::tibble(engine = eng, topic = topic_names)

  # Combine them to keep the order in which they were registered
  all_eng <- get_from_env(mod) %>% dplyr::distinct(engine)
  all_eng$.order <- 1:nrow(all_eng)
  eng <- dplyr::left_join(eng, all_eng, by = "engine")
  eng <- eng[order(eng$.order),]

  # Determine and label default engine
  default <- get_default_engine(mod)
  eng$default <- ifelse(eng$engine == default, " (default)", "")

  eng
}

#' @export
#' @rdname doc-tools
make_engine_list <- function(mod) {
  eng <- find_engine_files(mod)

  res <-
    glue::glue("  \\item \\code{\\link[=|eng$topic|]{|eng$engine|} |eng$default| }",
               .open = "|", .close = "|")

  res <- paste0("\\itemize{\n", paste0(res, collapse = "\n"), "\n}")
  res
}

get_default_engine <- function(mod) {
  cl <- rlang::call2(mod, .ns = "parsnip")
  rlang::eval_tidy(cl)$engine
}

#' @export
#' @rdname  doc-tools
make_seealso_list <- function(mod) {
  eng <- find_engine_files(mod)

  res <-
    glue::glue("\\code{\\link[=|eng$topic|]{|eng$engine| engine details}}",
               .open = "|", .close = "|")

  main <- c("\\code{\\link[=fit.model_spec]{fit.model_spec()}}",
            "\\code{\\link[=set_engine]{set_engine()}}",
            "\\code{\\link[=update]{update()}}"
            )
  paste0(c(main, res), collapse = ", ")
}

# These will never have documentation and we can avoid searching them.
excl_pkgs <-
  c("C50", "Cubist", "earth", "flexsurv", "forecast", "glmnet",
    "keras", "kernlab", "kknn", "klaR", "LiblineaR", "liquidSVM",
    "magrittr", "MASS", "mda", "mixOmics", "naivebayes", "nnet",
    "prophet", "pscl", "randomForest", "ranger", "rpart", "rstanarm",
    "sparklyr", "stats", "survival", "xgboost", "xrf")

search_for_engine_docs <- function(mod) {
  all_deps <- get_from_env(paste0(mod, "_pkgs"))
  all_deps <- unlist(all_deps$pkg)
  all_deps <- unique(c("parsnip", all_deps))

  all_deps <- all_deps[!(all_deps %in% excl_pkgs)]
  res <- purrr::map(all_deps, find_details_topics, mod = mod)
  res <- unique(unlist(res))
  res
}

find_details_topics <- function(pkg, mod) {
  meta_loc <- system.file("Meta/Rd.rds", package = pkg)
  meta_loc <- meta_loc[meta_loc != ""]
  if (length(meta_loc) > 0) {
    topic_names <- readRDS(meta_loc)$Name
    res <- grep(paste0("details_", mod), topic_names, value = TRUE)
  } else {
    res <- character(0)
  }
  res
}
