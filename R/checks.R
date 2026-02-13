err_msg <- function(infix) {
  stringi::stri_c(
    ignore_null = TRUE,
    "{.arg {arg}} must be a",
    infix,
    ", not {.obj_type_friendly {x}}."
  )
}

# Validation Functions ----
# All validation functions use validate_* prefix for consistency and discoverability

#' Validate boolean parameter
#' @keywords internal
validate_bool <- function(
  x,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x)
) {
  if (!(is.logical(x) && length(x) == 1 && !is.na(x))) {
    cli::cli_abort(err_msg(" logical of length 1"), call = call)
  }
}

#' Validate integerish parameter
#' @keywords internal
validate_integerish <- function(
  x,
  min = -Inf,
  max = Inf,
  null_allowed = FALSE,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x)
) {
  if (isTRUE(null_allowed) && is.null(x)) {
    return()
  }
  pos_str <- if (min == 0) " positive" else "n"
  max_str <- if (!max == Inf) {
    stringi::stri_c(ignore_null = TRUE, " (max=", max, ")")
  } else {
    ""
  }
  if (!rlang::is_integerish(x, n = 1) || x > max || x < min) {
    cli::cli_abort(
      err_msg("{pos_str} integer of length 1{max_str}"),
      call = call
    )
    c(pos_str, max_str)
  }
}

#' Validate double/numeric parameter
#' @keywords internal
validate_double <- function(
  x,
  min = -Inf,
  max = Inf,
  null_allowed = FALSE,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x)
) {
  if (isTRUE(null_allowed) && is.null(x)) {
    return()
  }
  pos_str <- if (min == 0) " positive" else ""

  max_str <- if (!max == Inf) {
    stringi::stri_c(ignore_null = TRUE, " (max=", max, ")")
  } else {
    ""
  }

  min_str <- if (!min == -Inf) {
    stringi::stri_c(ignore_null = TRUE, " (min=", min, ")")
  } else {
    ""
  }

  if (!rlang::is_double(x, n = 1) || x < min || x > max) {
    cli::cli_abort(
      err_msg("{pos_str} numeric of length 1{max_str}{min_str}"),
      call = call
    )
    c(pos_str, max_str, min_str)
  }
}

#' Validate string parameter
#' @keywords internal
validate_string <- function(
  x,
  null_allowed = FALSE,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x)
) {
  if (isTRUE(null_allowed) && is.null(x)) {
    return()
  }
  if (!rlang::is_string(x)) {
    cli::cli_abort(err_msg(" string (character of length 1)"), call = call)
  }
}

# Backwards compatibility aliases ----
check_bool <- validate_bool
check_integerish <- validate_integerish
check_double <- validate_double
check_string <- validate_string

# Validation Rule Builders (for declarative validation) ----

#' Create integerish validation rule
#' @keywords internal
validate_integerish_rule <- function(min = -Inf, max = Inf, null_ok = FALSE) {
  function(x) {
    if (null_ok && is.null(x)) return(TRUE)
    rlang::is_integerish(x, n = 1, finite = TRUE) && 
      x >= min && x <= max
  }
}

#' Create double validation rule
#' @keywords internal
validate_double_rule <- function(min = -Inf, max = Inf, null_ok = FALSE) {
  function(x) {
    if (null_ok && is.null(x)) return(TRUE)
    is.numeric(x) && length(x) == 1 && is.finite(x) && 
      x >= min && x <= max
  }
}

#' Create boolean validation rule
#' @keywords internal
validate_bool_rule <- function() {
  function(x) {
    is.logical(x) && length(x) == 1 && !is.na(x)
  }
}

#' Create string validation rule
#' @keywords internal
validate_string_rule <- function(null_ok = FALSE) {
  function(x) {
    if (null_ok && is.null(x)) return(TRUE)
    rlang::is_string(x)
  }
}

# Batch Validation Helper ----

#' Validate multiple parameters at once
#' 
#' @param params Named list of parameter values to validate
#' @param spec Named list of validation specifications. Each element should have:
#'   - type: one of "integerish", "double", "bool", "string"
#'   - min, max: optional for numeric types
#'   - null_allowed: optional boolean (default FALSE)
#' @param call Calling environment for error messages
#' 
#' @keywords internal
validate_params <- function(params, spec, call = rlang::caller_env()) {
  for (param_name in names(spec)) {
    if (!param_name %in% names(params)) next
    
    val <- params[[param_name]]
    rules <- spec[[param_name]]
    
    switch(rules$type,
      "integerish" = validate_integerish(
        val,
        min = rules$min %||% -Inf,
        max = rules$max %||% Inf,
        null_allowed = rules$null_allowed %||% FALSE,
        call = call,
        arg = param_name
      ),
      "bool" = validate_bool(
        val,
        call = call,
        arg = param_name
      ),
      "double" = validate_double(
        val,
        min = rules$min %||% -Inf,
        max = rules$max %||% Inf,
        null_allowed = rules$null_allowed %||% FALSE,
        call = call,
        arg = param_name
      ),
      "string" = validate_string(
        val,
        null_allowed = rules$null_allowed %||% FALSE,
        call = call,
        arg = param_name
      ),
      cli::cli_abort("Unknown validation type: {rules$type}", call = call)
    )
  }
  invisible(TRUE)
}

# Other validation functions ----

#' @keywords internal
validate_multiple_dep_and_one_indep <-
  function(data, dep, indep, call = rlang::caller_env()) {
    dep_call <- rlang::expr_deparse(rlang::enquo(dep))
    indep_call <- rlang::expr_deparse(rlang::enquo(indep))
    if (
      (ncol(dplyr::select(.data = data, {{ dep }})) > 1L &&
        ncol(dplyr::select(.data = data, {{ indep }})) >= 1L)
    ) {
      cli::cli_abort(
        c(
          "Multiple columns for {.arg dep} and {.arg indep} are not allowed.",
          i = "You provided dep = {dep_call} and indep = {indep_call}"
        ),
        call = call
      )
      dep_call
      indep_call
    }
  }

# Backwards compatibility alias
check_multiple_dep_and_one_indep <- validate_multiple_dep_and_one_indep

#' @keywords internal
validate_summary_data_cols <- function(
  x,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x)
) {
  missing_cols <-
    .saros.env$summary_data_sort2[
      !.saros.env$summary_data_sort2 %in% colnames(x)
    ]
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c("{.arg {arg}} is missing columns {.var {missing_cols}}."),
      call = call
    )
  }
}

# Additional palette-specific validation functions ----

check_palette_codes <- function(
  x,
  null_allowed = TRUE,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x)
) {
  if (isTRUE(null_allowed) && is.null(x)) {
    return()
  }
  if (
    !((rlang::is_list(x) &&
      all(vapply(x, is.character, logical(1)))) ||
      (isTRUE(null_allowed) && is.null(x)))
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be NULL or a list of character vectors.",
      call = call
    )
  }
}

check_priority_palette_codes <- function(
  x,
  null_allowed = TRUE,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x)
) {
  if (isTRUE(null_allowed) && is.null(x)) {
    return()
  }
  if (!(rlang::is_character(x) || (isTRUE(null_allowed) && is.null(x)))) {
    cli::cli_abort(
      "{.arg {arg}} must be a character vector (possibly named) or NULL.",
      call = call
    )
  }
}

#' Validate Palette Parameters
#'
#' Validates palette-related parameters used by `girafe()` and `ggsaver()`.
#' Ensures type safety and provides clear error messages for invalid inputs.
#'
#' @param palette_codes Optional list of named character vectors
#' @param priority_palette_codes Optional character vector
#' @param label_wrap_width Integer for legend label wrapping
#' @param ncol Optional integer for legend columns
#' @param byrow Logical for legend key arrangement
#' @param call Calling environment for error messages
#'
#' @return NULL (called for side effects - validation)
#' @keywords internal
validate_palette_params <- function(
  palette_codes = NULL,
  priority_palette_codes = NULL,
  label_wrap_width = NULL,
  ncol = NULL,
  byrow = NULL,
  call = rlang::caller_env()
) {
  if (!is.null(label_wrap_width)) {
    check_integerish(label_wrap_width, min = 1, call = call)
  }
  if (!is.null(ncol)) {
    check_integerish(ncol, min = 1, null_allowed = TRUE, call = call)
  }
  if (!is.null(byrow)) {
    check_bool(byrow, call = call)
  }
  if (!is.null(palette_codes)) {
    check_palette_codes(palette_codes, call = call)
  }
  if (!is.null(priority_palette_codes)) {
    check_priority_palette_codes(priority_palette_codes, call = call)
  }
  invisible()
}

# Backwards compatibility alias
check_summary_data_cols <- validate_summary_data_cols
