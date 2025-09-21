check_multiple_dep_and_one_indep <-
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
    }
  }
