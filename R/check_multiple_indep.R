check_multiple_indep <-
  function(data, indep, call = rlang::caller_env()) {
    indep_call <- rlang::expr_deparse(rlang::enquo(indep))

    if (ncol(dplyr::select(.data = data, {{ indep }})) > 1L) {
      cli::cli_abort(
        c(
          "Too many columns provided for {.arg indep}.",
          x = "Only 1 indep-column is currently allowed.",
          i = "You provided indep = {indep_call}"
        ),
        call = call
      )
    }
  }
