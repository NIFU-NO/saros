err_msg <- function(infix) {
  stringi::stri_c(ignore_null=TRUE, "{.arg {arg}} must be a",
                  infix,
                  ", not {.obj_type_friendly {x}}.")
}


check_bool <- function(x, call = rlang::caller_env(),
                       arg = rlang::caller_arg(x)) {
  if(!(is.logical(x) && length(x) == 1 && !is.na(x)) || is.na(x)) {
    cli::cli_abort(err_msg(" logical of length 1"),
                   call = call)
  }
}


check_integerish <- function(x, min=-Inf, max=Inf,
                             null_allowed = FALSE,
                             call = rlang::caller_env(),
                             arg = rlang::caller_arg(x)) {
  if(isTRUE(null_allowed) && is.null(x)) return()
  pos_str <- if(min==0) " positive" else "n"
  max_str <- if(!max==Inf) stringi::stri_c(ignore_null=TRUE, " (max=", max, ")") else ""
  if(!rlang::is_integerish(x, n = 1) || x > max || x < min) {
    cli::cli_abort(err_msg("{pos_str} integer of length 1{max_str}"),
                   call = call)
  }
}

check_double <- function(x, min=-Inf, max=Inf, null_allowed = FALSE,
                         call = rlang::caller_env(),
                         arg = rlang::caller_arg(x)) {
  if(isTRUE(null_allowed) && is.null(x)) return()
  pos_str <- if(min==0) " positive" else ""
  max_str <- if(!max==Inf) stringi::stri_c(ignore_null=TRUE, " (max=", max, ")") else ""
  min_str <- if(!min==-Inf) stringi::stri_c(ignore_null=TRUE, " (min=", min, ")") else ""
  if(!rlang::is_double(x, n = 1) || x < min || x > max) {
    cli::cli_abort(err_msg("{pos_str} numeric of length 1{max_str}{min_str}"),
                   call = call)
  }
}


check_multiple_dep_and_one_indep <-
  function(data, dep, indep,
           call = rlang::caller_env()) {

    dep_call <- rlang::expr_deparse(rlang::enquo(dep))
    indep_call <- rlang::expr_deparse(rlang::enquo(indep))

    if((ncol(dplyr::select(.data = data, {{dep}})) > 1L &&
        ncol(dplyr::select(.data = data, {{indep}})) >= 1L)) {
      cli::cli_abort(c("Multiple columns for {.arg dep} and {.arg indep} are not allowed.",
                       i="You provided dep = {dep_call} and indep = {indep_call}"),
                     call = call)
    }
  }




check_summary_data_cols <- function(x, call = rlang::caller_env(),
                                    arg = rlang::caller_arg(x)) {
  missing_cols <-
    .saros.env$summary_data_sort2[
      !.saros.env$summary_data_sort2 %in% colnames(x)]
  if(length(missing_cols)>0) {
    cli::cli_abort(c("{.arg {arg}} is missing columns {.var {missing_cols}}."),
                   call = call)
  }
}
