err_msg <- function(infix) {
  paste0("{.arg {arg}} must be a",
         infix,
         ", not {.obj_type_friendly {x}}.")
}

check_bool <- function(x, call = rlang::caller_env(),
                       arg = rlang::caller_arg(x)) {
  if(!rlang::is_bool(x) || is.na(x)) {
    cli::cli_abort(err_msg(" logical of length 1"),
                   call = call)
  }
}

check_pval <- function(x, n = 1,
                       call = rlang::caller_env(),
                       arg = rlang::caller_arg(x)) {
  if(!(rlang::is_double(x = x, n = n, finite = TRUE) &&
       all(x >= 0) && all(x <= 1))) {
    cli::cli_abort(err_msg(" numeric between 0 and 1"))
  }
  }


check_integerish <- function(x, min=-Inf, max=Inf,
                             call = rlang::caller_env(),
                             arg = rlang::caller_arg(x)) {
  pos_str <- if(min==0) " positive" else "n"
  max_str <- if(!max==Inf) paste0(" (max=", max, ")") else ""
  if(!rlang::is_integerish(x, n = 1) || x > max || x < min) {
    cli::cli_abort(err_msg("{pos_str} integer of length 1{max_str}"),
                   call = call)
  }
}

check_double <- function(x, min=-Inf, max=Inf, call = rlang::caller_env(),
                         arg = rlang::caller_arg(x)) {
  pos_str <- if(min==0) " positive" else ""
  max_str <- if(!max==Inf) paste0(" (max=", max, ")") else ""
  min_str <- if(!min==-Inf) paste0(" (min=", min, ")") else ""
  if(!rlang::is_double(x, n = 1) || x < min || x > max) {
    cli::cli_abort(err_msg("{pos_str} numeric of length 1{max_str}{min_str}"),
                   call = call)
  }
}


check_string <- function(x, null.ok = FALSE, n = 1,
                         call = rlang::caller_env(),
                         arg = rlang::caller_arg(x)) {
  msg_suffix <- err_msg(paste0(" character vector",
                        if(!is.null(n)) " of length ",
                        if(!is.null(n)) n))
  if(is.null(x)) {
    if(!null.ok) {
      cli::cli_abort(message = msg_suffix, call = call)
    }
  } else if (!rlang::is_character(x, n = n)) {
    msg_prefix <- if(null.ok) "If not NULL, " else ""
    msg <- paste0(msg_prefix, msg_suffix)
    cli::cli_abort(message = msg, call = call)
  }
}



check_list <- function(x,
                       null.ok = TRUE,
                       n = NULL,
                       call = rlang::caller_env(),
                       arg = rlang::caller_arg(x)) {
  msg_suffix <- err_msg(paste0(" list",
                               if(!is.null(n)) " of length ",
                               if(!is.null(n)) n))
  if(is.null(x)) {
    if(!null.ok) {
      cli::cli_abort(message = msg_suffix, call = call)
    }
  } else if (!rlang::is_list(x, n = n)) {
    msg_prefix <- if(null.ok) "If not NULL, " else ""
    msg <- paste0(msg_prefix, msg_suffix)
    cli::cli_abort(message = msg, call = call)
  }
}


check_colour <- function(x, call = rlang::caller_env(),
                         arg = rlang::caller_arg(x)) {
  if(!is.null(x) && (!all(is_colour(x)) || length(x)>1)) {
    cli::cli_abort(paste0("If not NULL, ", err_msg(" character (hex colour code) of length 1")),
                   call = call)
  }
}

check_colours <- function(x, call = rlang::caller_env(),
                          arg = rlang::caller_arg(x)) {
  if(!is.null(x) && !all(is_colour(x))) {
    cli::cli_abort(err_msg(" character vector (hex colours)"),
                   call = call)
  }
}

# check_colour <- function(x, single = FALSE, call = rlang::caller_env()) {
#   if(!is.null(x) && !all(is_colour(x))) {
#     cli::cli_abort(c("{.arg {deparse(substitute(x))}} must be a character vector (hex colours), not {.obj_type_friendly {x}}."),
#                    call = call)
#   }
#   if(single && length(x) > 1) {
#     cli::cli_abort(c("If not NULL, {.arg {deparse(substitute(x))}} must be a character (hex colour code) of length 1, not {.obj_type_friendly {x}}."),
#                    call = call)
#   }
# }


check_data_frame <- function(x, n=NULL, call = rlang::caller_env(),
                             arg = rlang::caller_arg(x)) {
  if(!inherits(x, "data.frame")) {
    cli::cli_abort(err_msg(" data.frame or tibble"),
                   call = call)
  }
  if(!is.null(n) && (ncol(x) == 0 || nrow(x) == 0)) {
    cli::cli_abort("{.arg {x} is an empty data frame.")
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


check_autonum <- function(x, call = rlang::caller_env(),
                          arg = rlang::caller_arg(x)) {
  if(!is.null(x) && !inherits(x, "run_autonum")) {
    cli::cli_abort(paste0("If not NULL, ", err_msg("n object from {.fun officer::run_autonum}")),
                   call = call)
  }
}



check_multiple_by <-
  function(data, by,
           call = rlang::caller_env()) {

    by_call <- rlang::expr_deparse(rlang::enquo(by))

    if(ncol(dplyr::select(.data = data, {{by}})) > 1L) {
      cli::cli_abort(c("Too many columns provided for {.arg by}.",
                       x = "Only 1 by-column is currently allowed.",
                       i = "You provided by = {by_call}"), call = call)
    }
  }


check_multiple_cols_and_one_by <-
  function(data, cols, by,
           call = rlang::caller_env()) {

    cols_call <- rlang::expr_deparse(rlang::enquo(cols))
    by_call <- rlang::expr_deparse(rlang::enquo(by))

    if((ncol(dplyr::select(.data = data, {{cols}})) > 1L &&
       ncol(dplyr::select(.data = data, {{by}})) >= 1L)) {
      cli::cli_abort(c("Multiple columns for {.arg cols} and {.arg by} are not allowed.",
                       i="You provided cols = {cols_call} and by = {by_call}"),
                     call = call)
    }
  }


check_sort_by <- function(x, sort_by = NULL,
                          call = rlang::caller_env()) {

  set_options <- c(.saros.env$summary_data_sort1,
                   .saros.env$summary_data_sort2)
  categories_in_data <- as.character(x)

  if(!is.null(sort_by)) {
    if(
      (rlang::is_character(x = sort_by, n = 1) &&
       sort_by %in% set_options) ||
      (rlang::is_character(sort_by) &&
       all(sort_by %in% categories_in_data)) ) {
      return()
    }
    cli::cli_abort(c(x="{.arg sort_by} must be either a single string from the set options {.var {set_options}} or all valid categories in the data frame.",
                     i="You supplied {.var {sort_by}}."),
                   call = call)
  }
}

check_element_name <-
  function(x, n = 1, null.ok = FALSE, call = rlang::caller_env(), arg = rlang::caller_arg(x)) {

    check_string(x = x, n = n, null.ok = null.ok, call = call, arg = arg)
    all(x %in% list_available_element_types())
    }

check_elements <-
  function(x, call = rlang::caller_env()) {

    check_list(x=x, null.ok = TRUE, call = call)
    if(!rlang::is_named(x=x)) cli::cli_abort("{.arg {x}} must be named.")
    if(!all(names(x) %in% list_available_element_types())) {
      cli::cli_abort(c("{.arg elements} must only contain the following names: {.var {list_available_element_types()}}.",
                     "Invalid elements: {names(x)[!names(x) %in% list_available_element_types()]}"))
    }
    purrr::walk(x, ~{
      if(!rlang::is_null(x) &&
         !rlang::is_bool(x) &&
         !(rlang::is_named(x) && (rlang::is_character(x) || rlang::is_bare_list(x)))) {
        cli::cli_abort(c("The value of {.arg {x[.x]}} must be either",
                         x="a) NULL,",
                         x="b) boolean (TRUE or FALSE, not NA),",
                         x="c) a named character of paths, or",
                         x="d) a named list of objects having the same class,",
                         x="not {.obj_type_friendly {x}}."))
    }
    })
  }

check_yml <-
  function(x, call = rlang::caller_env()) {
    if(!is.null(x) && !ymlthis::is_yml(x)) {
      cli::cli_abort(c(paste0("If not NULL, ", err_msg(" yml-object from {.fun ymlthis::yml()}")),
                       i = "See {.url https://cran.r-project.org/web/packages/ymlthis/vignettes/introduction-to-ymlthis.html}."))
    }
  }

check_existing_path <-
  function(x, call = rlang::caller_env()) {
    if(!fs::is_file(x, follow = TRUE)) {
      cli::cli_abort("{.arg {x}} is not an existing file path.")
    }
  }
