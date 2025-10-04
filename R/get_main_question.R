# Helper function to extract main question from the data
get_main_question <-
  function(
    x,
    label_separator,
    warn_multiple = TRUE,
    call = rlang::caller_env()
  ) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return("")
    }
    if (!rlang::is_string(label_separator) && length(x) == 1) {
      return(x)
    }

    if (!(is.character(x) || is.factor(x) || is.ordered(x))) {
      cli::cli_abort(
        c(
          x = "{.arg x} must be of type {.cls character} or {.cls factor}.",
          i = "not {.obj_type_friendly {x}}."
        ),
        call = call
      )
    }

    x <-
      stringi::stri_replace(
        str = x,
        regex = stringi::stri_c(
          ignore_null = TRUE,
          "^(.*)",
          label_separator,
          "(.*)$"
        ),
        replacement = "$1",
        dot_all = TRUE
      ) |>
      unique()
    if (length(x) > 1L && isTRUE(warn_multiple)) {
      cli::cli_warn(
        c(
          x = "There are multiple main questions for these variables.",
          i = "Check your data."
        ),
        call = call
      )
    } else if (length(x) == 1 && nchar(x) == 0L) {
      cli::cli_warn(
        c(
          x = "No main question found.",
          i = "Check your {.arg label_separator} ({.val {label_separator}})."
        ),
        call = call
      )
    }
    x <- if (length(x) > 0) {
      stringi::stri_c(ignore_null = TRUE, x, collapse = "\n")
    }
    x
  }
