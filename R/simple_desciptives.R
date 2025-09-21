simple_descriptives_int_table <- function(
  data,
  y_var,
  x_var = NULL,
  na.rm = TRUE
) {
  dplyr::summarize(
    data,
    dplyr::across(
      tidyselect::all_of(y_var),
      list(
        mean = ~ mean(.x, na.rm = na.rm),
        sd = ~ sd(.x, na.rm = na.rm),
        median = ~ stats::median(.x, na.rm = na.rm),
        mad = ~ stats::mad(.x, na.rm = na.rm),
        iqr = ~ stats::IQR(.x, na.rm = na.rm),
        n_valid = ~ sum(!is.na(.x)),
        n_miss = ~ sum(is.na(.x)),
        n = ~ dplyr::n(),
        min = ~ min(.x, na.rm = na.rm),
        max = ~ max(.x, na.rm = na.rm)
      ),
      .names = "{.fn}"
    ),
    .by = tidyselect::all_of(x_var)
  )
}
simplest_descriptives_table <- function(data, y_var, x_var = NULL) {
  dplyr::summarize(
    data,
    dplyr::across(
      tidyselect::all_of(y_var),
      list(
        n_valid = ~ sum(!is.na(as.character(.x)), na.rm = TRUE),
        n = ~ dplyr::n()
      ),
      .names = "{.fn}"
    ),
    .by = tidyselect::all_of(x_var)
  )
}

simple_descriptives <- function(
  data,
  y_var,
  x_var = NULL,
  na.rm = TRUE,
  max_k = 5,
  table_wide = FALSE,
  label_separator = NULL
) {
  if (length(x_var) > 1) {
    rlang::abort("`x_var` must currently be of length 1 or `NULL`.")
  }

  out <-
    y_var |>
    rlang::set_names() |>
    lapply(function(yvar) {
      is_integerish <- is.numeric(data[[yvar]]) || is.ordered(data[[yvar]])
      yvar_label <- get_raw_labels(data, col_pos = yvar)
      yvar_label_prefix <- get_main_question(
        yvar_label,
        label_separator = label_separator
      )
      yvar_label <-
        keep_subitem(
          yvar_label,
          label_separator = label_separator,
          ordered = FALSE
        )
      yvar_variable_position <- which(names(data) == yvar)
      if (is.ordered(data[[yvar]])) {
        data[[yvar]] <- as.numeric(data[[yvar]])
      }
      if (
        rlang::is_string(x_var) &&
          (length(unique(data[[x_var]])) > max_k || yvar == x_var)
      ) {
        x_var <- NULL
      }

      if (isTRUE(na.rm) && rlang::is_string(x_var) && x_var %in% names(data)) {
        data <- data[!is.na(data[[x_var]]), , drop = FALSE]
      }

      if (is_integerish) {
        out <- simple_descriptives_int_table(
          data,
          y_var = yvar,
          x_var = x_var,
          na.rm = na.rm
        )
      } else {
        out <- simplest_descriptives_table(
          data,
          y_var = yvar,
          x_var = x_var
        )
      }
      out$.variable_position <- yvar_variable_position
      out$.variable_label <- yvar_label
      out$.variable_label_prefix <- yvar_label_prefix
      out
    }) |>
    dplyr::bind_rows(.id = ".variable_name")

  if (!rlang::is_string(x_var) || isFALSE(table_wide)) {
    return(out)
  }
  return(
    tidyr::pivot_wider(
      out,
      names_from = tidyselect::all_of(x_var),
      values_from = -tidyselect::all_of(x_var)
    )
  )
}
