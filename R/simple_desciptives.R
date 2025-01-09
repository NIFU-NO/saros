simple_descriptives_int_table <- function(data, y_var, x_var = NULL, na.rm = TRUE) {
  dplyr::summarize(data,
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


simple_descriptives <- function(data, y_var, x_var = NULL, na.rm = TRUE, max_k = 5,
                                table_wide = FALSE) {
  if ((is.numeric(data[[y_var]]) || is.ordered(data[[y_var]])) &&
    (is.null(x_var) || length(unique(data[[x_var]])) <= max_k)) {
    if (isTRUE(na.rm) && rlang::is_string(x_var)) data <- data[!is.na(data[[x_var]]), , drop = FALSE]

    data[[y_var]] <- as.numeric(data[[y_var]]) # Ensure ordered variables can be handled

    out <- simple_descriptives_int_table(data, y_var = y_var, x_var = x_var, na.rm = na.rm)

    if (rlang::is_string(x_var) && isTRUE(table_wide)) {
      return(
        tidyr::pivot_wider(out,
          names_from = tidyselect::all_of(x_var),
          values_from = -tidyselect::all_of(x_var)
        )
      )
    } else {
      return(out)
    }
  } else {
    if (isTRUE(na.rm) && rlang::is_string(x_var)) data <- data[!is.na(data[[x_var]]), , drop = FALSE]

    out <-
      data |>
      dplyr::summarize(
        dplyr::across(tidyselect::all_of(y_var),
          list(
            n_valid = ~ sum(!is.na(as.character(.x)), na.rm = TRUE),
            n = ~ dplyr::n()
          ),
          .names = "{.fn}"
        ),
        .by = tidyselect::all_of(x_var)
      )
    if (rlang::is_string(x_var) && isTRUE(table_wide)) {
      return(tidyr::pivot_wider(out,
        names_from = tidyselect::all_of(x_var),
        values_from = -tidyselect::all_of(x_var)
      ))
    } else {
      return(out)
    }
  }
}
