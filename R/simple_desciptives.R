simple_descriptives <- function(data, y_var, x_var = NULL, na.rm=FALSE, max_k = 5,
                                table_wide = FALSE) {

  if((is.numeric(data[[y_var]]) || is.ordered(data[[y_var]])) &&
     (is.null(x_var) || length(unique(data[[x_var]])) <= max_k)) {

    if(isTRUE(na.rm) && rlang::is_string(x_var)) data <- data[!is.na(data[[x_var]]), , drop=FALSE]

    out <-
      data |>
      dplyr::summarize(dplyr::across(tidyselect::all_of(y_var),
                                     list(mean = ~mean(.x, na.rm=TRUE),
                                          sd = ~sd(.x, na.rm=TRUE),
                                          median = ~median(.x, na.rm=TRUE),
                                          mad = ~mad(.x, na.rm=TRUE),
                                          n_valid = ~sum(!is.na(.x), na.rm = TRUE),
                                          n = ~dplyr::n()
                                          ),
                                     .names = "{.fn}"),
                       .by = tidyselect::all_of(x_var))
    if(rlang::is_string(x_var) && isTRUE(table_wide)) {
      return(
      tidyr::pivot_wider(out, names_from = tidyselect::all_of(x_var),
                         values_from = -tidyselect::all_of(x_var))
      )
    } else return(out)

  } else {

    if(isTRUE(na.rm) && rlang::is_string(x_var)) data <- data[!is.na(data[[x_var]]), , drop=FALSE]

    out <-
      data |>
      dplyr::summarize(dplyr::across(tidyselect::all_of(y_var),
                                     list(n_valid = ~sum(!is.na(as.character(.x)), na.rm = TRUE),
                                          n = ~dplyr::n()
                                     ),
                                     .names = "{.fn}"),
                       .by = tidyselect::all_of(x_var))
    if(rlang::is_string(x_var) && isTRUE(table_wide)) {

        return(tidyr::pivot_wider(out,
                           names_from = tidyselect::all_of(x_var),
                           values_from = -tidyselect::all_of(x_var))
        )
    } else return(out)
  }
}
