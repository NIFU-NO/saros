check_no_duplicated_label_suffix <- function(data_summary, error_on_duplicates = TRUE) {
    duplicates <-
        data_summary |>
        dplyr::grouped_df(vars = c(".variable_label")) |>
        dplyr::filter(dplyr::n_distinct(.data[[".variable_name"]], na.rm = FALSE) > 1) |>
        dplyr::ungroup()
    if (nrow(duplicates) > 0 && !all(is.na(duplicates[[".variable_label"]]))) {
        msg <- "Found duplicated variable labels: {unique(duplicates[['.variable_label']])}"
        if (isTRUE(error_on_duplicates)) {
            cli::cli_abort(msg)
        } else {
            cli::cli_warn(msg)
        }
    }
}
