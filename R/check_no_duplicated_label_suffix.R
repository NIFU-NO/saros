check_no_duplicated_label_suffix <- function(
  data_summary,
  error_on_duplicates = TRUE
) {
  # Check if data_summary is a valid data frame
  if (!is.data.frame(data_summary)) {
    cli::cli_warn(
      "data_summary is not a data frame, skipping duplicate label check"
    )
    return()
  }

  # Check if required column exists
  if (!".variable_label" %in% colnames(data_summary)) {
    cli::cli_warn(
      "Column '.variable_label' not found in data_summary, skipping duplicate label check"
    )
    return()
  }

  # Check if data_summary is empty
  if (nrow(data_summary) == 0) {
    cli::cli_warn(
      "data_summary is empty (0 rows), skipping duplicate label check"
    )
    return()
  }

  duplicates <-
    data_summary |>
    dplyr::grouped_df(vars = c(".variable_label")) |>
    dplyr::filter(
      dplyr::n_distinct(.data[[".variable_name"]], na.rm = FALSE) > 1
    ) |>
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
