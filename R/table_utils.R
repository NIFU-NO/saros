# Table Utility Functions
# Common helper functions used across make_content table methods

#' Setup table data from dots
#'
#' Common setup logic for table functions including data extraction and early return
#'
#' @param dots List from rlang::list2(...)
#' @return List with data and should_return flag
#' @keywords internal
setup_table_data <- function(dots) {
  out <- dots$data_summary

  if (nrow(out) == 0) {
    return(list(data = data.frame(), should_return = TRUE))
  }

  list(data = out, should_return = FALSE)
}

#' Process categorical data for showNA settings
#'
#' Handle NA categories based on showNA parameter for categorical tables
#'
#' @param data Data frame with .category column
#' @param dots List with showNA and indep settings
#' @return Processed data frame
#' @keywords internal
process_categorical_na <- function(data, dots) {
  # Handle empty category levels
  levels(data[[".category"]])[
    levels(data[[".category"]]) == ""
  ] <- NA_character_

  if (dots$showNA == "never") {
    # Remove NA values
    data[[".category"]] <- forcats::fct_na_level_to_value(data[[".category"]])
    data <- data[!is.na(data[[".category"]]), , drop = FALSE]

    for (ind in dots$indep) {
      data[[ind]] <- forcats::fct_na_level_to_value(data[[ind]])
      data <- data[!is.na(data[[ind]]), , drop = FALSE]
    }
  } else {
    # Convert NA values to "NA" level
    data[[".category"]] <- forcats::fct_na_value_to_level(
      data[[".category"]],
      level = "NA"
    )
    for (ind in dots$indep) {
      data[[ind]] <- forcats::fct_na_value_to_level(data[[ind]], level = "NA")
    }
  }

  data
}

#' Get independent variable labels
#'
#' Process independent variable labels with consistent logic across table functions
#'
#' @param dots List from rlang::list2(...)
#' @return Character vector of processed labels
#' @keywords internal
get_indep_labels <- function(dots) {
  if (length(dots$indep) == 0) {
    return(character())
  }

  indep_label <- get_raw_labels(data = dots$data, col_pos = dots$indep)
  indep_label <- get_main_question(
    indep_label,
    label_separator = dots$label_separator,
    warn_multiple = TRUE
  )
  indep_label <- unique(indep_label)

  if (length(indep_label) > 0 && nchar(indep_label[1]) == 0) {
    indep_label <- dots$indep[1]
  }

  indep_label
}

#' Determine variable column basis
#'
#' Consistent logic for determining whether to use .variable_label or .variable_name
#'
#' @param data_summary Data frame with variable information
#' @return String indicating column to use as basis
#' @keywords internal
determine_variable_basis <- function(data_summary) {
  if (all(!is.na(data_summary[[".variable_label"]]))) {
    ".variable_label"
  } else {
    cli::cli_warn(
      "No variable labels found for {.var {sort(unique(data_summary[['.variable_name']]))}}. Using variable names."
    )
    ".variable_name"
  }
}

#' Process main question and extract suffixes
#'
#' Handle label separation and suffix extraction for table functions
#'
#' @param data Data frame to process
#' @param dots List from rlang::list2(...)
#' @param col_basis Current column basis (.variable_label or .variable_name)
#' @return List with processed data, main_question, and updated col_basis
#' @keywords internal
process_main_question_and_suffixes <- function(data, dots, col_basis) {
  main_question <- ""

  if (
    !is.null(dots$label_separator) &&
      nchar(dots$label_separator) > 0 &&
      col_basis == ".variable_label"
  ) {
    # Extract main question
    main_question <- if (
      !is.null(dots$main_question) && nchar(dots$main_question) > 0
    ) {
      dots$main_question
    } else if (".variable_label_prefix" %in% colnames(data)) {
      as.character(unique(data[[".variable_label_prefix"]]))[1]
    } else {
      ""
    }

    # Extract suffixes if we have a main question
    if (nchar(main_question) > 0) {
      data[[".variable_label_suffix"]] <- keep_subitem(
        fct = data[[".variable_label"]],
        label_separator = dots$label_separator,
        ordered = is.ordered(data[[".variable_label"]]),
        call = rlang::caller_env()
      )
      col_basis <- ".variable_label_suffix"
    }
  }

  list(
    data = data,
    main_question = main_question,
    col_basis = col_basis
  )
}

#' Get standard column renaming function
#'
#' Standardized column renaming logic for table functions
#'
#' @param main_question Main question for header
#' @param use_header Whether to use main question as header
#' @param column_mappings Named list of additional column mappings
#' @return Function for renaming columns
#' @keywords internal
get_standard_column_renamer <- function(
  main_question = "",
  use_header = FALSE,
  column_mappings = NULL
) {
  function(col) {
    # Handle variable label columns
    if (
      col %in% c(".variable_label", ".variable_label_suffix", ".variable_name")
    ) {
      return(
        if (use_header && nchar(main_question) > 0) {
          main_question
        } else {
          ".variable_label"
        }
      )
    }

    # Standard statistical columns
    standard_mappings <- c(
      "n" = "N",
      "n_valid" = "N_valid",
      "n_miss" = "N_missing",
      ".count" = "N",
      ".mean" = "Mean",
      ".mean_se" = "Mean_SE",
      ".median" = "Median",
      "mean" = "Mean",
      "sd" = "SD",
      "median" = "Median",
      "mad" = "MAD",
      "iqr" = "IQR",
      "min" = "Min",
      "max" = "Max"
    )

    # Combine with custom mappings if provided
    all_mappings <- c(standard_mappings, column_mappings)

    # Return mapped name if it exists, otherwise return original
    if (col %in% names(all_mappings)) {
      all_mappings[[col]]
    } else {
      col
    }
  }
}

#' Round numeric statistics
#'
#' Apply rounding to numeric statistical columns
#'
#' @param data Data frame to process
#' @param digits Number of decimal places
#' @return Data frame with rounded numeric columns
#' @keywords internal
round_numeric_stats <- function(data, digits) {
  numeric_cols <- c(
    # Original column names
    ".mean",
    ".mean_se",
    ".median",
    "mean",
    "sd",
    "median",
    "mad",
    "iqr",
    "min",
    "max",
    # Renamed column names
    "Mean",
    "Mean_SE",
    "Median",
    "SD",
    "MAD",
    "IQR",
    "Min",
    "Max"
  )

  for (col in intersect(numeric_cols, colnames(data))) {
    data[[col]] <- round(data[[col]], digits = digits)
  }

  data
}

#' Arrange output data by prespecified orders
#'
#' Standard data arrangement for table functions
#'
#' @param data Data frame to arrange
#' @param col_basis Column to use as primary sort
#' @param indep_vars Independent variable columns
#' @return Arranged data frame
#' @keywords internal
arrange_table_data <- function(data, col_basis, indep_vars = NULL) {
  # Use explicit order columns if available (new centralized sorting)
  if (".dep_order" %in% names(data)) {
    # Primary sort by dependent variable order, then category order
    # Include independent variable order if available
    data |>
      dplyr::arrange(
        .data$.dep_order,
        if (!is.null(data$.indep_order)) .data$.indep_order,
        if (!is.null(data$.category_order)) .data$.category_order
      )
  } else {
    # Fallback to original logic for backward compatibility
    # Preserve factor level order for the primary sort column
    if (is.factor(data[[col_basis]])) {
      # For factors, arrange by factor levels (preserves the order set by sorting functions)
      data |>
        dplyr::arrange(
          as.integer(.data[[col_basis]]),
          if (length(indep_vars) > 0) .data[[indep_vars[1]]]
        )
    } else {
      # For non-factors, use standard sorting
      data |>
        dplyr::arrange(
          .data[[col_basis]],
          if (length(indep_vars) > 0) .data[[indep_vars[1]]]
        )
    }
  }
}

#' Process data with standard table operations
#'
#' Apply column selection, renaming, and independent variable handling
#'
#' @param data Data frame to process
#' @param col_basis Column basis for variables
#' @param indep_vars Independent variable columns
#' @param indep_label Independent variable labels
#' @param main_question Main question for headers
#' @param use_header Whether to use main question as header
#' @param stat_columns Statistical columns to include
#' @param column_mappings Additional column mappings
#' @return Processed data frame
#' @keywords internal
process_table_data <- function(
  data,
  col_basis,
  indep_vars = NULL,
  indep_label = character(),
  main_question = "",
  use_header = FALSE,
  stat_columns = NULL,
  column_mappings = NULL
) {
  # Select relevant columns
  if (is.null(stat_columns)) {
    stat_columns <- c(
      "n",
      "n_valid",
      "n_miss",
      ".count",
      ".mean",
      ".mean_se",
      ".median",
      "mean",
      "sd",
      "median",
      "mad",
      "iqr",
      "min",
      "max"
    )
  }

  output_cols <- c(
    col_basis,
    indep_vars,
    intersect(stat_columns, colnames(data))
  )
  data <- data[, output_cols, drop = FALSE]

  # Rename columns
  colnames(data) <- vapply(
    colnames(data),
    get_standard_column_renamer(main_question, use_header, column_mappings),
    character(1)
  )

  # Handle independent variable naming conflicts
  if (
    length(indep_vars) > 0 &&
      length(indep_label) > 0 &&
      nchar(indep_label[1]) > 0
  ) {
    current_var_col_name <- colnames(data)[1]

    final_indep_name <- if (indep_label[1] == current_var_col_name) {
      paste0(indep_label[1], " (indep)")
    } else {
      indep_label[1]
    }

    data <- dplyr::rename_with(
      data,
      .cols = tidyselect::all_of(indep_vars),
      .fn = ~final_indep_name
    )
  }

  data
}
