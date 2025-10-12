#' Create sorting order variables for output dataframe
#'
#' This module provides centralized sorting functionality to ensure consistent
#' ordering across all output types (tables, plots) by using explicit order
#' columns instead of relying on factor levels that can be overridden.

#' Apply comprehensive sorting order to survey data
#'
#' @param data Dataset with survey results
#' @param sort_dep_by How to sort dependent variables
#' @param sort_indep_by How to sort independent variable categories
#' @param sort_category_by How to sort response categories
#' @param descend Whether to reverse the dependent variable order
#' @return Dataset with added order columns: .dep_order, .indep_order, .category_order
#'
#' @keywords internal
add_sorting_order_vars <- function(
  data,
  sort_dep_by = ".variable_position",
  sort_indep_by = NULL,
  sort_category_by = NULL,
  descend = FALSE
) {
  data |>
    add_dep_order(sort_dep_by, descend) |>
    add_indep_order(sort_indep_by) |>
    add_category_order(sort_category_by) |>
    apply_final_arrangement()
}

descend_if_descending <- function(x, descend) {
  if (descend) {
    max_x <- max(x, na.rm = TRUE)
    return(max_x - x + 1)
  }
  x
}

#' Add dependent variable ordering
#'
#' @param data Dataset
#' @param sort_by Sorting method for dependent variables
#' @param descend Whether to reverse the order
#' @return Dataset with .dep_order column added
#'
#' @keywords internal
add_dep_order <- function(data, sort_by, descend = FALSE) {
  # Convert NULL to default
  if (is.null(sort_by)) {
    sort_by <- ".variable_position"
  }

  # Calculate base order based on sort method

  data$.dep_order <-
    dplyr::case_when(
      all(sort_by %in% ".variable_position") ~
        {
          if (".variable_position" %in% names(data)) {
            data$.variable_position
          } else {
            # Fallback: use factor level order
            as.integer(data$.variable_label)
          }
        },
      all(sort_by %in% ".variable_label") ~
        {
          # Alphabetical order by variable labels
          # Create mapping from variable label to alphabetical rank
          unique_labels <- unique(data$.variable_label)
          sorted_labels <- sort(unique_labels)
          label_to_order <- stats::setNames(
            seq_along(sorted_labels),
            sorted_labels
          )

          # Map each row's variable label to its alphabetical order
          label_to_order[as.character(data$.variable_label)]
        },
      all(sort_by %in% ".variable_name") ~
        {
          # Alphabetical order by variable names
          # Create mapping from variable name to alphabetical rank
          unique_names <- unique(data$.variable_name)
          sorted_names <- sort(unique_names)
          name_to_order <- stats::setNames(
            seq_along(sorted_names),
            sorted_names
          )

          # Map each row's variable name to its alphabetical order
          name_to_order[as.character(data$.variable_name)]
        },
      length(sort_by) == 1 && all(sort_by %in% .saros.env$summary_data_sort1) ~
        {
          # Calculate order based on upper category proportions
          calculate_proportion_order(data, sort_by)
        },
      .default = {
        # Default fallback
        if (".variable_position" %in% names(data)) {
          data$.variable_position
        } else {
          as.integer(data$.variable_label)
        }
      }
    )

  # Apply descending order if requested
  data$.dep_order <- descend_if_descending(data$.dep_order, descend = descend)

  data
}

#' Add independent variable category ordering
#'
#' @param data Dataset
#' @param sort_by Sorting method for independent categories (NULL = no sorting)
#' @param descend Whether to reverse the order
#' @return Dataset with .indep_order column added
#'
#' @keywords internal
add_indep_order <- function(
  data,
  indep_cols = NULL,
  sort_by = NULL,
  descend = FALSE
) {
  # Get independent variable column names (excluding standard columns)
  indep_cols <- names(data)[!names(data) %in% .saros.env$summary_data_sort2]

  if (is.null(sort_by) || length(indep_cols) == 0) {
    # No sorting - preserve original order
    data$.indep_order <- 1
    return(data)
  }

  # For now, implement basic indep ordering
  # This can be expanded later for specific sorting methods
  data$.indep_order <- 1

  data$.indep_order <- descend_if_descending(
    data$.indep_order,
    descend = descend
  )

  data
}

#' Add response category ordering (only useful for long format cat-cat tables)
#'
#' @param data Dataset
#' @param sort_by Sorting method for response categories
#' @return Dataset with .category_order column added
#'
#' @keywords internal
add_category_order <- function(data, sort_by = NULL) {
  if (is.null(sort_by)) {
    # Use existing factor level order
    data$.category_order <- as.integer(data$.category)
  } else {
    # Future: implement specific category sorting methods
    data$.category_order <- as.integer(data$.category)
  }

  data
}

#' Apply final arrangement based on order columns
#'
#' @param data Dataset with order columns
#' @return Arranged dataset
#'
#' @keywords internal
apply_final_arrangement <- function(data) {
  data |>
    dplyr::arrange(.data$.dep_order, .data$.indep_order, .data$.category_order)
}

#' Calculate proportion-based ordering for dependent variables
#'
#' @param data Dataset
#' @param method Either ".upper", ".top", etc.
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_proportion_order <- function(data, method) {
  # This is a simplified implementation
  # The real logic would calculate proportions per variable and rank them

  # For now, return a placeholder that uses sum_value if available
  if (".sum_value" %in% names(data)) {
    # Use existing sum_value calculation
    data |>
      dplyr::group_by(.data$.variable_label) |>
      dplyr::summarise(
        order_value = mean(.data$.sum_value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$order_value)) |>
      dplyr::mutate(order_rank = dplyr::row_number()) |>
      dplyr::select(tidyselect::all_of(c(".variable_label", "order_rank"))) |>
      dplyr::left_join(data, by = ".variable_label") |>
      dplyr::pull(.data$order_rank)
  } else {
    # Fallback to position order
    if (".variable_position" %in% names(data)) {
      data$.variable_position
    } else {
      as.integer(data$.variable_label)
    }
  }
}

#' Set factor levels based on order columns (for backward compatibility)
#'
#' @param data Dataset with order columns
#' @return Dataset with factor levels set according to order
#'
#' @keywords internal
set_factor_levels_from_order <- function(data) {
  # Set variable label factor levels based on .dep_order
  if (".dep_order" %in% names(data) && is.factor(data$.variable_label)) {
    var_order <-
      data |>
      dplyr::distinct(.data$.variable_label, .data$.dep_order) |>
      dplyr::arrange(.data$.dep_order) |>
      dplyr::pull(.data$.variable_label) |>
      as.character()

    data$.variable_label <- factor(
      data$.variable_label,
      levels = var_order
    )
  }

  # Set category factor levels based on .category_order
  if (".category_order" %in% names(data) && is.factor(data$.category)) {
    cat_order <-
      data |>
      dplyr::distinct(.data$.category, .data$.category_order) |>
      dplyr::arrange(.data$.category_order) |>
      dplyr::pull(.data$.category) |>
      as.character()

    data$.category <- factor(
      data$.category,
      levels = cat_order
    )
  }

  data
}
