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
  descend = FALSE,
  descend_indep = FALSE
) {
  data |>
    add_dep_order(sort_dep_by, descend) |>
    add_indep_order(sort_indep_by, descend_indep) |>
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

#' Apply sorting with optional descending order
#'
#' Unified helper to consistently handle ascending/descending sort order
#' across all sorting functions.
#'
#' @param data Dataset to arrange
#' @param order_col Symbol/name of the column to sort by
#' @param descend Whether to sort in descending order
#' @return Arranged dataset
#' @keywords internal
arrange_with_order <- function(data, order_col, descend = FALSE) {
  if (descend) {
    data |> dplyr::arrange(dplyr::desc({{ order_col }}))
  } else {
    data |> dplyr::arrange({{ order_col }})
  }
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

  if (all(sort_by %in% ".variable_position")) {
    data$.dep_order <- if (".variable_position" %in% names(data)) {
      data$.variable_position
    } else {
      # Fallback: use factor level order
      as.integer(data$.variable_label)
    }

    data$.dep_order <- descend_if_descending(data$.dep_order, descend)
  } else if (all(sort_by %in% ".variable_label")) {
    # Alphabetical order by variable labels
    # Create mapping from variable label to alphabetical rank
    unique_labels <- unique(data$.variable_label)
    sorted_labels <- sort(unique_labels)
    label_to_order <- stats::setNames(
      seq_along(sorted_labels),
      sorted_labels
    )

    # Map each row's variable label to its alphabetical order
    data$.dep_order <- label_to_order[as.character(data$.variable_label)]
    data$.dep_order <- descend_if_descending(data$.dep_order, descend)
  } else if (all(sort_by %in% ".variable_name")) {
    # Alphabetical order by variable names
    # Create mapping from variable name to alphabetical rank
    unique_names <- unique(data$.variable_name)
    sorted_names <- sort(unique_names)
    name_to_order <- stats::setNames(
      seq_along(sorted_names),
      sorted_names
    )

    # Map each row's variable name to its alphabetical order
    data$.dep_order <- name_to_order[as.character(data$.variable_name)]
    data$.dep_order <- descend_if_descending(data$.dep_order, descend)
  } else if (all(sort_by %in% unique(data$.category))) {
    # Category-based sorting (e.g., "A bit", "Not at all", etc.)
    if (length(sort_by) == 1) {
      # Single category: sort by count values for that specific category
      data$.dep_order <- calculate_category_order(
        data,
        sort_by[1],
        descend = descend
      )
    } else {
      # Multiple categories: sort by sum of counts for those categories
      # This uses .sum_value which should be pre-calculated
      if (".sum_value" %in% names(data)) {
        data$.dep_order <- calculate_sum_value_order(data, descend = descend)
      } else {
        # Fallback: calculate the sum ourselves
        data$.dep_order <- calculate_multiple_category_order(
          data,
          sort_by,
          descend = descend
        )
      }
    }
  } else if (all(sort_by %in% names(data))) {
    # Direct column sorting (e.g., .count, .proportion, .mean, etc.)
    data$.dep_order <- calculate_column_order(
      data,
      sort_by[1],
      descend = descend
    )
  } else if (
    length(sort_by) == 1 && all(sort_by %in% .saros.env$summary_data_sort1)
  ) {
    # Calculate order based on upper category proportions (.upper, .top, .bottom, .lower, .mid_upper, .mid_lower)
    data$.dep_order <- calculate_proportion_order(
      data,
      sort_by,
      descend = descend
    )
  } else {
    # Default fallback
    data$.dep_order <- if (".variable_position" %in% names(data)) {
      data$.variable_position
    } else {
      as.integer(data$.variable_label)
    }

    data$.dep_order <- descend_if_descending(data$.dep_order, descend)
  }

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
  sort_by = NULL,
  descend = FALSE
) {
  if (is.null(sort_by)) {
    # Return data with default ordering if no independent sorting requested
    data$.indep_order <- 1
    return(data)
  }

  # Get the independent variable column
  indep_col <- get_indep_col_name(data)
  if (is.null(indep_col)) {
    # No independent variable column found, return with default ordering
    data$.indep_order <- 1
    return(data)
  }

  # Apply ascending order column sorting if it's not ".variable_label" (which should remain in existing order)
  data$.indep_order <- if (
    length(sort_by) == 1 && sort_by == ".variable_label"
  ) {
    # Alphabetical sorting by variable labels
    indep_labels <- data[[indep_col]]
    if (descend) {
      order(-rank(indep_labels)) # Descending alphabetical
    } else {
      rank(indep_labels) # Ascending alphabetical
    }
  } else if (length(sort_by) == 1 && startsWith(sort_by, ".")) {
    # Handle special sorting methods (.top, .bottom, .upper, .lower, .count, etc.)
    if (
      sort_by %in%
        c(".top", ".bottom", ".upper", ".lower", ".mid_upper", ".mid_lower")
    ) {
      # Proportion-based sorting
      calculate_indep_proportion_order(data, sort_by, indep_col, descend)
    } else if (startsWith(sort_by, ".count")) {
      # Count-based sorting
      if (sort_by == ".count_per_indep_group") {
        column_name <- ".count_total_indep"
      } else {
        column_name <- ".count"
      }
      calculate_indep_column_order(data, column_name, indep_col, descend)
    } else if (sort_by %in% c(".mean", ".median", ".sum_value")) {
      # Statistical measures
      calculate_indep_column_order(data, sort_by, indep_col, descend)
    } else {
      # Fallback to 1-based ordering if unrecognized
      seq_len(nrow(data))
    }
  } else {
    # Category-based sorting or multiple categories
    if (length(sort_by) == 1 && sort_by %in% levels(data$.category)) {
      # Single category
      calculate_indep_category_order(data, sort_by, indep_col, descend)
    } else if (all(sort_by %in% levels(data$.category))) {
      # Multiple categories - sum their values
      calculate_indep_sum_value_order(data, sort_by, indep_col, descend)
    } else {
      # Fallback if categories not found
      seq_len(nrow(data))
    }
  }

  data
}

#' Get the name of the independent variable column
#'
#' @param data Dataset
#' @return Character string with column name, or NULL if not found
#'
#' @keywords internal
get_indep_col_name <- function(data) {
  # Get independent variable column names (excluding standard columns)
  indep_cols <- names(data)[!names(data) %in% .saros.env$summary_data_sort2]

  if (length(indep_cols) > 0) {
    return(indep_cols[1]) # Return first independent column
  } else {
    return(NULL)
  }
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
  } else if (length(sort_by) == 1 && sort_by %in% unique(data$.category)) {
    # Category-based sorting: put the target category first
    data$.category_order <- ifelse(
      data$.category == sort_by,
      1, # Target category gets priority (first)
      as.integer(data$.category) + 1 # Other categories follow
    )
  } else {
    # Default: use existing factor level order
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
#' @param descend Logical indicating if sorting should be descending
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_proportion_order <- function(data, method, descend = FALSE) {
  # Get the target category based on the method
  target_category <- get_target_categories(data, method)

  # Calculate order based on proportions for the target category/categories
  if (length(target_category) == 1) {
    # Single category - aggregate proportions by variable
    category_summary <-
      data |>
      dplyr::filter(.data$.category == target_category) |>
      dplyr::summarise(
        avg_proportion = mean(.data$.proportion, na.rm = TRUE),
        .by = tidyselect::all_of(".variable_name")
      ) |>
      arrange_with_order(.data$avg_proportion, descend = descend) |> # Asc/Desc order
      dplyr::mutate(order_rank = dplyr::row_number()) |>
      dplyr::select(tidyselect::all_of(c(".variable_name", "order_rank")))
  } else {
    # Multiple categories - sum their proportions and aggregate by variable
    category_summary <- data |>
      dplyr::filter(.data$.category %in% target_category) |>
      dplyr::summarise(
        sum_proportion = sum(.data$.proportion, na.rm = TRUE),
        .by = tidyselect::all_of(c(".variable_name"))
      ) |>
      arrange_with_order(.data$sum_proportion, descend = descend) |> # Asc/Desc order
      dplyr::mutate(order_rank = dplyr::row_number()) |>
      dplyr::select(tidyselect::all_of(c(".variable_name", "order_rank")))
  }

  # Join back to original data and extract order ranks
  # Use relationship = "many-to-one" since multiple data rows match one summary row
  data |>
    dplyr::left_join(
      category_summary,
      by = ".variable_name",
      relationship = "many-to-one"
    ) |>
    dplyr::pull(.data$order_rank)
}

#' Calculate ordering based on multiple category values
#'
#' @param data Dataset with .category and .count columns
#' @param category_values Vector of category values to sum (e.g., c("A bit", "A lot"))
#' @param descend Logical indicating if sorting should be descending
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_multiple_category_order <- function(
  data,
  category_values,
  descend = FALSE
) {
  # Filter data to the specified categories and sum their counts by variable
  category_summary <- data |>
    dplyr::filter(.data$.category %in% category_values) |>
    dplyr::summarise(
      sum_count = sum(.data$.count, na.rm = TRUE),
      .by = tidyselect::all_of(".variable_label")
    ) |>
    arrange_with_order(.data$sum_count, descend = descend) |> # Asc/Desc order
    dplyr::mutate(order_rank = dplyr::row_number()) |>
    dplyr::select(tidyselect::all_of(c(".variable_label", "order_rank")))

  # Join back to original data and extract order ranks
  # Use relationship = "many-to-one" since multiple data rows match one summary row
  data |>
    dplyr::left_join(
      category_summary,
      by = ".variable_label",
      relationship = "many-to-one"
    ) |>
    dplyr::pull(.data$order_rank)
}

#' Calculate ordering based on a specific category value
#'
#' @param data Dataset with .category and .count columns
#' @param category_value The category value to sort by (e.g., "A bit")
#' @param descend Logical indicating if sorting should be descending
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_category_order <- function(data, category_value, descend = FALSE) {
  # Filter data to the specific category and calculate order by .count
  category_summary <- data |>
    dplyr::filter(.data$.category == category_value) |>
    dplyr::select(tidyselect::all_of(c(".variable_label", ".count"))) |>
    arrange_with_order(.data$.count, descend = descend) |> # Asc/Desc order
    dplyr::mutate(order_rank = dplyr::row_number()) |>
    dplyr::select(tidyselect::all_of(c(".variable_label", "order_rank")))

  # Join back to original data and extract order ranks
  # Use relationship = "many-to-one" since multiple data rows match one summary row
  data |>
    dplyr::left_join(
      category_summary,
      by = ".variable_label",
      relationship = "many-to-one"
    ) |>
    dplyr::pull(.data$order_rank)
}

#' Calculate ordering based on a specific column value
#'
#' @param data Dataset
#' @param column_name Name of the column to sort by
#' @param descend Logical indicating if sorting should be descending
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_column_order <- function(data, column_name, descend = FALSE) {
  # Group by variable and calculate summary statistic for ordering
  summary_order <- data |>
    dplyr::summarise(
      # Aggregation rule: for .count always use sum across groups; otherwise keep previous max behavior
      order_value = if (identical(column_name, ".count")) {
        sum(.data[[column_name]], na.rm = TRUE)
      } else {
        max(.data[[column_name]], na.rm = TRUE)
      },
      .by = tidyselect::all_of(".variable_label")
    ) |>
    arrange_with_order(.data$order_value, descend = descend) |> # Asc/Desc order
    dplyr::mutate(order_rank = dplyr::row_number()) |>
    dplyr::select(tidyselect::all_of(c(".variable_label", "order_rank")))

  # Join back to original data and extract order ranks
  # Use relationship = "many-to-one" since multiple data rows match one summary row
  data |>
    dplyr::left_join(
      summary_order,
      by = ".variable_label",
      relationship = "many-to-one"
    ) |>
    dplyr::pull(.data$order_rank)
}

#' Calculate ordering based on .sum_value (for category-based sorting)
#'
#' @param data Dataset with .sum_value column
#' @param descend Logical indicating if sorting should be descending
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_sum_value_order <- function(data, descend = FALSE) {
  # Use .sum_value for ordering (this is pre-calculated in add_collapsed_categories)
  summary_order <- data |>
    dplyr::summarise(
      # Aggregation rule: use sum for sum_value to reflect total contribution
      order_value = sum(.data$.sum_value, na.rm = TRUE),
      .by = tidyselect::all_of(".variable_label")
    ) |>
    # Default behaviour historically sorted descending; invert flag to preserve API
    arrange_with_order(.data$order_value, descend = !descend) |>
    dplyr::mutate(order_rank = dplyr::row_number()) |>
    dplyr::select(tidyselect::all_of(c(".variable_label", "order_rank")))

  # Join back to original data and extract order ranks
  data |>
    dplyr::left_join(summary_order, by = ".variable_label") |>
    dplyr::pull(.data$order_rank)
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

#' Calculate independent variable ordering based on a specific category value
#'
#' @param data Dataset with independent variable columns
#' @param category_value The category value to sort by (e.g., "Not at all")
#' @param indep_col Name of the independent variable column
#' @param descend_indep Logical indicating if sorting should be descending
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_indep_category_order <- function(
  data,
  category_value,
  indep_col,
  descend_indep = FALSE
) {
  # Filter data to the specific category and calculate order by averaging across dep variables
  category_summary <- data |>
    dplyr::filter(.data$.category == category_value) |>
    dplyr::summarise(
      avg_proportion = mean(.data$.proportion, na.rm = TRUE),
      .by = tidyselect::all_of(indep_col)
    ) |>
    arrange_with_order(.data$avg_proportion, descend = descend_indep) |>
    dplyr::mutate(order_rank = dplyr::row_number()) |>
    dplyr::select(tidyselect::all_of(c(indep_col, "order_rank")))

  # Join back to original data and extract order ranks
  data |>
    dplyr::left_join(
      category_summary,
      by = indep_col,
      relationship = "many-to-one"
    ) |>
    dplyr::pull(.data$order_rank)
}

#' Calculate independent variable ordering based on a specific column value
#'
#' @param data Dataset
#' @param column_name Name of the column to sort by
#' @param indep_col Name of the independent variable column
#' @param descend_indep Logical indicating if sorting should be descending
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_indep_column_order <- function(
  data,
  column_name,
  indep_col,
  descend_indep = FALSE
) {
  # Group by independent variable and calculate summary statistic for ordering
  summary_order <- data |>
    dplyr::summarise(
      # Aggregation rule: for count-like columns use sum across groups; otherwise mean is acceptable
      order_value = if (identical(column_name, ".count") || identical(column_name, ".count_total_indep")) {
        sum(.data[[column_name]], na.rm = TRUE)
      } else {
        mean(.data[[column_name]], na.rm = TRUE)
      },
      .by = tidyselect::all_of(indep_col)
    ) |>
    arrange_with_order(.data$order_value, descend = descend_indep) |>
    dplyr::mutate(order_rank = dplyr::row_number()) |>
    dplyr::select(tidyselect::all_of(c(indep_col, "order_rank")))

  # Join back to original data and extract order ranks
  data |>
    dplyr::left_join(
      summary_order,
      by = indep_col,
      relationship = "many-to-one"
    ) |>
    dplyr::pull(.data$order_rank)
}

#' Calculate independent variable ordering based on position categories
#'
#' @param data Dataset
#' @param method Either ".upper", ".top", etc.
#' @param indep_col Name of the independent variable column
#' @param descend_indep Logical indicating if sorting should be descending
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_indep_proportion_order <- function(
  data,
  method,
  indep_col,
  descend_indep = FALSE
) {
  # Get the target category based on the method
  target_category <- get_target_categories(data, method)

  # Calculate order based on proportions for the target category/categories
  if (length(target_category) == 1) {
    # Single category - use existing calculate_indep_category_order
    calculate_indep_category_order(
      data,
      target_category,
      indep_col,
      descend_indep
    )
  } else {
    # Multiple categories - sum their proportions
    category_summary <- data |>
      dplyr::filter(.data$.category %in% target_category) |>
      dplyr::summarise(
        sum_proportion = sum(.data$.proportion, na.rm = TRUE),
        .by = tidyselect::all_of(indep_col)
      ) |>
      arrange_with_order(.data$sum_proportion, descend = descend_indep) |>
      dplyr::mutate(order_rank = dplyr::row_number()) |>
      dplyr::select(tidyselect::all_of(c(indep_col, "order_rank")))

    # Join back to original data and extract order ranks
    data |>
      dplyr::left_join(
        category_summary,
        by = indep_col,
        relationship = "many-to-one"
      ) |>
      dplyr::pull(.data$order_rank)
  }
}

#' Calculate independent variable ordering based on multiple category values
#'
#' @param data Dataset
#' @param category_values Vector of category values to sum
#' @param indep_col Name of the independent variable column
#' @param descend_indep Logical indicating if sorting should be descending
#' @return Numeric vector of ordering values
#'
#' @keywords internal
calculate_indep_sum_value_order <- function(
  data,
  category_values,
  indep_col,
  descend_indep = FALSE
) {
  # Filter data to the specified categories and sum their proportions by indep variable
  category_summary <- data |>
    dplyr::filter(.data$.category %in% category_values) |>
    dplyr::summarise(
      sum_proportion = sum(.data$.proportion, na.rm = TRUE),
      .by = tidyselect::all_of(indep_col)
    ) |>
    arrange_with_order(.data$sum_proportion, descend = descend_indep) |>
    dplyr::mutate(order_rank = dplyr::row_number()) |>
    dplyr::select(tidyselect::all_of(c(indep_col, "order_rank")))

  # Join back to original data and extract order ranks
  data |>
    dplyr::left_join(
      category_summary,
      by = indep_col,
      relationship = "many-to-one"
    ) |>
    dplyr::pull(.data$order_rank)
}

#' Get target categories for positional sorting
#'
#' Uses subset_vector to determine which categories to include based on
#' positional methods like .top, .bottom, .upper, .lower, etc.
#'
#' @param data Dataset with .category column
#' @param method Positional method (.top, .bottom, .upper, .lower, etc.)
#' @return Character vector of target category names
#' @keywords internal
get_target_categories <- function(data, method) {
  all_categories <- levels(data$.category)
  subset_vector(all_categories, method)
}
