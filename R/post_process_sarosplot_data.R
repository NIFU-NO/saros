#' Process Independent Variable Factor Levels
#'
#' Reverses factor levels for independent variables, but only for unordered factors.
#' Preserves the natural ordering of ordered factors.
#'
#' @param data Data frame containing the data
#' @param indep Character string naming the independent variable (or NULL)
#'
#' @return Modified data frame with reversed factor levels for unordered factors
#'
#' @keywords internal
process_indep_factor_levels <- function(data, indep = NULL) {
  if (length(indep) > 0) {
    # Only reverse unordered factors, preserve ordered factor levels
    if (!is.ordered(data[[indep]])) {
      data[[indep]] <- forcats::fct_rev(data[[indep]])
    }
  }
  data
}

#' Process Binary Category Colors
#'
#' Reverses the .category variable for binary categories when a special color
#' condition is met. This is specific to categorical plot functionality.
#'
#' @param data Data frame containing the data with .category column
#' @param showNA Character indicating how to handle NA values
#' @param colour_2nd_binary_cat Color specification for second binary category
#'
#' @return Modified data frame with potentially reversed .category levels
#'
#' @keywords internal
process_binary_category_colors <- function(
  data,
  showNA = "never",
  colour_2nd_binary_cat = NULL
) {
  # Only process if .category column exists
  if (!".category" %in% colnames(data)) {
    return(data)
  }

  if (
    dplyr::n_distinct(data$.category, na.rm = showNA == "never") == 2 &&
      !is.null(colour_2nd_binary_cat)
  ) {
    data$.category <- forcats::fct_rev(data$.category)
  }
  data
}

#' Post-process Makeme Data (Legacy)
#'
#' Legacy function that combines both factor level processing and binary category
#' color processing. Use the individual functions for new code.
#'
#' @param data Data frame containing the data
#' @param indep Character string naming the independent variable (or NULL)
#' @param showNA Character indicating how to handle NA values
#' @param colour_2nd_binary_cat Color specification for second binary category
#'
#' @return Modified data frame
#'
#' @keywords internal
post_process_makeme_data <- function(
  data,
  indep = NULL,
  showNA = "never",
  colour_2nd_binary_cat = NULL
) {
  # Process independent variable factor levels
  data <- process_indep_factor_levels(data, indep)

  # Process binary category colors
  data <- process_binary_category_colors(data, showNA, colour_2nd_binary_cat)

  data
}
