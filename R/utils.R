# Common utility functions for string and variable processing ----

#' Apply string wrapping to variables (character or factor)
#'
#' A utility function that applies string wrapping to both character and factor
#' variables, preserving factor structure while wrapping the labels.
#'
#' @param x Variable to wrap (character or factor)
#' @param width Maximum width for wrapping
#'
#' @return Modified variable with wrapped text
#' @keywords internal
strip_wrap_var <- function(x, width = Inf) {
  if (is.character(x)) {
    return(string_wrap(x, width = width))
  }
  if (is.factor(x)) {
    levels(x) <- string_wrap(
      levels(x),
      width = width
    )
    x
  }
}

#' Determine display column for dependent variables in int_plot_html
#'
#' Checks if the number of dep variables matches the number of labels to determine
#' whether to use .variable_label or .variable_name for display.
#'
#' @param dep_count Number of dependent variables
#' @param dep_labels Vector of dependency labels
#'
#' @return Character string indicating which column to use
#' @keywords internal
get_dep_display_column <- function(dep_count, dep_labels) {
  if (dep_count == length(dep_labels)) {
    ".variable_label"
  } else {
    ".variable_name"
  }
}

#' Determine display column based on data availability
#'
#' Checks if .variable_label column exists and has non-NA values to determine
#' whether to use .variable_label or .variable_name for display.
#'
#' @param data Data frame containing variable information
#'
#' @return Character string indicating which column to use
#' @keywords internal
get_data_display_column <- function(data) {
  if (
    ".variable_label" %in%
      colnames(data) &&
      all(!is.na(data[[".variable_label"]]))
  ) {
    ".variable_label"
  } else {
    ".variable_name"
  }
}

#' Validate single dependent variable requirement
#'
#' Common validation pattern for functions that require exactly one dependent variable.
#'
#' @param dep Vector of dependent variables
#' @param function_name Name of the function requiring validation (for error message)
#'
#' @return Nothing if valid, throws error if invalid
#' @keywords internal
validate_single_dep_var <- function(dep, function_name) {
  if (length(dep) > 1) {
    rlang::abort(paste0(
      function_name,
      " requires exactly one dependent variable"
    ))
  }
}

#' Given Ordered Integer Vector, Return Requested Set.
#'
#' Useful for identifying which categories are to be collected.
#'
#' @param vec A vector of any type.
#' @param set A character string, one of c(".top", ".upper", ".mid_upper", ".lower",
#'   ".mid_lower", ".bottom")
#' @param spread_n The number of values to extract when set is "spread".
#' @param sort Whether to sort the output, defaults to FALSE.
#' @return Selected set of vector.
#' @keywords internal
# subset_vector(vec=1:7, set=".mid_lower")
subset_vector <-
  function(
    vec,
    set = c(
      ".top",
      ".upper",
      ".mid_upper",
      ".lower",
      ".mid_lower",
      ".bottom",
      ".spread"
    ),
    spread_n = NULL,
    sort = FALSE
  ) {
    set <- rlang::arg_match(set)
    n <- length(vec)
    if (sort) {
      vec <- sort(vec)
    }
    if (n %in% 0:1) {
      vec
    } else if (set == ".top") {
      vec[n]
    } else if (set == ".bottom") {
      vec[1]
    } else if (n %% 2 == 0L && set != ".spread") {
      if (set %in% c(".mid_upper", ".upper")) {
        vec[(n / 2 + 1):n]
      } else if (set %in% c(".mid_lower", ".lower")) {
        vec[1:(n / 2)]
      }
    } else {
      m <- stats::median(seq_len(n))
      if (set == ".upper") {
        vec[(m + 1):n]
      } else if (set == ".lower") {
        vec[1:(m - 1)]
      } else if (set == ".mid_upper") {
        vec[m:n]
      } else if (set == ".mid_lower") {
        vec[1:m]
      } else if (set == ".spread") {
        if (n <= spread_n) {
          vec
        } else {
          # Algorithm to maximize spread: select extremes first, then fill in
          indices <- c()

          # Always start with extremes if we need more than 1 element
          if (spread_n >= 2L) {
            indices <- c(1L, n)
          }

          # If we need just 1 element, take the middle
          if (spread_n == 1L) {
            indices <- round(stats::median(seq_len(n)))
          } else if (spread_n > 2L) {
            # Add middle element if we need an odd number of elements
            if (spread_n %% 2 != 0L) {
              indices <- c(indices, round(stats::median(seq_len(n))))
            }

            # Fill in remaining positions by maximizing gaps
            remaining <- spread_n - length(indices)
            if (remaining > 0L) {
              # Create a sequence of evenly spaced positions
              positions <- seq(1, n, length.out = spread_n)
              indices <- unique(round(positions))
            }
          }

          # Ensure we don't exceed bounds and have the right number of elements
          indices <- sort(unique(pmax(1L, pmin(n, indices))))
          if (length(indices) > spread_n) {
            # If we have too many, keep the most spread out ones
            keep_positions <- seq(1, length(indices), length.out = spread_n)
            indices <- indices[round(keep_positions)]
          }

          vec[indices]
        }
      }
    }
  }


###  Check that all pairs of cols share at least one observed response category
check_category_pairs <-
  function(data, cols_pos, call = rlang::caller_env(), return_error = TRUE) {
    lapply(X = seq_along(cols_pos), FUN = function(i) {
      y <- names(cols_pos)[[i]]

      cols_rest <-
        cols_pos[-c(1:match(y, names(cols_pos)))]
      lapply(X = seq_along(cols_rest), FUN = function(e) {
        y2 <- names(cols_rest)[[e]]

        val_y <- if (is.factor(data[[y]])) {
          levels(data[[y]])
        } else {
          unique(data[[y]])
        }
        val_y2 <- if (is.factor(data[[y2]])) {
          levels(data[[y2]])
        } else {
          unique(data[[y2]])
        }
        common <- dplyr::intersect(val_y, val_y2)
        if (length(common) == 0L) {
          cli::cli_abort(
            c(
              "Unequal variables.",
              "!" = "All variables must share at least one common category.",
              "i" = "Column {.var {y}} and column {.var {y2}} lack common categories."
            ),
            call = call
          )
        }
      })
    })
    TRUE
  }
