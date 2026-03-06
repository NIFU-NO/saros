# Common utility functions for string and variable processing ----

#' Attach dep_label_prefix attribute to a make_content output object
#'
#' Attaches the main question (i.e. the label prefix of the dep variables)
#' as an attribute `"dep_label_prefix"` on the returned object. Works for
#' ggplot, data.frame, and mschart objects. Should not be called when returning
#' an rdocx object.
#'
#' Storage location by class:
#' - **ggplot / gg**: stored on `obj$data` when `obj$data` is a data.frame,
#'   so the attribute survives further `+` operations. Falls back to
#'   `attr(obj, ...)` when `obj$data` is a `waiver()` (empty `ggplot()`).
#' - **ms_barchart**: stored on `obj$data` (consistent with ggplot).
#' - **everything else** (data.frame, tibble, …): stored on the object itself.
#'
#' @param obj The object to annotate (ggplot, data.frame, mschart, etc.)
#' @param main_question Character scalar; the dep label prefix. Must satisfy
#'   `rlang::is_string(main_question)` and be non-empty. If not, `obj` is
#'   returned unchanged.
#'
#' @return `obj`, unchanged when `main_question` is not a non-empty string,
#'   otherwise with `"dep_label_prefix"` set in the appropriate location.
#' @keywords internal
attach_dep_label_prefix <- function(obj, main_question) {
  if (rlang::is_string(main_question) && nzchar(main_question)) {
    # For ggplot objects, store on $data so the attribute survives further
    # `+` operations (which never replace $data).
    # For mschart objects, store on $data for the same reason.
    # For all other objects (data.frame, …), store on the object itself.
    if (inherits(obj, "gg") && is.data.frame(obj$data)) {
      attr(obj$data, "dep_label_prefix") <- main_question
    } else if (inherits(obj, "ms_barchart") && is.data.frame(obj$data)) {
      attr(obj$data, "dep_label_prefix") <- main_question
    } else {
      attr(obj, "dep_label_prefix") <- main_question
    }
  }
  obj
}

#' Retrieve the dep label prefix from a saros output object
#'
#' Retrieves the `"dep_label_prefix"` attribute that saros attaches to every
#' object returned by [makeme()] / `make_content.*()`. This is the main
#' question text — the shared label prefix of all dependent variables used to
#' produce the object.
#'
#' Storage location by class:
#' - **ggplot / gg** and **ms_barchart**: attribute is stored on `obj$data`
#'   (when `obj$data` is a data.frame) so that it survives further `+`
#'   operations. This function reads from `obj$data` first for both classes.
#' - **data.frame and other objects**: attribute stored directly on `obj`.
#'
#' @param obj Any object returned by [makeme()] or a `make_content.*()` method
#'   (ggplot, data.frame, mschart, …).
#'
#' @return A character scalar: the dep label prefix if present and non-empty,
#'   otherwise `""`.
#'
#' @export
#' @examples
#' p <- makeme(data = ex_survey, dep = b_1:b_3)
#' get_dep_label_prefix(p)
get_dep_label_prefix <- function(obj) {
  is_valid <- function(x) isTRUE(nzchar(x))
  # For ggplots and mschart, check $data first
  if (
    (inherits(obj, "gg") || inherits(obj, "ms_barchart")) &&
      is.data.frame(obj$data)
  ) {
    val <- attr(obj$data, "dep_label_prefix", exact = TRUE)
    if (is_valid(val)) return(val)
  }
  # Fallback: check the object itself (data.frame, or ggplot with waiver $data)
  val <- attr(obj, "dep_label_prefix", exact = TRUE)
  if (is_valid(val)) val else ""
}

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

#' Detect if data is from int_plot_html
#'
#' Checks if a data frame has the structure expected from int_plot_html output,
#' which uses .value column instead of .category column.
#'
#' @param data Data frame to check
#'
#' @return Logical indicating if this is int_plot_html format
#' @keywords internal
is_int_plot_html <- function(data) {
  if (is.null(data)) return(FALSE)
  ".value" %in% colnames(data) && !".category" %in% colnames(data)
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
