get_variable_label_column <- function(data) {
  # Use original variable label if available (when hide_axis_text_if_single_variable = TRUE)
  if (
    any(colnames(data) == ".variable_label_original") &&
      all(data$.variable_label == "")
  ) {
    ".variable_label_original"
  } else {
    ".variable_label"
  }
}

get_common_variable_label_column <- function(dat_1, dat_2) {
  # Determine which variable label column to use based on BOTH datasets
  # Prefer a column that has non-empty values in BOTH datasets

  # Check .variable_label_original in both datasets.
  # This is preferred over .variable_label for cross-crowd matching because
  # add_n_to_label() appends a crowd-specific "(N = X)" suffix to
  # .variable_label, making it differ between crowds even for the same
  # variable. .variable_label_original retains the pre-suffix value and is
  # therefore a stable join key.
  dat_1_has_original <- ".variable_label_original" %in%
    colnames(dat_1) &&
    any(nchar(trimws(as.character(dat_1$.variable_label_original))) > 0, na.rm = TRUE)
  dat_2_has_original <- ".variable_label_original" %in%
    colnames(dat_2) &&
    any(nchar(trimws(as.character(dat_2$.variable_label_original))) > 0, na.rm = TRUE)

  # Prefer .variable_label_original when BOTH datasets have it — this avoids
  # the "(N = X)" mismatch that causes zero-proportion lookups.
  if (dat_1_has_original && dat_2_has_original) {
    return(list(
      var_col = ".variable_label_original",
      dat_1 = dat_1,
      dat_2 = dat_2
    ))
  }

  # Check .variable_label in both datasets
  dat_1_has_variable_label <- ".variable_label" %in%
    colnames(dat_1) &&
    any(nchar(trimws(as.character(dat_1$.variable_label))) > 0, na.rm = TRUE)
  dat_2_has_variable_label <- ".variable_label" %in%
    colnames(dat_2) &&
    any(nchar(trimws(as.character(dat_2$.variable_label))) > 0, na.rm = TRUE)

  # Fall back to .variable_label if BOTH datasets have values there
  if (dat_1_has_variable_label && dat_2_has_variable_label) {
    return(list(var_col = ".variable_label", dat_1 = dat_1, dat_2 = dat_2))
  }

  # If one dataset has .variable_label and the other has .variable_label_original,
  # copy values to make them comparable
  if (
    dat_1_has_original &&
      !dat_1_has_variable_label &&
      dat_2_has_variable_label &&
      !dat_2_has_original
  ) {
    # Copy dat_1's .variable_label_original to .variable_label
    dat_1$.variable_label <- dat_1$.variable_label_original
    return(list(var_col = ".variable_label", dat_1 = dat_1, dat_2 = dat_2))
  }

  if (
    dat_2_has_original &&
      !dat_2_has_variable_label &&
      dat_1_has_variable_label &&
      !dat_1_has_original
  ) {
    # Copy dat_2's .variable_label_original to .variable_label
    dat_2$.variable_label <- dat_2$.variable_label_original
    return(list(var_col = ".variable_label", dat_1 = dat_1, dat_2 = dat_2))
  }

  # Fallback: prefer .variable_label
  return(list(var_col = ".variable_label", dat_1 = dat_1, dat_2 = dat_2))
}

get_prop_for_highest_categories <- function(
  plot_data,
  var,
  selected_categories,
  var_col
) {
  data.frame(
    var = var,
    value = plot_data |>
      dplyr::filter(
        .data[[var_col]] == var,
        as.character(.data$.category) %in% selected_categories
      ) |>
      dplyr::pull(.data$.proportion) |>
      as.numeric() |>
      sum(na.rm = TRUE)
  )
}

#' Extract Text Summary from Categorical Mesos Plots
#'
#' @description
#' Generates text summaries comparing two groups from categorical mesos plot data.
#' The function identifies meaningful differences between groups based on proportions
#' of respondents selecting specific categories and produces narrative text descriptions.
#'
#' @param plots A list of two plot objects (or data frames with plot data) to compare.
#'   Each must contain columns: `.variable_label`, `.category`, `.category_order`, `.proportion`.
#' @param min_prop_diff Numeric. Minimum proportion difference (default 0.10) required
#'   between groups to generate text. Differences below this threshold are ignored.
#' @param n_highest_categories Integer. Number of top categories to include in the
#'   comparison (default 1). Categories are selected based on `.category_order`.
#'   Only applied if the variable has more categories than this value.
#' @param flip_to_lowest_categories Logical. If TRUE, compare lowest categories instead
#'   of highest (default FALSE).
#' @param digits Integer. Number of decimal places for rounding proportions (default 2).
#' @param selected_categories_last_split Character. Separator for the last item when
#'   listing multiple categories (default " or ").
#' @param fallback_string Character. String to return when validation fails (default `character()`).
#' @param checked,not_checked Optional string. When the categories of a variable exactly match
#'   these two values, the comparison is always made on `checked` — mirroring the visual convention
#'   in the bar chart where the checked category is rendered in colour on the left.
#'   Defaults to `NULL` (no checkbox handling). If `NULL`, the values are auto-detected from
#'   `global_settings_get("girafe")$checked` / `$not_checked` as a fallback.
#' @param reverse Logical. If TRUE, reverses the order of the output text summaries (default FALSE).
#' @param glue_str_pos Character vector. Templates for positive differences (group_1 > group_2).
#'   Available placeholders: `{var}`, `{group_1}`, `{group_2}`, `{selected_categories}`.
#' @param glue_str_neg Character vector. Templates for negative differences (group_2 > group_1).
#'   Same placeholders as `glue_str_pos`.
#'
#' @return A character vector of text summaries, one per variable with meaningful differences.
#'   Returns empty character vector if no plots provided or no meaningful differences found.
#'
#' @details
#' The function compares proportions between two groups for each variable in the plot data.
#' One template is randomly selected from the provided vectors for variety in output text.
#'
#' **Checkbox (checked/not_checked) variables**: When `checked` and `not_checked` are both
#' strings, any variable whose categories exactly match that pair is treated as a checkbox
#' variable.  For such variables the comparison is always made on the `checked` category,
#' regardless of `flip_to_lowest_categories`.  This mirrors the visual convention in the bar
#' chart where the checked category is rendered in colour on the left — the semantically
#' meaningful side — even though its `.category_order` may not be the highest.
#' If `checked`/`not_checked` are `NULL`, the function auto-detects them from
#' `global_settings_get("girafe")$checked` / `$not_checked`.
#'
#' @examples
#' \dontrun{
#' # Create sample plot data
#' plot_data_1 <- data.frame(
#'   .variable_label = rep("Job satisfaction", 3),
#'   .category = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High")),
#'   .category_order = 1:3,
#'   .proportion = c(0.2, 0.3, 0.5)
#' )
#'
#' plot_data_2 <- data.frame(
#'   .variable_label = rep("Job satisfaction", 3),
#'   .category = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High")),
#'   .category_order = 1:3,
#'   .proportion = c(0.3, 0.4, 0.3)
#' )
#'
#' plots <- list(
#'   list(data = plot_data_1),
#'   list(data = plot_data_2)
#' )
#'
#' # Generate text summaries
#' txt_from_cat_mesos_plots(plots, min_prop_diff = 0.10)
#'
#' # Compare lowest categories instead
#' txt_from_cat_mesos_plots(
#'   plots,
#'   flip_to_lowest_categories = TRUE,
#'   min_prop_diff = 0.05
#' )
#' }
#'
#' @export
txt_from_cat_mesos_plots <- function(
  plots,
  min_prop_diff = .10,
  n_highest_categories = 1,
  flip_to_lowest_categories = FALSE,
  checked = NULL,
  not_checked = NULL,
  digits = 2,
  selected_categories_last_split = " or ",
  fallback_string = character(),
  reverse = FALSE,
  glue_str_pos = c(
    paste0(
      "For {var}, the target group has a higher proportion of respondents ",
      "({group_1}) than all others ({group_2}) who answered {selected_categories}."
    ),
    paste0(
      "More respondents answered {selected_categories} for {var} in the ",
      "target group ({group_1}) than in other groups ({group_2})."
    ),
    paste0(
      "The statement {var} shows {selected_categories} responses are more ",
      "common in the target group ({group_1}) compared to others ({group_2})."
    )
  ),
  glue_str_neg = c(
    paste0(
      "For {var}, the target group has a lower proportion of respondents ",
      "({group_1}) than all others ({group_2}) who answered {selected_categories}."
    ),
    paste0(
      "Fewer respondents answered {selected_categories} for {var} in the ",
      "target group ({group_1}) than in other groups ({group_2})."
    ),
    paste0(
      "The statement {var} shows {selected_categories} responses are less ",
      "common in the target group ({group_1}) compared to others ({group_2})."
    )
  )
) {
  args <- check_options(
    call = match.call(),
    ignore_args = .saros.env$ignore_args,
    defaults_env = global_settings_get(fn_name = "txt_from_cat_mesos_plots"),
    default_values = formals(txt_from_cat_mesos_plots)
  )

  # Re-insert plots after check_options (like data in other functions)
  args$plots <- plots

  check_string(args$checked, null_allowed = TRUE, arg = "checked")
  check_string(args$not_checked, null_allowed = TRUE, arg = "not_checked")

  # Auto-detect checked/not_checked from girafe global settings when not
  # provided explicitly or via txt_from_cat_mesos_plots global settings.
  if (is.null(args$checked) || is.null(args$not_checked)) {
    girafe_fallback <- global_settings_get("girafe")
    if (is.null(args$checked)) {
      args$checked <- girafe_fallback$checked
    }
    if (is.null(args$not_checked)) {
      args$not_checked <- girafe_fallback$not_checked
    }
  }

  # Validate plots argument
  if (!is.list(args$plots)) {
    cli::cli_warn(
      c(
        "{.arg plots} must be a list, not {.cls {class(args$plots)}}.",
        "i" = "Returning {.val {args$fallback_string}}."
      )
    )
    return(args$fallback_string)
  }

  if (length(args$plots) < 2) {
    cli::cli_warn(
      c(
        "{.arg plots} must contain at least 2 elements, not {length(args$plots)}.",
        "i" = "Returning {.val {args$fallback_string}}."
      )
    )
    return(args$fallback_string)
  }

  # Check that each element has a data component
  has_data <- vapply(
    args$plots,
    function(x) {
      !is.null(x) && (inherits(x, "data.frame") || !is.null(x$data))
    },
    logical(1)
  )

  if (!all(has_data)) {
    missing_data <- which(!has_data)
    cli::cli_warn(
      c(
        "{.arg plots} elements {missing_data} do not contain plot data.",
        "i" = "Each element must be a data frame or have a {.field data} component.",
        "i" = "Returning {.val {args$fallback_string}}."
      )
    )
    return(args$fallback_string)
  }

  # Extract data - handle both data frames and plot objects with $data
  dat_1 <- if (inherits(args$plots[[1]], "data.frame")) {
    args$plots[[1]]
  } else {
    args$plots[[1]]$data
  }

  dat_2 <- if (inherits(args$plots[[2]], "data.frame")) {
    args$plots[[2]]
  } else {
    args$plots[[2]]$data
  }

  # Check if .category_order exists and has non-NA values
  if (
    !".category_order" %in% colnames(dat_1) || all(is.na(dat_1$.category_order))
  ) {
    cli::cli_warn(
      c(
        "{.field .category_order} column is missing or all NA in plot data.",
        "i" = "Cannot determine category ordering for comparison.",
        "i" = "Returning {.val {args$fallback_string}}."
      )
    )
    return(args$fallback_string)
  }

  # Use original variable label if available (when hide_axis_text_if_single_variable = TRUE)
  # Check both datasets to ensure we use a column that works for both
  # This may normalize the datasets to use the same column
  normalized <- get_common_variable_label_column(dat_1, dat_2)
  var_col <- normalized$var_col
  dat_1 <- normalized$dat_1
  dat_2 <- normalized$dat_2

  # Get unique variables to process
  unique_vars <- dat_1[[var_col]] |>
    as.character() |>
    unique()

  # Process each variable separately to handle different category counts
  out <- lapply(unique_vars, function(var) {
    # Get categories for this specific variable from first dataset
    var_categories <- dat_1 |>
      dplyr::filter(.data[[var_col]] == var) |>
      dplyr::distinct(.data$.category, .keep_all = TRUE) |>
      dplyr::filter(!is.na(.data$.category_order))

    n_categories <- nrow(var_categories)

    # Detect checkbox scenario: when the variable's categories exactly match the
    # checked/not_checked pair from girafe global settings, always compare on the
    # checked category.  In the bar chart, checked is rendered to the LEFT in colour
    # (the visually prominent position), but its .category_order is not necessarily
    # the highest, so the normal order-based selection would pick not_checked instead.
    is_checkbox_var <- rlang::is_string(args$checked) &&
      rlang::is_string(args$not_checked) &&
      setequal(
        as.character(var_categories$.category),
        c(args$checked, args$not_checked)
      )

    if (is_checkbox_var) {
      # For checkbox variables, always report on the checked category only.
      selected_categories <- as.character(args$checked)
    } else if (n_categories <= args$n_highest_categories) {
      # For variables with few categories, use only the highest/lowest single category
      selected_categories <- var_categories |>
        dplyr::filter(
          .data$.category_order ==
            if (isFALSE(args$flip_to_lowest_categories)) {
              max(.data$.category_order, na.rm = TRUE)
            } else {
              min(.data$.category_order, na.rm = TRUE)
            }
        ) |>
        dplyr::pull(.data$.category) |>
        as.character() |>
        unique()
    } else {
      # For variables with many categories, use n_highest_categories
      selected_categories <- var_categories |>
        dplyr::filter(
          .data$.category_order %in%
            if (isFALSE(args$flip_to_lowest_categories)) {
              (max(c(
                1,
                max(.data$.category_order, na.rm = TRUE) -
                  args$n_highest_categories +
                  1
              )):max(.data$.category_order, na.rm = TRUE))
            } else {
              min(.data$.category_order, na.rm = TRUE):(min(c(
                max(.data$.category_order, na.rm = TRUE),
                args$n_highest_categories
              )))
            }
        ) |>
        dplyr::pull(.data$.category) |>
        as.character() |>
        unique()
    }

    # Get proportions for both groups
    props <- list(group_1 = dat_1, group_2 = dat_2) |>
      lapply(function(.x) {
        get_prop_for_highest_categories(
          plot_data = .x,
          var = var,
          selected_categories = selected_categories,
          var_col = var_col
        )
      }) |>
      dplyr::bind_rows(.id = "group") |>
      tidyr::pivot_wider(names_from = "group", values_from = "value")

    # Add selected categories for this variable
    props$selected_categories <- cli::ansi_collapse(
      selected_categories,
      sep = "; ",
      last = args$selected_categories_last_split,
      trunc = 10,
      sep2 = args$selected_categories_last_split
    )

    props
  }) |>
    dplyr::bind_rows()

  # Generate text based on differences
  out[["txt"]] <- dplyr::case_when(
    out[[2]] > out[[3]] + args$min_prop_diff ~
      sample(args$glue_str_pos, size = nrow(out), replace = TRUE),
    out[[3]] > out[[2]] + args$min_prop_diff ~
      sample(args$glue_str_neg, size = nrow(out), replace = TRUE),
    .default = ""
  )
  out[[2]] <- round(out[[2]], digits = args$digits)
  out[[3]] <- round(out[[3]], digits = args$digits)

  for (i in seq_len(nrow(out))) {
    out[i, "txt"] <- glue::glue_data(.x = out[i, ], out[i, "txt"][[1]])
  }
  out <- stringi::stri_omit_empty_na(out$txt)
  if (args$reverse) {
    out <- rev(out)
  }
  out
}
