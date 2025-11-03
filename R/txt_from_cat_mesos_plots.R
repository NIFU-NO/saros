get_prop_for_highest_categories <- function(
  plot_data,
  var,
  selected_categories
) {
  data.frame(
    var = var,
    value = plot_data |>
      dplyr::filter(
        .data$.variable_label == var,
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
#' @param flip_to_lowest_categories Logical. If TRUE, compare lowest categories instead
#'   of highest (default FALSE).
#' @param digits Integer. Number of decimal places for rounding proportions (default 2).
#' @param selected_categories_last_split Character. Separator for the last item when
#'   listing multiple categories (default " or ").
#' @param fallback_string Character. String to return when validation fails (default `character()`).
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
  digits = 2,
  selected_categories_last_split = " or ",
  fallback_string = character(),
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
  # Validate plots argument
  if (!is.list(plots)) {
    cli::cli_warn(
      c(
        "{.arg plots} must be a list, not {.cls {class(plots)}}.",
        "i" = "Returning {.val {fallback_string}}."
      )
    )
    return(fallback_string)
  }

  if (length(plots) < 2) {
    cli::cli_warn(
      c(
        "{.arg plots} must contain at least 2 elements, not {length(plots)}.",
        "i" = "Returning {.val {fallback_string}}."
      )
    )
    return(fallback_string)
  }

  # Check that each element has a data component
  has_data <- vapply(
    plots,
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
        "i" = "Returning {.val {fallback_string}}."
      )
    )
    return(fallback_string)
  }

  # Extract data - handle both data frames and plot objects with $data
  dat_1 <- if (inherits(plots[[1]], "data.frame")) {
    plots[[1]]
  } else {
    plots[[1]]$data
  }

  dat_2 <- if (inherits(plots[[2]], "data.frame")) {
    plots[[2]]
  } else {
    plots[[2]]$data
  }

  selected_categories <-
    dat_1 |>
    dplyr::distinct(.data$.category, .keep_all = TRUE) |>
    dplyr::filter(
      .data$.category_order %in%
        if (isFALSE(flip_to_lowest_categories)) {
          (max(c(
            1,
            max(.data$.category_order) - n_highest_categories + 1
          )):max(.data$.category_order))
        } else if (isTRUE(flip_to_lowest_categories)) {
          min(.data$.category_order):(min(c(
            max(.data$.category_order),
            n_highest_categories
          )))
        }
    ) |>
    dplyr::pull(.data$.category) |>
    as.character() |>
    unique()

  out <-
    unique(as.character(dat_1$.variable_label)) |>
    lapply(function(var) {
      list(group_1 = dat_1, group_2 = dat_2) |>
        lapply(function(.x) {
          get_prop_for_highest_categories(
            plot_data = .x,
            var = var,
            selected_categories = selected_categories
          )
        }) |>
        dplyr::bind_rows(.id = "group")
    }) |>
    dplyr::bind_rows() |>
    tidyr::pivot_wider(names_from = "group", values_from = "value")
  out[["txt"]] <- dplyr::case_when(
    out[[2]] > out[[3]] + min_prop_diff ~
      sample(glue_str_pos, size = nrow(out), replace = TRUE),
    out[[3]] > out[[2]] + min_prop_diff ~
      sample(glue_str_neg, size = nrow(out), replace = TRUE),
    .default = ""
  )
  out[[2]] <- round(out[[2]], digits = digits)
  out[[3]] <- round(out[[3]], digits = digits)

  out[["selected_categories"]] <- cli::ansi_collapse(
    selected_categories,
    sep = "; ",
    last = selected_categories_last_split,
    trunc = 10,
    sep2 = selected_categories_last_split
  )

  for (i in seq_len(nrow(out))) {
    out[i, "txt"] <- glue::glue_data(.x = out[i, ], out[i, "txt"][[1]])
  }
  stringi::stri_omit_empty_na(out$txt)
}
