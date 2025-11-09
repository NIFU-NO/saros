add_label_tooltip <- function(
  desc_tbl,
  digits = 2,
  label_glue_exp = "Median: %.*f",
  tooltip_glue_exp = "M = %.*f, SD = %.*f\nMedian = %.*f, MAD = %.*f\nIQR = %.*f\nN = %d (valid), %d (miss)\nMin = %.*f, Max = %.*f"
) {
  desc_tbl |>
    dplyr::mutate(
      .tooltip = sprintf(
        tooltip_glue_exp,
        digits,
        .data$mean,
        digits,
        .data$sd,
        digits,
        .data$median,
        digits,
        .data$mad,
        digits,
        .data$iqr,
        .data$n_valid,
        .data$n_miss,
        digits,
        .data$min,
        digits,
        .data$max
      ),
      .label = sprintf(label_glue_exp, digits, .data$median)
    )
}

#' Apply label wrapping based on plot layout
#'
#' Helper function to consistently wrap variable labels based on whether
#' they appear on facet strips or x-axis, and whether inverse layout is used.
#'
#' @param data Data frame containing .variable_label column
#' @param indep_length Number of independent variables (0 or 1)
#' @param inverse Logical, whether inverse layout is used
#' @param strip_width Width for facet strip labels
#' @param x_axis_label_width Width for x-axis labels
#' @return Data frame with wrapped .variable_label column
#' @keywords internal
apply_label_wrapping <- function(
  data,
  indep_length,
  inverse,
  strip_width,
  x_axis_label_width
) {
  if (indep_length == 1 && isFALSE(inverse)) {
    # When faceting by variable labels, apply strip_width wrapping
    data[[".variable_label"]] <-
      strip_wrap_var(data[[".variable_label"]], width = strip_width)
  } else if (indep_length == 1 && isTRUE(inverse)) {
    # When variable labels are on x-axis, apply x_axis_label_width wrapping
    data[[".variable_label"]] <-
      strip_wrap_var(data[[".variable_label"]], width = x_axis_label_width)
  }
  data
}

#' @export
make_content.int_plot_html <-
  function(type, ..., min_value = -Inf, max_value = Inf) {
    dots <- rlang::list2(...)

    dep_labels <- get_raw_labels(dots$data, col_pos = dots$dep)
    dep_axis_text_var <- get_dep_display_column(length(dots$dep), dep_labels)
    dep_labels <- data.frame(
      .variable_name = names(dep_labels),
      .variable_label = unname(dep_labels)
    )
    # Always apply keep_subitem to ensure consistency with simple_descriptives()
    dep_labels[[".variable_label"]] <-
      keep_subitem(
        dep_labels[[".variable_label"]],
        label_separator = dots$label_separator,
        ordered = FALSE
      )

    x_axis_var <- dep_axis_text_var
    facet_var <- character()
    if (length(dots$indep) == 1 && isFALSE(dots$inverse)) {
      x_axis_var <- dots$indep
      dots$data[[dots$indep]] <-
        strip_wrap_var(dots$data[[dots$indep]], width = dots$x_axis_label_width)
      facet_var <- dep_axis_text_var
    }
    if (length(dots$indep) == 1 && isTRUE(dots$inverse)) {
      x_axis_var <- dep_axis_text_var
      facet_var <- dots$indep
    }

    # Apply label wrapping to dep_labels
    dep_labels <- apply_label_wrapping(
      dep_labels,
      indep_length = length(dots$indep),
      inverse = dots$inverse,
      strip_width = dots$strip_width,
      x_axis_label_width = dots$x_axis_label_width
    )

    desc_tbl <- simple_descriptives(
      data = dots$data,
      y_var = dots$dep,
      x_var = dots$indep,
      na.rm = TRUE,
      label_separator = dots$label_separator
    ) |>
      add_label_tooltip()

    # Apply same label wrapping to desc_tbl
    desc_tbl <- apply_label_wrapping(
      desc_tbl,
      indep_length = length(dots$indep),
      inverse = dots$inverse,
      strip_width = dots$strip_width,
      x_axis_label_width = dots$x_axis_label_width
    )

    p_data <-
      dots$data |>
      tidyr::pivot_longer(
        cols = tidyselect::all_of(dots$dep),
        names_to = ".variable_name",
        values_to = ".value"
      ) |>
      dplyr::left_join(y = dep_labels, by = ".variable_name")

    out <-
      p_data |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data[[".value"]],
          x = .data[[x_axis_var]],
          fill = .data[[x_axis_var]]
        )
      ) +
      ggiraph::geom_violin_interactive(
        scale = "count",
        na.rm = TRUE
      ) +
      ggiraph::geom_boxplot_interactive(
        staplewidth = .1,
        width = .1,
        fill = "white",
        na.rm = TRUE
      ) +
      ggiraph::geom_label_interactive(
        data = desc_tbl,
        mapping = ggplot2::aes(
          x = .data[[x_axis_var]],
          y = .data[["median"]],
          label = .data[[".label"]],
          tooltip = .data[[".tooltip"]]
        ),
        vjust = 1.5,
        fill = "white",
        inherit.aes = FALSE,
        na.rm = TRUE
      ) +
      ggplot2::guides(fill = "none")

    if (length(facet_var) > 0) {
      out <- out +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!rlang::sym(facet_var)),
          scales = "free",
          switch = "both"
        )
    }
    if (isFALSE(dots$vertical)) {
      out <- out + ggplot2::coord_flip()
    }

    out
  }
