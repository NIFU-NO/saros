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

#' @export
make_content.int_plot_html <-
  function(type, ..., min_value = -Inf, max_value = Inf) {
    dots <- rlang::list2(...)

    dep_labels <- get_raw_labels(dots$data, col_pos = dots$dep)
    dep_axis_text_var <- if (length(dots$dep) == length(dep_labels)) {
      ".variable_label"
    } else {
      ".variable_name"
    }
    dep_labels <- data.frame(
      .variable_name = names(dep_labels),
      .variable_label = unname(dep_labels)
    )
    if (!is.null(dots$label_separator)) {
      dep_labels[[".variable_label"]] <-
        keep_subitem(
          dep_labels[[".variable_label"]],
          label_separator = dots$label_separator,
          ordered = FALSE
        )
    }

    x_axis_var <- dep_axis_text_var
    facet_var <- character()
    if (length(dots$indep) == 1 && isFALSE(dots$inverse)) {
      x_axis_var <- dots$indep
      dots$data[[dots$indep]] <-
        strip_wrap_var(dots$data[[dots$indep]], width = dots$x_axis_label_width)
      facet_var <- dep_axis_text_var
      dep_labels[[".variable_label"]] <-
        strip_wrap_var(
          dep_labels[[".variable_label"]],
          width = dots$strip_width
        )
    }
    if (length(dots$indep) == 1 && isTRUE(dots$inverse)) {
      x_axis_var <- dep_axis_text_var
      dep_labels[[".variable_label"]] <-
        strip_wrap_var(
          dep_labels[[".variable_label"]],
          width = dots$x_axis_label_width
        )

      facet_var <- dots$indep
    }

    desc_tbl <- simple_descriptives(
      data = dots$data,
      y_var = dots$dep,
      x_var = dots$indep,
      na.rm = TRUE,
      label_separator = dots$label_separator
    ) |>
      add_label_tooltip()

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
        scale = "count"
      ) +
      ggiraph::geom_boxplot_interactive(
        staplewidth = .1,
        width = .1,
        fill = "white"
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
        inherit.aes = FALSE
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
  }
