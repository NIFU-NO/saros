#' @export
make_content.cat_plot_html <-
  function(type,
           ...) {
    dots <- rlang::list2(...)

    data <- dots$data_summary

    if (dots$showNA %in% c("never") &&
      length(levels(data[[".category"]])) == 0) {
      showna_arg_str <- "showNA = 'always'"
      cli::cli_warn("Variables contain NA on all dep-by-indep cells. Returning an empty plot. Consider {.arg {showna_arg_str}} or check your data.")
      return(ggplot2::ggplot() +
        ggplot2::theme_void())
    }

    indep_vars <- colnames(data)[!colnames(data) %in%
      .saros.env$summary_data_sort2]

    hide_axis_text <-
      isTRUE(dots$hide_axis_text_if_single_variable) &&
        length(indep_vars) == 0 &&
        dplyr::n_distinct(data$.variable_label, na.rm = TRUE) == 1

    if (isTRUE(hide_axis_text)) {
      data$.variable_label <- ""
    }

    # max_nchar_cat <- max(c(nchar(levels(data$.category)), 0), na.rm = TRUE)
    # browser()
    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")
    x <- if (length(indep_vars) == 1 && isFALSE(dots$inverse)) {
      indep_vars
    } else if (all(!is.na(data[[".variable_label"]]))) {
      ".variable_label"
    } else {
      ".variable_name"
    }

    if (!is.ordered(data[[x]])) {
      data[[x]] <- reorder_within(
        x = data[[x]],
        by = ifelse(is.na(data[[".sum_value"]]), 0, data[[".sum_value"]]),
        within = data[, c(x)],
        fun = mean, na.rm = TRUE
      )
    }

    p <-
      dplyr::mutate(data,
        .id = seq_len(nrow(data)),
        .tooltip = # Tooltip contains all data except variable name
          sprintf(
            fmt = stringi::stri_c("%s",
              "n = %.0f",
              stringi::stri_c("P = %.", dots$digits, "f%%", ignore_null = TRUE),
              "%s",
              "n (valid) = %.0f",
              "N (valid) = %.0f",
              sep = "\n", ignore_null = TRUE
            ),
            .data$.category,
            .data$.count,
            .data$.proportion * 100,
            .data$.variable_label,
            .data$.count_per_indep_group,
            .data$.count_per_dep
          ),
        .tooltip = ifelse(!is.na(.data$.tooltip) & rlang::is_string(indep_vars),
          yes = sprintf(
            fmt = stringi::stri_c("%s", "%s", sep = "\n", ignore_null = TRUE),
            .data$.tooltip,
            .data[[indep_vars]]
          ),
          no = .data$.tooltip
        ),
        .onclick = sprintf(
          fmt = stringi::stri_c("%s", "Variable: %s", sep = "\n", ignore_null = TRUE),
          .data$.tooltip, .data$.variable_name
        ),
        .onclick = paste0('alert(\"', .data[[".onclick"]], '\");'),
        .onclick = stringi::stri_replace_all_regex(.data$.onclick,
          pattern = "\n",
          replacement = "\\\\n"
        )
      ) |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data[[if (prop_family) ".proportion" else stringi::stri_c(".", dots$data_label, ignore_null = TRUE)]],
          x = .data[[x]],
          fill = .data$.category,
          group = .data$.category,
          label = .data$.data_label,
          data_id = .data$.id,
          onclick = .data$.onclick
        ),
        cumulative = TRUE
      ) +
      ggiraph::geom_col_interactive(
        mapping = ggplot2::aes(tooltip = .data$.tooltip),
        position = if (prop_family) ggplot2::position_stack(reverse = TRUE) else ggplot2::position_dodge(width = .9),
        na.rm = TRUE,
        show.legend = TRUE
      ) +
      ggiraph::geom_text_interactive(
        mapping = ggplot2::aes(
          y = if (prop_family) .data[[".proportion"]] else .data[[".count"]] * .5,
          colour = ggplot2::after_scale(x = hex_bw(.data$fill))
        ),
        position = if (prop_family) ggplot2::position_stack(vjust = 0.5, reverse = TRUE) else ggplot2::position_dodge(width = .9),
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      ggplot2::scale_y_continuous(
        limits = c(-.003, if (prop_family) 1.015 else NA),
        expand = c(0, 0.03),
        labels = if (percentage) function(x) stringi::stri_c(ignore_null = TRUE, x * 100, "%") else ggplot2::waiver()
      ) +
      ggiraph::scale_fill_discrete_interactive(
        name = "",
        data_id = function(x) x,
        tooltip = function(x) x,
        drop = FALSE
      ) +
      ggiraph::scale_colour_discrete_interactive(
        guide = FALSE,
        drop = FALSE
      ) +
      scale_x_reorder(limits = rev, x_axis_label_width = dots$x_axis_label_width) +
      ggplot2::guides(
        fill = ggiraph::guide_legend_interactive(data_id = "fill.guide"),
        colour = "none"
      )

    if (length(indep_vars) > 1L ||
      (length(indep_vars) >= 1L &&
        (dplyr::n_distinct(data$.variable_label) > 1 ||
          (dplyr::n_distinct(data$.variable_label) == 1 &&
            isFALSE(dots$hide_axis_text_if_single_variable))))) {
      if (isFALSE(dots$inverse)) {
        lab <- ".variable_label"
        if (is.factor(p$data[[lab]])) {
          levels(p$data[[lab]]) <- string_wrap(levels(p$data[[lab]]), width = dots$strip_width)
        } else {
          p$data[[lab]] <- string_wrap(p$data[[lab]], width = dots$strip_width)
        }

        p <- p +
          ggiraph::facet_grid_interactive(
            rows = ggplot2::vars(.data$.variable_label),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(
                data_id = .data[[lab]],
                tooltip = .data[[lab]]
              )
            ),
            interactive_on = "text",
            switch = "y", scales = "free", space = "free_y"
          )
      } else if (isTRUE(dots$inverse)) {
        for (lab in indep_vars) {
          if (is.factor(p$data[[lab]])) {
            levels(p$data[[lab]]) <- string_wrap(levels(p$data[[lab]]), width = dots$strip_width)
          } else {
            p$data[[lab]] <- string_wrap(p$data[[lab]], width = dots$strip_width)
          }
        }

        p <- p +
          ggiraph::facet_grid_interactive(
            rows = ggplot2::vars(.data[[indep_vars]]),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(
                data_id = .data[[indep_vars]],
                tooltip = .data[[indep_vars]]
              )
            ),
            interactive_on = "text",
            switch = "y", scales = "free_y", space = "free_y"
          )
      }
    }

    if (isFALSE(dots$vertical) && length(levels(p$data[[".category"]])) > 0) {
      p + ggplot2::coord_flip()
    } else {
      p
    }
  }
