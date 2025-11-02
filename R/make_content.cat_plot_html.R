#' @export
make_content.cat_plot_html <-
  function(type, ...) {
    dots <- rlang::list2(...)

    data <- dots$data_summary

    if (
      dots$showNA %in% c("never") && length(levels(data[[".category"]])) == 0
    ) {
      showna_arg_str <- "showNA = 'always'"

      cli::cli_warn(
        "Variables contain NA on all dep-by-indep cells. Returning an empty plot. Consider {.arg {showna_arg_str}} or check your data."
      )
      return(
        ggplot2::ggplot() +
          ggplot2::theme_void()
      )
      showna_arg_str
    }

    # Process binary category colors for cat_plot_html
    data <- process_binary_category_colors(
      data,
      showNA = dots$showNA,
      colour_2nd_binary_cat = dots$colour_2nd_binary_cat
    )

    dep_var <- get_data_display_column(data)
    stat_col <- stringi::stri_c(
      ".",
      dots$data_label,
      ignore_null = TRUE
    )

    indep_vars <- colnames(data)[
      !colnames(data) %in%
        .saros.env$summary_data_sort2
    ]

    hide_axis_text <-
      isTRUE(dots$hide_axis_text_if_single_variable) &&
      length(indep_vars) == 0 &&
      dplyr::n_distinct(data[[dep_var]], na.rm = TRUE) == 1

    if (isTRUE(hide_axis_text)) {
      data[[dep_var]] <- ""
    }

    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in%
      c("percentage", "percentage_bare", "proportion")
    if (prop_family) {
      stat_col <- ".proportion"
    }

    x_axis_var <- dep_var
    facet_var <- character()
    if (length(indep_vars) == 1 && isFALSE(dots$inverse)) {
      x_axis_var <- indep_vars
      facet_var <- dep_var
    }
    if (length(indep_vars) == 1 && isTRUE(dots$inverse)) {
      x_axis_var <- dep_var
      facet_var <- indep_vars
    }

    needs_reorder <- !is.ordered(data[[x_axis_var]]) && length(indep_vars) > 0
    if (isTRUE(needs_reorder)) {
      # When the x-axis is the independent variable, leverage centralized
      # .indep_order (which already respects descend_indep and per-dep logic).
      if (
        identical(x_axis_var, indep_vars) && ".indep_order" %in% names(data)
      ) {
        data[[x_axis_var]] <- reorder_within(
          x = data[[x_axis_var]],
          by = data[[".indep_order"]],
          within = data[, facet_var, drop = FALSE],
          fun = mean,
          na.rm = TRUE
        )
      } else {
        # Fallback: order by .sum_value within each facet (legacy behavior)
        data[[x_axis_var]] <- reorder_within(
          x = data[[x_axis_var]],
          by = ifelse(is.na(data[[".sum_value"]]), 0, data[[".sum_value"]]),
          within = data[, facet_var, drop = FALSE],
          fun = mean,
          na.rm = TRUE
        )
      }
    }
    tooltip_glue_specs <-
      c(
        "Category" = "{(.category)}",
        "Dependent" = paste0("{(", dep_var, ")}"),
        "Independent group(s)" = if (length(indep_vars)) {
          paste0(
            "{stringi::stri_replace_all_regex(",
            indep_vars,
            ", pattern = '___.+$', replacement = '')}",
            collapse = ", "
          )
        },
        "Percentage" = "{(.percentage)}",
        "n (cell)" = "{(.count)}",
        "N (per independent var; valid)" = "{(.count_per_indep_group)}",
        "N (total; valid)" = "{(.count_per_dep)}"
      )

    names(tooltip_glue_specs) <- paste0(names(tooltip_glue_specs), ":")

    tooltip_glue_specs <- stats::setNames(
      paste0("<b>", unname(tooltip_glue_specs), "</b>"),
      nm = names(tooltip_glue_specs)
    )
    tooltip_glue_specs <- paste0(
      names(tooltip_glue_specs),
      " ",
      unname(tooltip_glue_specs)
    )

    tooltip_glue_spec <- stringi::stri_c(
      tooltip_glue_specs,
      collapse = "\n",
      ignore_null = TRUE
    )
    ##
    onclick_glue_spec <- stats::setNames(
      paste0("<b>", unname(tooltip_glue_specs), "</b>"),
      nm = names(tooltip_glue_specs)
    )
    onclick_glue_spec <- paste0(
      names(onclick_glue_spec),
      " ",
      unname(onclick_glue_spec)
    )
    onclick_glue_spec <- c(
      onclick_glue_spec,
      "Dependent variable name: {(.variable_name)}",
      paste0("Independent variable name: ", paste0(indep_vars, collapse = ", "))
    )
    onclick_glue_spec <- stringi::stri_replace_all_regex(
      onclick_glue_spec,
      pattern = "</*[bi]>|</*strong>|</*em>",
      replacement = ""
    )
    onclick_glue_spec <- stringi::stri_c(
      onclick_glue_spec,
      collapse = "\n",
      ignore_null = TRUE
    )

    p_data <-
      dplyr::mutate(
        data,
        .id = seq_len(nrow(data)),
        .percentage = round(.data$.proportion * 100, digits = dots$digits),
        .tooltip = glue::glue(tooltip_glue_spec),
        .onclick = glue::glue(onclick_glue_spec),
        .onclick = paste0('alert(\"', .data[[".onclick"]], '\");'),
        .onclick = stringi::stri_replace_all_regex(
          .data$.onclick,
          pattern = "\n",
          replacement = "\\\\n"
        )
      )
    p <-
      p_data |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data[[stat_col]],
          x = .data[[x_axis_var]],
          fill = .data$.category,
          group = .data$.category,
          label = .data$.data_label,
          data_id = .data$.id,
          onclick = .data$.onclick
        )
      ) +
      ggiraph::geom_col_interactive(
        mapping = ggplot2::aes(tooltip = .data$.tooltip),
        position = if (prop_family) {
          ggplot2::position_stack(reverse = TRUE)
        } else {
          ggplot2::position_dodge(width = .9)
        },
        na.rm = TRUE,
        show.legend = TRUE
      ) +
      ggiraph::geom_text_interactive(
        mapping = ggplot2::aes(
          y = if (prop_family) {
            .data[[stat_col]]
          } else {
            if (dots$data_label_position == "center") {
              .data[[stat_col]] * 0.5 # Middle of bar
            } else if (dots$data_label_position == "bottom") {
              pmax(.data[[stat_col]] * 0.05, max(.data[[stat_col]]) * 0.01) # Near bottom
            } else if (dots$data_label_position == "top") {
              .data[[stat_col]] * 0.9 # Near top but inside
            } else if (dots$data_label_position == "above") {
              .data[[stat_col]] * 1.05 # Above bar
            } else {
              .data[[stat_col]] * 0.5 # Default to center
            }
          },
          colour = ggplot2::after_scale(
            x = hex_bw(.data$fill, na_colour = dots$colour_na)
          )
        ),
        position = if (prop_family) {
          if (dots$data_label_position == "center") {
            ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
          } else if (dots$data_label_position == "bottom") {
            ggplot2::position_stack(vjust = 0.1, reverse = TRUE)
          } else if (dots$data_label_position == "top") {
            ggplot2::position_stack(vjust = 0.9, reverse = TRUE)
          } else if (dots$data_label_position == "above") {
            ggplot2::position_stack(vjust = 1.1, reverse = TRUE)
          } else {
            ggplot2::position_stack(vjust = 0.5, reverse = TRUE) # Default
          }
        } else {
          ggplot2::position_dodge(width = 0.9, reverse = FALSE)
        },
        hjust = if (prop_family) {
          0
        } else {
          if (dots$data_label_position == "center") {
            0.5
          } else if (dots$data_label_position == "bottom") {
            0.1
          } else if (dots$data_label_position == "top") {
            0.9
          } else if (dots$data_label_position == "above") {
            1.1
          } else {
            0.5
          }
        },
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      ggplot2::scale_y_continuous(
        limits = c(-.003, if (prop_family) 1.015 else NA),
        expand = c(0, 0.03),
        labels = if (percentage) {
          function(x) stringi::stri_c(ignore_null = TRUE, x * 100, "%")
        } else {
          ggplot2::waiver()
        }
      ) +
      ggiraph::scale_fill_discrete_interactive(
        name = "fill.guide",
        data_id = function(x) x,
        tooltip = function(x) x,
        drop = FALSE
      ) +
      ggiraph::scale_colour_discrete_interactive(
        guide = FALSE,
        drop = FALSE
      ) +
      scale_x_reorder(
        limits = rev,
        sep = if (needs_reorder) "___",
        x_axis_label_width = dots$x_axis_label_width
      ) +
      ggplot2::guides(
        fill = ggiraph::guide_legend_interactive(),
        colour = "none"
      )

    if (
      length(indep_vars) > 1L ||
        (length(indep_vars) >= 1L &&
          (dplyr::n_distinct(data$.variable_name) > 1 ||
            (dplyr::n_distinct(data$.variable_name) == 1 &&
              isFALSE(dots$hide_axis_text_if_single_variable))))
    ) {
      if (isFALSE(dots$inverse)) {
        lab <- dep_var
        p$data[[lab]] <- strip_wrap_var(p$data[[lab]], width = dots$strip_width)

        p <- p +
          ggiraph::facet_grid_interactive(
            rows = ggplot2::vars(.data[[dep_var]]),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(
                data_id = .data[[lab]],
                tooltip = .data[[lab]]
              )
            ),
            interactive_on = "text",
            switch = "y",
            scales = "free",
            space = "free_y"
          )
      } else if (isTRUE(dots$inverse)) {
        for (lab in indep_vars) {
          p$data[[lab]] <- strip_wrap_var(
            p$data[[lab]],
            width = dots$strip_width
          )
        }

        p <- p +
          ggiraph::facet_grid_interactive(
            rows = ggplot2::vars(!!!rlang::syms(indep_vars)),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(
                data_id = .data[[indep_vars[1]]],
                tooltip = .data[[indep_vars[1]]]
              )
            ),
            interactive_on = "text",
            switch = "y",
            scales = "free_y",
            space = "free_y"
          )
      }
    }

    if (isFALSE(dots$vertical) && length(levels(p$data[[".category"]])) > 0) {
      p + ggplot2::coord_flip()
    } else {
      p
    }
  }
