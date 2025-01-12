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
    dep_var <- if (all(!is.na(data[[".variable_label"]]))) ".variable_label" else ".variable_name"

    indep_vars <- colnames(data)[!colnames(data) %in%
      .saros.env$summary_data_sort2]

    hide_axis_text <-
      isTRUE(dots$hide_axis_text_if_single_variable) &&
        length(indep_vars) == 0 &&
        dplyr::n_distinct(data[[dep_var]], na.rm = TRUE) == 1

    if (isTRUE(hide_axis_text)) {
      data[[dep_var]] <- ""
    }

    # max_nchar_cat <- max(c(nchar(levels(data$.category)), 0), na.rm = TRUE)

    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")

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
      data[[x_axis_var]] <- reorder_within(
        x = data[[x_axis_var]],
        by = ifelse(is.na(data[[".sum_value"]]), 0, data[[".sum_value"]]),
        within = data[, facet_var, drop = FALSE],
        fun = mean, na.rm = TRUE
      )
    }
    tooltip_glue_specs <-
      c(
        "Category" = "{(.category)}",
        "Dependent" = paste0("{(", dep_var, ")}"),
        "Independent group(s)" =
          if (length(indep_vars)) {
            paste0("{stringi::stri_replace_all_regex(", indep_vars, ", pattern = '___.+$', replacement = '')}", collapse = ", ")
          },
        "Percentage" = "{(.percentage)}",
        "n (cell)" = "{(.count)}",
        "N (per independent var; valid)" = "{(.count_per_indep_group)}",
        "N (total; valid)" = "{(.count_per_dep)}"
      )

    names(tooltip_glue_specs) <- paste0(names(tooltip_glue_specs), ":")
    #    max_width_prefix <- max(stringi::stri_length(names(tooltip_glue_specs)), na.rm = TRUE) + 1
    #    names(tooltip_glue_specs) <- stringi::stri_pad_right(names(tooltip_glue_specs), pad = " ", width = max_width_prefix)
    #    names(tooltip_glue_specs) <- convert_trailing_chars(names(tooltip_glue_specs), from_char = " ", to_char = "&nbsp;")
    tooltip_glue_specs <- setNames(paste0("<b>", unname(tooltip_glue_specs), "</b>"), nm = names(tooltip_glue_specs))
    tooltip_glue_specs <- paste0(names(tooltip_glue_specs), "&nbsp;", unname(tooltip_glue_specs))

    tooltip_glue_spec <- stringi::stri_c(tooltip_glue_specs, collapse = "\n", ignore_null = TRUE)
    ##
    onclick_glue_spec <- c(
      tooltip_glue_specs,
      "Dependent variable name: {(.variable_name)}",
      paste0("Independent variable name: ", paste0(indep_vars, collapse = ", "))
    )
    onclick_glue_spec <- stringi::stri_replace_all_regex(onclick_glue_spec, pattern = "</*[bi]>|</*strong>|</*em>", replacement = "")
    onclick_glue_spec <- stringi::stri_c(onclick_glue_spec, collapse = "\n", ignore_null = TRUE)

    p_data <-
      dplyr::mutate(data,
        .id = seq_len(nrow(data)),
        .percentage = round(.data$.proportion * 100, digits = dots$digits),
        .tooltip = glue::glue(tooltip_glue_spec),
        .onclick = glue::glue(onclick_glue_spec),
        .onclick = paste0('alert(\"', .data[[".onclick"]], '\");'),
        .onclick = stringi::stri_replace_all_regex(.data$.onclick,
          pattern = "\n",
          replacement = "\\\\n"
        )
      )
    p <-
      p_data |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data[[if (prop_family) ".proportion" else stringi::stri_c(".", dots$data_label, ignore_null = TRUE)]],
          x = .data[[x_axis_var]],
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
      scale_x_reorder(limits = rev, sep = if (needs_reorder) "___", x_axis_label_width = dots$x_axis_label_width) +
      ggplot2::guides(
        fill = ggiraph::guide_legend_interactive(data_id = "fill.guide"),
        colour = "none"
      )

    if (length(indep_vars) > 1L ||
      (length(indep_vars) >= 1L &&
        (dplyr::n_distinct(data$.variable_name) > 1 ||
          (dplyr::n_distinct(data$.variable_name) == 1 &&
            isFALSE(dots$hide_axis_text_if_single_variable))))) {
      if (isFALSE(dots$inverse)) {
        lab <- dep_var
        if (is.factor(p$data[[lab]])) {
          levels(p$data[[lab]]) <- string_wrap(levels(p$data[[lab]]), width = dots$strip_width)
        } else {
          p$data[[lab]] <- string_wrap(p$data[[lab]], width = dots$strip_width)
        }

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
            rows = ggplot2::vars(!!!rlang::syms(indep_vars)),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(
                data_id = .data[[indep_vars[1]]],
                tooltip = .data[[indep_vars[1]]]
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
