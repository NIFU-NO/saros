#'
#' Create Single Interactive Categorical Plot with Univariates for Categorical Columns Sharing Same Categories
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @inheritParams gen_qmd_chapters
#' @param inverse Flag, defaults to FALSE. If TRUE, swaps x-axis and faceting.
#'
#' @importFrom rlang !!!
#'
#' @return ggiraph object, plottable with plot()
#' @keywords internal
# prep_uni_int_plot <-
#   function(data,
#            ...,
#            colour_palette = NULL,
#            inverse = FALSE,
#            call = rlang::caller_env()) {
#
#
#     dots <- update_dots(dots = rlang::list2(...),
#                         caller_function = "int_prop_plot")
#
#
#
#     if(is.null(colour_palette)) {
#       n <- length(levels(data[[".category"]]))
#       hues <- seq(15, 375, length = n + 1)
#       colour_palette <- grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
#     }
#
#
#     multi <- length(colour_palette) > 2
#
#     indep_vars <- colnames(data)[!colnames(data) %in%
#       .saros.env$summary_data_sort2]
#
#     hide_axis_text <-
#       isTRUE(dots$hide_axis_text_if_single_variable) &&
#       length(indep_vars) == 0 &&
#       dplyr::n_distinct(data[[".variable_label"]]) == 1
#
#     hide_legend <-
#       dplyr::n_distinct(data[[".category"]], na.rm = TRUE) == 2 &&
#         !is.null(dots$colour_2nd_binary_cat)
#
#     percentage <- dots$data_label %in% c("percentage", "percentage_bare")
#     prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")
#
#     p <-
#       data %>%
#       dplyr::mutate(.id = seq_len(nrow(.)),
#         .tooltip = # Tooltip is opposite of the regular display
#           if (prop_family) {
#             sprintf(fmt = "%s\nN = %.0f\n%s", .data[[".category"]], .data[[".count"]], .data[[".variable_label"]])
#           } else {
#             sprintf(
#               fmt = stringi::stri_c("%s\nP = %.", dots$digits, "f%%\n%s", ignore_null=TRUE),
#               .data[[".category"]], .data[[".proportion"]] * 100, .data[[".variable_label"]]
#             )
#           },
#         .onclick = paste0('alert(\"variable: ', .data[['.variable_name']], '\")')
#       ) %>%
#       ggplot2::ggplot(
#         mapping = ggplot2::aes(
#           y = .data[[if (prop_family) ".proportion" else stringi::stri_c(ignore_null=TRUE, ".", dots$data_label)]],
#           x = if(length(indep_vars) == 1 && isFALSE(inverse)) .data[[indep_vars]] else .data[[".variable_label"]],
#           fill = .data[[".category"]],
#           group = .data[[".category"]],
#           label = .data[[".data_label"]],
#           data_id = .data[[".id"]],
#           onclick = .data[[".onclick"]]
#         ),
#         cumulative = TRUE
#       ) +
#       ggiraph::geom_col_interactive(
#         mapping = ggplot2::aes(tooltip = .data[[".tooltip"]]), # BUG: Messes up order of categories if enabled.
#         position = ggplot2::position_stack(reverse = TRUE),
#         na.rm = TRUE
#       ) +
#       ggiraph::geom_text_interactive(
#         mapping = ggplot2::aes(
#           colour = ggplot2::after_scale(x = hex_bw(.data$fill))),
#         position = ggplot2::position_stack(vjust = .5, reverse = TRUE),
#         show.legend = FALSE, na.rm = TRUE
#       ) +
#       ggplot2::scale_y_continuous(
#         limits = c(-.003, if (prop_family) 1.015 else NA),
#         expand = c(0, 0),
#         labels = if (percentage) function(x) stringi::stri_c(ignore_null=TRUE, x * 100, "%") else ggplot2::waiver()
#       ) +
#       ggiraph::scale_fill_manual_interactive(
#         name = "",
#         values = colour_palette,
#         data_id = function(x) x,
#         tooltip = function(x) x,
#         drop = FALSE
#       ) +
#       ggiraph::scale_colour_manual_interactive(guide = FALSE, values = c("black", "white")) +
#       ggplot2::scale_x_discrete(limits = rev, labels = function(x) string_wrap(x, width = dots$x_axis_label_width)) +
#       ggplot2::guides(
#         fill = if (hide_legend) "none" else ggiraph::guide_legend_interactive(data_id = "fill.guide", byrow = TRUE),
#         colour = "none"
#       ) +
#       ggplot2::theme_classic() +
#       ggplot2::theme(
#         text = ggplot2::element_text(family = dots$font_family),
#         axis.text.y = if (hide_axis_text) ggplot2::element_blank() else ggiraph::element_text_interactive(data_id = "axis.text.y"),
#         plot.caption = ggiraph::element_text_interactive(data_id = "plot.caption", size = dots$main_font_size),
#         legend.position = "bottom",
#         legend.text = ggiraph::element_text_interactive(data_id = "legend.text", size = dots$main_font_size),
#         strip.placement = "outside",
#         strip.text.x = ggplot2::element_text(margin = ggplot2::margin(l = 0, t = 0, r = 0, b = 2, "cm")),
#         strip.text = ggiraph::element_text_interactive(data_id = "strip.text", angle = dots$strip_angle, hjust = .5, size = dots$main_font_size), # if(length(indep_vars)>0) ggplot2::element_blank() else
#         strip.background = ggiraph::element_rect_interactive(colour = NA)
#       ) +
#       ggplot2::labs(x = NULL, y = NULL)
#
#     if (length(indep_vars) > 1L || (length(indep_vars) >= 1L && dplyr::n_distinct(data[[".variable_label"]]) > 1)) {
#       if(!inverse) {
#       p <- p +
#         ggiraph::facet_grid_interactive(
#           rows = ggplot2::vars(.data[[".variable_label"]]),
#           labeller = ggiraph::labeller_interactive(
#             .mapping = ggplot2::aes(
#               data_id = .data[[".variable_label"]],
#               tooltip = .data[[".variable_label"]],
#               label = string_wrap(.data$.variable_label,
#                                   width = dots$x_axis_label_width
#               )
#             )
#           ),
#           interactive_on = "text",
#           switch = "y", scales = "free_y", space = "free_y"
#         )
#       } else {
#         p <- p +
#           ggiraph::facet_grid_interactive(
#             rows = ggplot2::vars(.data[[indep_vars]]),
#             labeller = ggiraph::labeller_interactive(
#               .mapping = ggplot2::aes(
#                 data_id = .data[[indep_vars]],
#                 tooltip = .data[[indep_vars]],
#                 label = string_wrap(.data[[indep_vars]],
#                                     width = dots$x_axis_label_width
#                 )
#               )
#             ),
#             interactive_on = "text",
#             switch = "y", scales = "free_y", space = "free_y"
#           )
#       }
#     }
#
#     if (!dots$vertical) {
#       p + ggplot2::coord_flip()
#     } else {
#       p
#     }
#   }



#' Embed Interactive Interval/Continuous Plot
#'
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @inheritParams gen_qmd_chapters
#' @param colour_palette Character vector of colour codes.
#' @param inverse Flag, defaults to FALSE. If TRUE, swaps x-axis and faceting.
#' @param html_interactive *Toggle interactive plot*
#'
#'   `scalar<logical>` // *default:* `TRUE` (`optional`)
#'
#'   Whether plot is to be interactive (ggiraph) or static (ggplot2).

# embed_int_prop_plot <-
#   function(data,
#            ...,
#            dep = tidyselect::everything(),
#            indep = NULL,
#            colour_palette = NULL,
#            mesos_group = NULL,
#            html_interactive = TRUE,
#            inverse = FALSE,
#            call = rlang::caller_env()) {
#
#     dots <- update_dots(dots = rlang::list2(...),
#                         caller_function = "int_prop_plot")
#
#
#     check_multiple_indep(data, indep = {{ indep }}, call = call)
#
#     dep_enq <- rlang::enquo(arg = dep)
#     dep_pos <- tidyselect::eval_select(dep_enq, data = data, error_call = call)
#     indep_enq <- rlang::enquo(arg = indep)
#     indep_pos <- tidyselect::eval_select(indep_enq, data = data, error_call = call)
#
#     check_category_pairs(data = data, cols_pos = c(dep_pos))
#
#     data_out <-
#       rlang::exec(
#         summarize_data,
#         data = data,
#         dep = names(dep_pos),
#         indep = names(indep_pos),
#         # add_n_to_bygroup = TRUE,
#         !!!dots
#       )
#
#
#     if (length(indep_pos) > 0) {
#       data_out[[names(indep_pos)]] <- forcats::fct_rev(data_out[[names(indep_pos)]])
#     }
#
#     if (dplyr::n_distinct(data_out[[".category"]], na.rm = dots$showNA == "never") == 2 &&
#       !is.null(dots$colour_2nd_binary_cat)) {
#       data_out$.category <- forcats::fct_rev(data_out$.category)
#     }
#
#     chart <-
#       rlang::exec(
#         prep_uni_int_plot,
#         data = data_out,
#         inverse = inverse,
#         colour_palette = colour_palette,
#         !!!dots
#       )
#
#     if(!is.null(dots$label_separator)) {
#       indep_label <- unname(get_raw_labels(data = data, col_pos = indep_pos))
#       attr(chart, "saros_caption") <-
#         get_raw_labels(data = data, col_pos = dep_pos) %>%
#         get_main_question2(label_separator = dots$label_separator) %>%
#         create_caption(
#           data_out = data_out,
#           indep_pos = indep_label,
#           mesos_group = mesos_group,
#           filepath = NULL,
#           translations = dots$translations
#         )
#     }
#
#     chart
#   }
