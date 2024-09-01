#' @export
# make_content.uni_int_plot <-
#   function(...) {
#
#
#     dots <- rlang::list2(...)
#
#
#
#
#     indep_vars <- colnames(data)[!colnames(data) %in%
#       .saros.env$summary_data_sort2]
#
#     hide_axis_text <-
#       isTRUE(hide_axis_text_if_single_variable) &&
#       length(indep_vars) == 0 &&
#       dplyr::n_distinct(data[[".variable_label"]]) == 1
#
#     percentage <- data_label %in% c("percentage", "percentage_bare")
#     prop_family <- data_label %in% c("percentage", "percentage_bare", "proportion")
#
#     p <-
#       data |>
#       dplyr::mutate(.id = seq_len(nrow(.)),
#         .tooltip = # Tooltip is opposite of the regular display
#           if (prop_family) {
#             sprintf(fmt = "%s\nN = %.0f\n%s", .data[[".category"]], .data[[".count"]], .data[[".variable_label"]])
#           } else {
#             sprintf(
#               fmt = stringi::stri_c("%s\nP = %.", digits, "f%%\n%s", ignore_null=TRUE),
#               .data[[".category"]], .data[[".proportion"]] * 100, .data[[".variable_label"]]
#             )
#           },
#         .onclick = paste0('alert(\"variable: ', .data[['.variable_name']], '\")')
#       ) |>
#       ggplot2::ggplot(
#         mapping = ggplot2::aes(
#           y = .data[[if (prop_family) ".proportion" else stringi::stri_c(ignore_null=TRUE, ".", data_label)]],
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
#       ggplot2::scale_x_discrete(limits = rev, labels = function(x) string_wrap(x, width = x_axis_label_width)) +
#       ggplot2::guides(
#         fill = if (hide_legend) "none" else ggiraph::guide_legend_interactive(data_id = "fill.guide", byrow = TRUE),
#         colour = "none"
#       ) +
#       ggplot2::theme_classic() +
#       ggplot2::theme(
#         text = ggplot2::element_text(family = font_family),
#         axis.text.y = if (hide_axis_text) ggplot2::element_blank() else ggiraph::element_text_interactive(data_id = "axis.text.y"),
#         plot.caption = ggiraph::element_text_interactive(data_id = "plot.caption", size = main_font_size),
#         legend.position = "bottom",
#         legend.text = ggiraph::element_text_interactive(data_id = "legend.text", size = main_font_size),
#         strip.placement = "outside",
#         strip.text.x = ggplot2::element_text(margin = ggplot2::margin(l = 0, t = 0, r = 0, b = 2, "cm")),
#         strip.text = ggiraph::element_text_interactive(data_id = "strip.text", angle = strip_angle, hjust = .5, size = main_font_size), # if(length(indep_vars)>0) ggplot2::element_blank() else
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
#                                   width = x_axis_label_width
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
#                                     width = x_axis_label_width
#                 )
#               )
#             ),
#             interactive_on = "text",
#             switch = "y", scales = "free_y", space = "free_y"
#           )
#       }
#     }
#
#     if (!vertical) {
#       p + ggplot2::coord_flip()
#     } else {
#       p
#     }
#   }

