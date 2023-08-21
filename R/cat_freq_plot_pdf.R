#'
#' Create Single Interactive Categorical Plot with Univariates for Categorical Columns Sharing Same Categories
#'
#' @inheritParams summarize_data
#' @inheritParams prep_cat_prop_plot_pdf
#'
#' @importFrom rlang !!!
#'
#' @return ggiraph object, plottable with plot()
prep_cat_freq_plot_pdf <-
  function(data,
           ...,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)

    colour_palette <-
      get_colour_set(
        x = levels(data[[".category"]]),
        user_colour_set = dots$colour_palette,
        colour_na = dots$colour_na,
        colour_2nd_binary_cat = dots$colour_2nd_binary_cat,
        call = call)

    multi <- length(colour_palette) > 2

    by_vars <- colnames(data)[!colnames(data) %in%
                                .saros.env$summary_data_sort2]

    hide_axis_text <- length(by_vars) == 0 && dplyr::n_distinct(data[[".variable_label"]]) == 1

    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")

    p <-
      data %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data[[".count"]],
          x = if(length(by_vars) == 1) .data[[by_vars]] else .data[[".variable_label"]],
          fill = .data[[".category"]],
          label = .data[[".data_label"]]),
        cumulative = TRUE) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(group = .data[[".category"]],
                               colour =
                                 ggplot2::after_scale(x = hex_bw(.data$fill))),
        position = ggplot2::position_dodge(width = 0.9), hjust = 2) +
      ggplot2::scale_y_continuous(limits = c(-.003, NA),
                                  expand = c(0,0)) +
      ggplot2::scale_fill_manual(name="", values = colour_palette, drop = FALSE) +
      ggplot2::scale_colour_manual(guide = FALSE, values = c("black", "white")) +
      ggplot2::scale_x_discrete(limits = rev, labels = function(x) string_wrap(x, width = dots$x_axis_label_width)) +
      ggplot2::guides(fill = ggplot2::guide_legend(byrow = TRUE),
                      colour = "none") +
      ggplot2::theme_classic() +
      ggplot2::theme(text = ggplot2::element_text(family = dots$font_family),
                     plot.caption = ggplot2::element_text(size = dots$main_font_size),
                     axis.text.y = if(hide_axis_text) ggplot2::element_blank() else ggplot2::element_text(size = dots$main_font_size),
                     legend.position = "bottom",
                     legend.text = ggplot2::element_text(size = dots$main_font_size),
                     strip.placement = "outside",
                     strip.text = ggiraph::element_text_interactive(angle=90, hjust = .5, size = dots$main_font_size), #if(length(by_vars)>0) ggplot2::element_blank() else

                     strip.background = ggplot2::element_rect(colour = NA)) +
      ggplot2::labs(x=NULL, y=NULL)

      if(length(by_vars) == 1L) {
        p <- p +
          ggplot2::facet_grid(
            rows = ggplot2::vars(.data[[".variable_label"]]),
            labeller = ggplot2::labeller(
              .mapping = ggplot2::aes(label = string_wrap(.data$.label, width = dots$x_axis_label_width))),
            switch = "y", scales = "free_y", space = "free_y"
          )
      }

    if(!dots$vertical) {
      p + ggplot2::coord_flip()
    } else p
  }


