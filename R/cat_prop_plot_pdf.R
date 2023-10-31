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
prep_cat_prop_plot_pdf <-
  function(data,
           ...,
           colour_palette = NULL,
           inverse = FALSE,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    dots <- utils::modifyList(x = formals(draft_report)[!names(formals(draft_report)) %in% c("data", "chapter_overview", "...")],
                              val = dots[!names(dots) %in% c("...")], keep.null = TRUE)

    # check_summary_data_cols(data, call = call)


    if(is.null(colour_palette)) {
      n <- length(levels(data[[".category"]]))
      hues <- seq(15, 375, length = n + 1)
      colour_palette <- grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
    }

    multi <- length(colour_palette) > 2

    by_vars <- colnames(data)[!colnames(data) %in%
                                .saros.env$summary_data_sort2]

    hide_axis_text <-
      isTRUE(dots$hide_axis_text_if_single_variable) &&
      length(indep_vars) == 0 &&
      dplyr::n_distinct(data[[".variable_label"]]) == 1

    hide_legend <- dplyr::n_distinct(data[[".category"]]) == 2 && !rlang::is_null(dots$colour_2nd_binary_cat)

    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")

    p <-
      data %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data[[if(prop_family) ".proportion" else stringi::stri_c(ignore_null=TRUE, ".", dots$data_label)]],
          x = if(length(by_vars) == 1 && isFALSE(inverse)) .data[[by_vars]] else .data[[".variable_label"]],
          fill = .data[[".category"]],
          label = .data[[".data_label"]]),
        cumulative = TRUE) +
      ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE),
                        na.rm = TRUE) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(colour =
                                 ggplot2::after_scale(x = hex_bw(.data$fill,
                                                                 colour_2nd_binary_cat = if(!multi) dots$colour_2nd_binary_cat))),
                                     position = ggplot2::position_stack(vjust = .5, reverse = TRUE),
        na.rm = TRUE) +
      ggplot2::scale_y_continuous(limits = c(-.003, if(prop_family) 1.015 else NA),
                                  expand = c(0,0),
                                  labels = if(percentage) function(x) stringi::stri_c(ignore_null=TRUE, x*100, "%") else ggplot2::waiver()) +
      ggplot2::scale_fill_manual(name="", values = colour_palette, drop = FALSE) +
      ggplot2::scale_colour_manual(guide = FALSE, values = c("black", "white")) +
      ggplot2::scale_x_discrete(limits = rev, labels = function(x) string_wrap(x, width = dots$x_axis_label_width)) +
      ggplot2::guides(fill = if(hide_legend) "none" else ggplot2::guide_legend(byrow = TRUE),
                      colour = "none") +
      ggplot2::theme_classic() +
      ggplot2::theme(text = ggplot2::element_text(family = dots$font_family),
                     plot.caption = ggplot2::element_text(size = dots$main_font_size),
                     axis.text.y = if(hide_axis_text) ggplot2::element_blank() else ggplot2::element_text(size = dots$main_font_size),
                     legend.position = "bottom",
                     legend.text = ggplot2::element_text(size = dots$main_font_size),
                     strip.placement = "outside",
                     strip.text = if(length(by_vars)>0) ggplot2::element_blank() else ggiraph::element_text_interactive(angle=90, hjust = .5, size = dots$main_font_size),

                     strip.background = ggplot2::element_rect(colour = NA)) +
      ggplot2::labs(x=NULL, y=NULL)

      if(length(by_vars) == 1L) {
        if(!inverse) {

        p <- p +
          ggplot2::facet_grid(
            rows = ggplot2::vars(.data[[".variable_label"]]),
            labeller = ggplot2::labeller(
              .mapping = ggplot2::aes(label = string_wrap(.data$.label, width = dots$x_axis_label_width))),
            switch = "y", scales = "free_y", space = "free_y"
          )
        } else {
          p <- p +
            ggplot2::facet_grid(
              rows = ggplot2::vars(.data[[by_vars]]),
              labeller = ggplot2::labeller(
                .mapping = ggplot2::aes(label = string_wrap(.data$.label, width = dots$x_axis_label_width))),
              switch = "y", scales = "free_y", space = "free_y"
            )
        }
      }

    if(!dots$vertical) {
      p + ggplot2::coord_flip()
    } else p
  }
