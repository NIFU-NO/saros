#'
#' Create Single Interactive Categorical Plot with Univariates for Categorical Columns Sharing Same Categories
#'
#' @inheritParams summarize_data
#' @inheritParams prep_cat_prop_plot_html
#'
#' @importFrom rlang !!!
#'
#' @return ggiraph object, plottable with plot()
prep_cat_freq_plot_html <-
  function(data,
           ...,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)

    colour_palette <-
      get_colour_set(
        x = levels(data[[".category"]]),
        user_colour_set = dots$colour_palette,
        colour_na = dots$colour_na,
        colour_2nd_binary_cat = NULL,
        call = call)

    multi <- length(colour_palette) > 2

    by_vars <- colnames(data)[!colnames(data) %in%
                                .saros.env$summary_data_sort2]

    hide_axis_text <- length(by_vars) == 0 && dplyr::n_distinct(data[[".variable_label"]]) == 1

    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")

    p <-
      data %>%
      dplyr::mutate(
        Tooltip =     # Tooltip is opposite of the regular display
          if(prop_family) {
            sprintf(fmt = "%s: %.0f", .data[[".category"]], .data[[".count"]])
            } else {
            sprintf(fmt = stringr::str_c("%s: %.", dots$digits, "f%%"),
                    .data[[".category"]], .data[[".proportion"]]*100)
            }) %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data[[".count"]],
          x = if(length(by_vars) == 1) .data[[by_vars]] else .data[[".variable_label"]],
          fill = .data[[".category"]],
          label = .data[[".data_label"]]),
        cumulative = TRUE) +
      ggiraph::geom_col_interactive(
        # mapping = ggplot2::aes(tooltip = .data[["Tooltip"]]), # BUG: Messes up order of categories if enabled.
        position = ggplot2::position_dodge(width = .9)
        ) +
      ggiraph::geom_text_interactive(
        mapping = ggplot2::aes(colour =
                                 ggplot2::after_scale(x = hex_bw(.data$fill)),
                               group = .data[[".category"]]),
        position = ggplot2::position_dodge(width = .9), hjust = 2
        ) +
      ggplot2::scale_y_continuous(limits = c(-.003, NA),
                                  expand = c(0,0)) +
      ggiraph::scale_fill_manual_interactive(name="",
                                             values = colour_palette,
                                             data_id = function(x) x,
                                             tooltip = function(x) x, drop = FALSE) +
      ggiraph::scale_colour_manual_interactive(guide = FALSE, values = c("black", "white")) +
      ggplot2::scale_x_discrete(limits = rev, labels = function(x) string_wrap(x, width = dots$x_axis_label_width)) +
      ggplot2::guides(fill = ggiraph::guide_legend_interactive(data_id="fill.guide", byrow = TRUE),
                      colour = "none") +
      ggplot2::theme_classic() +
      ggplot2::theme(text = ggiraph::element_text_interactive(family = dots$font_family),
                     axis.text.y = if(hide_axis_text) ggplot2::element_blank() else ggiraph::element_text_interactive(data_id = "axis.text.y"),
                     plot.caption = ggiraph::element_text_interactive(data_id = "plot.caption", size = dots$main_font_size),
                     legend.position = "bottom",
                     legend.text = ggiraph::element_text_interactive(data_id = "legend.text", size = dots$main_font_size),
                     strip.placement = "outside",
                     strip.text = if(length(by_vars)>0) ggplot2::element_blank() else ggiraph::element_text_interactive(angle=90, hjust = .5, size = dots$main_font_size),

                     strip.background = ggiraph::element_rect_interactive(colour = NA)) +
      ggplot2::labs(x=NULL, y=NULL)

      if(length(by_vars) == 1L) {
        p <- p +
          ggiraph::facet_grid_interactive(
            rows = ggplot2::vars(.data[[".variable_label"]]),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(tooltip = "Tooltip",
                                      label = string_wrap(.data$.label,
                                                          width = dots$x_axis_label_width))),
            interactive_on = "text",
            switch = "y", scales = "free_y", space = "free_y"
          )
      #} else {
        # p <- p +
        #   ggiraph::facet_grid_interactive(
        #     rows = ggplot2::vars(.data$.by_group),
        #     labeller = ggiraph::labeller_interactive(
        #       .mapping = ggplot2::aes(tooltip = "Tooltip")),
        #     interactive_on = "text",
        #     switch = "y", scales = "free_y", space = "free_y")
      }

    if(!dots$vertical) {
      p + ggplot2::coord_flip()
    } else p
  }



#' Embed Interactive Categorical Plot
#'
#'
#' @inheritParams summarize_data
#' @inheritParams prep_cat_freq_plot_html
#' @inheritParams add_caption_attribute
#' @param plot_height_multiplier [\code{numeric(1)>0}]\cr Height in cm per chart entry.
#' @param plot_height_fixed_constant [\code{numeric(1)>0}]\cr Fixed height in cm.
#' @param return_raw [\code{logical(1)}] Whether to return the raw static chart. Defaults to FALSE.
#' @param ... Optional parameters forwarded from above.
#' @return ggplot
#' @importFrom rlang !!!
#' @export
#'
#' @examples
#' \dontrun{
#' embed_cat_freq_plot(data = ex_survey1, cols = b_1:b_3)
#' }
embed_cat_freq_plot <-
  function(data,
         ...,
         cols = tidyselect::everything(),
         by = NULL,
         summarized_data = NULL,
         label_separator = NULL,
         translations = getOption("saros")$translations,
         html_interactive = TRUE,
         call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    cols_enq <- rlang::enquo(arg = cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data, error_call = call)
    by_enq <- rlang::enquo(arg = by)
    by_pos <- tidyselect::eval_select(by_enq, data = data, error_call = call)


    check_category_pairs(data = data, cols_pos = c(cols_pos))

    dots$data_label <- "count"

    data_out <-
      rlang::exec(
        summarize_data,
        data = data,
        cols = cols_pos,
        by = by_pos,
        label_separator = label_separator,
        add_n_to_bygroup = TRUE,
        call = call,
        !!!dots)

    if(length(by_pos)>0) {
      data_out[[names(by_pos)]] <- forcats::fct_rev(data_out[[names(by_pos)]])
    }
    if(dplyr::n_distinct(data_out[[".category"]], na.rm = dots$showNA == "never") == 2 &&
       !rlang::is_null(dots$colour_2nd_binary_cat)) {
      data_out$.category <- forcats::fct_rev(data_out$.category)
    }

    chart <-
      rlang::exec(
        if(html_interactive) prep_cat_freq_plot_html else prep_cat_freq_plot_pdf,
        data = data_out,
        call = call,
        !!!dots)

    if(!rlang::is_null(label_separator)) {
      by_label <- unname(get_raw_labels(data = data, cols_pos = by_pos))
      attr(chart, "saros_caption") <-
        get_raw_labels(data = data, cols_pos = cols_pos) %>%
        get_main_question2(label_separator = label_separator) %>%
        add_caption_attribute(data_out = data_out,
                              by_pos = by_label,
                              translations = translations)
    }


      chart

  }


