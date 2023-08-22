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
prep_cat_freq_plot_html <-
  function(data,
           ...,
           inverse = FALSE,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    dots <- utils::modifyList(x = formals(draft_report)[!names(formals(draft_report)) %in% c("data", "chapter_overview", "...")],
                              val = dots[!names(dots) %in% c("...")], keep.null = TRUE)


    colour_palette <-
      get_colour_set(
        x = levels(data[[".category"]]),
        user_colour_set = dots$colour_palette,
        colour_na = dots$colour_na,
        colour_2nd_binary_cat = NULL)

    multi <- length(colour_palette) > 2

    indep_vars <- colnames(data)[!colnames(data) %in%
                                .saros.env$summary_data_sort2]

    hide_axis_text <- length(indep_vars) == 0 && dplyr::n_distinct(data[[".variable_label"]]) == 1

    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")

    p <-
      data %>%
      dplyr::mutate(
        Tooltip =     # Tooltip is opposite of the regular display
          if(prop_family) {
            sprintf(fmt = "%s: %.0f", .data[[".category"]], .data[[".count"]])
            } else {
            sprintf(fmt = stringi::stri_c(ignore_null=TRUE, "%s: %.", dots$digits, "f%%"),
                    .data[[".category"]], .data[[".proportion"]]*100)
            },
        .variable_name = paste0('alert(\"variable: ', .data[['.variable_name']], '\")')
      ) %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = .data[[".count"]],
          x = if(length(indep_vars) == 1 && isFALSE(inverse)) .data[[indep_vars]] else .data[[".variable_label"]],
          fill = .data[[".category"]],
          group = .data[[".category"]],
          label = .data[[".data_label"]],
          data_id = .data[[".category"]],
          onclick = .data[[".variable_name"]]
          ),
        cumulative = TRUE) +
      ggiraph::geom_col_interactive(
        mapping = ggplot2::aes(tooltip = .data[["Tooltip"]]), # BUG: Messes up order of categories if enabled.
        position = ggplot2::position_dodge(width = .9),
        na.rm = TRUE
        ) +
      ggiraph::geom_text_interactive(
        mapping = ggplot2::aes(colour =
                                 ggplot2::after_scale(x = hex_bw(.data$fill)),
                               group = .data[[".category"]]),
        position = ggplot2::position_dodge(width = .9), hjust = 2,
        show.legend = FALSE, na.rm = TRUE
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
                     strip.text =  ggiraph::element_text_interactive(data_id = "strip.text", angle=90, hjust = .5, size = dots$main_font_size), #if(length(indep_vars)>0) ggplot2::element_blank() else
                     strip.background = ggiraph::element_rect_interactive(colour = NA)) +
      ggplot2::labs(x=NULL, y=NULL)

      if(length(indep_vars) == 1L) {
        if(!inverse) {

        p <- p +
          ggiraph::facet_grid_interactive(
            rows = ggplot2::vars(.data[[".variable_label"]]),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(tooltip = .data[[".variable_label"]],
                                      label = string_wrap(.data$.label,
                                                          width = dots$x_axis_label_width))),
            interactive_on = "text",
            switch = "y", scales = "free_y", space = "free_y"
          )
        } else {
          p <- p +
            ggiraph::facet_grid_interactive(
              rows = ggplot2::vars(.data[[indep_vars]]),
              labeller = ggiraph::labeller_interactive(
                .mapping = ggplot2::aes(tooltip = .data[[indep_vars]],
                                        label = string_wrap(.data$.label,
                                                            width = dots$x_axis_label_width))),
              interactive_on = "text",
              switch = "y", scales = "free_y", space = "free_y"
            )
        }
      }

    if(!dots$vertical) {
      p + ggplot2::coord_flip()
    } else p
  }



#' Embed Interactive Categorical Plot
#'
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @inheritParams gen_qmd_chapters
#'
#' @param html_interactive Flag, defaults to TRUE
#' @param inverse Flag, defaults to FALSE. If TRUE, swaps x-axis and faceting.
#'
#' @return ggplot
#' @importFrom rlang !!!
#' @export
#'
#' @examples
#' \dontrun{
#' embed_cat_freq_plot(data = ex_survey1, dep = b_1:b_3)
#' }
embed_cat_freq_plot <-
  function(data,
         ...,
         dep = tidyselect::everything(),
         indep = NULL,
         mesos_group = NULL,
         html_interactive = TRUE,
         inverse = FALSE,
         call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "cat_freq_plot")

    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data, error_call = call)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data, error_call = call)


    check_category_pairs(data = data, cols_pos = c(dep_pos))

    dots$data_label <- "count"

    data_out <-
      rlang::exec(
        summarize_data,
        data = data,
        dep = names(dep_pos),
        indep = names(indep_pos),
        # add_n_to_bygroup = TRUE,
        !!!dots)

    if(length(indep_pos)>0) {
      data_out[[names(indep_pos)]] <- forcats::fct_rev(data_out[[names(indep_pos)]])
    }
    if(dplyr::n_distinct(data_out[[".category"]], na.rm = dots$showNA == "never") == 2 &&
       !rlang::is_null(dots$colour_2nd_binary_cat)) {
      data_out$.category <- forcats::fct_rev(data_out$.category)
    }

    chart <-
      rlang::exec(
        if(html_interactive) prep_cat_freq_plot_html else prep_cat_freq_plot_pdf,
        data = data_out,
        inverse = inverse,
        !!!dots
        )

    if(!rlang::is_null(dots$label_separator)) {
      indep_label <- unname(get_raw_labels(data = data, col_pos = indep_pos))
      attr(chart, "saros_caption") <-
        get_raw_labels(data = data, col_pos = dep_pos) %>%
        get_main_question2(label_separator = dots$label_separator) %>%
        create_caption(
          data_out = data_out,
          indep_pos = indep_label,
          mesos_group = mesos_group,
          filepath = NULL,
          translations = dots$translations)
    }


      chart

  }
