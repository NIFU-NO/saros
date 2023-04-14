#'
#' Create Single Interactive Categorical Plot with Univariates for Categorical Columns Sharing Same Categories
#'
#' @param data Data frame or tibble.
#' @param label_font_size [\code{integer(1)}]\cr Font size for data labels
#' @param main_font_size [\code{integer(1)}]\cr Font size for all other text
#' Must contain at least the number of unique values (excl. missing) in the data set.
#' @param colour_palette [\code{character()}]\cr
#' Must contain at least the number of unique values (incl. missing) in the data set.
#' @param colour_na [\code{character(1)}]\cr Colour as a single string.
#' @param colour_2nd_binary_cat [\code{character(1)}]\cr Colour for second category in binary variables. Often useful to hide this.
#' @param font_family Word font family. See officer::fp_text
#' @param vertical [\code{logical(1)}] Logical. If FALSE (default), then horizontal.
#' @param percentage [\code{logical(1)}] If TRUE, will compute percentage and add percentage sign to data label.
#' @param digits [\code{integer(1)}]\cr Number of digits in percentages.
#' @param x_axis_label_width [\code{integer(1)}] Width of the labels used for the categorical column names.
#' @param seed [\code{integer(1)}] Optional random seed for selection of colours in blender.
#' @param call For internal use
#'
#' @importFrom ggplot2 ggplot aes position_fill coord_flip theme_classic guides theme labs scale_y_continuous
#' @importFrom scales percent_format
#' @importFrom ggiraph girafe geom_col_interactive scale_fill_manual_interactive guide_legend_interactive element_text_interactive element_rect_interactive geom_text_interactive
#' @importFrom cli cli_warn
#' @importFrom rlang caller_env is_bool is_integer is_string
#'
#' @return ggiraph object, plottable with plot()
chart_categorical_plot <-
  function(data,
           label_font_size = 10,
           main_font_size = 8,
           font_family = "Calibri",
           colour_palette = NULL,
           colour_na = "gray90",
           colour_2nd_binary_cat = "#ffffff",
           vertical = FALSE,
           percentage = TRUE,
           digits = 1,
           x_axis_label_width = 20,
           seed = 1,
           call = rlang::caller_env()) {

    check_data_frame(data, call = call)
    check_summary_data_cols(data, call = call)
    check_bool(vertical, call = call)
    check_bool(percentage, call = call)
    check_integerish(label_font_size, min=0, max=72, call = call)
    check_integerish(main_font_size, min=0, max=72, call = call)
    check_integerish(digits, min=0, call = call)
    check_integerish(seed, min=0, call = call)
    check_string(font_family, call = call)
    check_colour(colour_2nd_binary_cat, call = call)
    check_colour(colour_na, call = call)
    check_colours(colour_palette, call = call)


    colour_palette <-
      get_colour_set(n_colours_needed = length(levels(data[[".category"]])),
                     user_colour_set = colour_palette,
                     names = levels(data[[".category"]]),
                     colour_na = colour_na,
                     colour_2nd_binary_cat = colour_2nd_binary_cat,
                     seed = seed,
                     call = call)

    multi <- length(colour_palette) > 2

    by_vars <- colnames(data)[!colnames(data) %in%
                                .saros.env$summary_data_sort2]

    p <-
      data %>%
      dplyr::mutate(Tooltip =     # Tooltip is opposite of the regular display
                      if(percentage) sprintf(fmt = "%s: %.0f", .data[[".category"]], .data[[".count"]]),
                    Tooltip =
                      if(!percentage) sprintf(fmt = paste0("%s: %.", digits, "f%%"), .data[[".category"]], .data[[".proportion"]]*100)) %>%
      ggplot2::ggplot(
                    mapping = ggplot2::aes(
                      y = .data[[if(percentage) ".proportion" else ".count"]],
                      x = if(length(by_vars) == 1) .data[[by_vars]] else .data[[".variable_label"]],
                      fill = .data[[".category"]],
                      label = .data[[".data_label"]]),
                    cumulative = TRUE) +
      ggiraph::geom_col_interactive(
        # mapping = ggplot2::aes(tooltip = .data[["Tooltip"]]), # Currently does not work
        position = ggplot2::position_stack(reverse = TRUE)) +
      ggiraph::geom_text_interactive(mapping =
                                       ggplot2::aes(colour =
                                                      ggplot2::after_scale(x =
                                                                              hex_bw(.data$fill,
                                                                                     colour_2nd_binary_cat = if(!multi) colour_2nd_binary_cat))),
                                     position = ggplot2::position_stack(vjust = .5, reverse = TRUE)) +

      ggplot2::scale_y_continuous(limits = c(-.003, if(percentage) 1.015 else NA),
                                  expand = c(0,0),
                                  labels = if(percentage) function(x) paste0(x*100, "%")) +
      ggiraph::scale_fill_manual_interactive(name="",
                                             values = colour_palette,
                                             data_id = function(x) x,
                                             tooltip = function(x) x) +
      ggiraph::scale_colour_manual_interactive(guide = FALSE, values = c("black", "white")) +
      ggplot2::scale_x_discrete(limits = rev, labels = function(x) stringr::str_wrap(x, width = x_axis_label_width)) +
      ggplot2::guides(fill = ggiraph::guide_legend_interactive(data_id="fill.guide", nrow = 1),
                      colour = "none") +
      ggplot2::theme_classic() +
      ggplot2::theme(text = ggiraph::element_text_interactive(family = font_family),
                     axis.text.y = ggiraph::element_text_interactive(data_id = "axis.text.y"),
                     plot.caption = ggiraph::element_text_interactive(data_id = "plot.caption", size = main_font_size),
                     legend.position = "bottom",
                     legend.text = ggiraph::element_text_interactive(data_id = "legend.text", size = main_font_size),
                     strip.placement = "outside",
                     strip.text.x = element_text_interactive(),
                     strip.text.y = ggiraph::element_text_interactive(angle=180, hjust = .5, size = main_font_size),
                     strip.background = ggiraph::element_rect_interactive(colour = NA)) +
      ggplot2::labs(x=NULL, y=NULL)

      if(length(by_vars) == 1L) {
        p <- p +
          ggiraph::facet_grid_interactive(
            rows = ggplot2::vars(.data$.variable_label),
            labeller = ggiraph::labeller_interactive(
              .mapping = ggplot2::aes(tooltip = "Tooltip",
                                      label = stringr::str_wrap(.data$.label, width = x_axis_label_width, indent = 0, exdent = 0))),
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

    if(!vertical) {
      p + ggplot2::coord_flip()
    } else p
  }



#' Embed Interactive Categorical Plot
#'
#'
#' @inheritParams summarize_data
#' @inheritParams chart_categorical_plot
#' @param height_per_col [\code{numeric(1)>0}]\cr Height in cm per chart entry.
#' @param height_fixed [\code{numeric(1)>0}]\cr Fixed height in cm.
#' @param return_raw [\code{logical(1)}] Whether to return the raw static chart. Defaults to FALSE.
#' @param ... Optional parameters forwarded from above.
#' @return ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' embed_chart_categorical_ggplot(data = ex_survey1, cols = tidyselect::matches("b_"))
#' }
embed_chart_categorical_ggplot <-
  function(data,
         cols = tidyselect::everything(),
         by = NULL,
         ...,
         showNA = c("ifany", "always", "no"),
         label_font_size = 8,
         main_font_size = 9,
         font_family = "Calibri",
         colour_palette = NULL,
         colour_na = "gray90",
         colour_2nd_binary_cat = "#ffffff",
         height_per_col = .3,
         height_fixed = 1,
         percentage = TRUE,
         digits = 1,
         percent_sign = TRUE,
         sort_by = NULL,
         vertical = FALSE,
         desc = FALSE,
         ignore_if_below = 1,
         label_separator = NULL,
         x_axis_label_width = 20,
         seed = 1,
         return_raw = FALSE) {

    showNA <- rlang::arg_match(showNA, multiple = FALSE)
    check_data_frame(data)
    check_multiple_by(data, by = {{by}})
    check_string(label_separator, null.ok=TRUE)
    check_bool(return_raw)
    check_double(height_per_col, min = 0)
    check_double(height_fixed, min = 0)
    rlang::check_dots_used()


  data <- dplyr::select(data, {{cols}}, {{by}})


  cols_enq <- rlang::enquo(arg = cols)
  cols_pos <- tidyselect::eval_select(cols_enq, data = data)
  by_enq <- rlang::enquo(arg = by)
  by_pos <- tidyselect::eval_select(by_enq, data = data)

  check_category_pairs(data = data, cols_pos = c(cols_pos))


  data_out <-
    summarize_data(data = data,
                   cols = {{cols}},
                   by = {{by}},
                   percentage = percentage,
                   showNA = showNA,
                   digits = digits,
                   percent_sign = percent_sign,
                   sort_by = sort_by,
                   desc = desc,
                   ignore_if_below = ignore_if_below,
                   label_separator = label_separator,
                   call = rlang::caller_env())


  chart <-
    chart_categorical_plot(data = data_out,
                           label_font_size = label_font_size,
                           main_font_size = main_font_size,
                           font_family = font_family,
                           colour_palette = colour_palette,
                           colour_na = colour_na,
                           colour_2nd_binary_cat = colour_2nd_binary_cat,
                           vertical = vertical,
                           percentage = percentage,
                           digits = digits,
                           x_axis_label_width = x_axis_label_width,
                           seed = seed,
                           call = rlang::caller_env())


  if(!is.null(label_separator)) {


    main_question <-
      get_raw_labels(data = data, cols_pos = cols_pos) %>%
      get_main_question2(label_separator = label_separator)
  }

  if(return_raw) {
    chart
  } else {
    ggiraph::girafe(ggobj = chart,
                    options = ggiraph::opts_toolbar(
                      position = "topright",
                      saveaspng = TRUE,
                      pngname = "figure_.png"
                    ))
  }

  }


