#' Create Categorical Data Chart from Summarized Data
#'
#' @inheritParams prep_cat_prop_plot_html
#'
#' @return mschart-object. Can be added to an rdocx, rpptx or rxlsx object.
#'
#' @examples
#' saros:::prep_cat_prop_plot_docx(saros:::summarize_data(ex_survey1[stringr::str_c("b_", 1:3)]))
prep_cat_prop_plot_docx <-
  function(data,
           ...,
           call = rlang::caller_env()) {


    # check_summary_data_cols(data, call = call)
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
    hide_legend <- dplyr::n_distinct(data[[".category"]]) == 2 && !rlang::is_null(dots$colour_na)

    fp_text_settings <-
      lapply(colour_palette,
             function(color) {
               officer::fp_text(font.size = dots$label_font_size,
                                color = hex_bw(color, colour_2nd_binary_cat = if(!multi) dots$colour_2nd_binary_cat),
                                font.family = dots$font_family)
             })

    fp_text_settings <- fp_text_settings[seq_len(dplyr::n_distinct(data[[".category"]], na.rm = TRUE))]

    blank_border <- officer::fp_border(style = "none")

    main_text <- officer::fp_text(font.size = dots$main_font_size, font.family = dots$font_family)

    m <- mschart::ms_barchart(data = data,
                              y = ".count", x = ".variable_label",
                              group = ".category", labels = ".data_label")

    if(dots$data_label %in% c("percentage", "percentage_bare")) {
      m <- mschart::as_bar_stack(x = m, percent = TRUE)
    }
    # overlap <- if(!percentage) { # Silly way due to poor programming in mschart
    #
    #   m <- mschart::chart_settings(x = m, dir = if(vertical) "vertical" else "horizontal",
    #                                overlap = overlap)
    # } else {
    #   m <- mschart::chart_settings(x = m, dir = if(vertical) "vertical" else "horizontal")
    # }

    # if(!percentage) { # Silly way due to poor programming in mschart
      overlap <- if(!dots$data_label %in% c("percentage", "percentage_bare")) -40 else 100
      gap_width <- if(!dots$data_label %in% c("percentage", "percentage_bare")) 150 else 50

      m <- mschart::chart_settings(x = m,
                                   dir = if(dots$vertical) "vertical" else "horizontal",
                                   overlap = overlap, gap_width = gap_width)
    # } else {
    #   m <- mschart::chart_settings(x = m,
    #                                dir = if(vertical) "vertical" else "horizontal",
    #                                overlap = 100, gap_width = 50)
    # }
    m <- mschart::chart_data_fill(x = m, values = colour_palette)
    m <- mschart::chart_data_stroke(x = m, values = colour_palette)
    m <- mschart::chart_labels_text(x = m, values = fp_text_settings)
    m <- mschart::chart_labels(x = m, ylab = NULL, xlab = NULL, title = NULL)
    m <- mschart::chart_ax_x(x = m, major_tick_mark = "none")
    if(dots$data_label %in% c("percentage", "percentage_bare")) {
      m <- mschart::chart_ax_y(x = m, num_fmt = "0%")
    }
    m <- mschart::chart_theme(x = m,
                              legend_text = main_text,
                              axis_text_x = main_text,
                              axis_text_y = main_text,
                              grid_major_line_x = blank_border,
                              grid_major_line_y = blank_border,
                              grid_minor_line_x = blank_border,
                              grid_minor_line_y = blank_border,
                              legend_position = if(hide_legend) "n" else "b")
    m
  }




#' Create Word Report with Univariates for Categorical Columns Sharing Same Categories
#'
#' @inheritParams summarize_data
#' @inheritParams prep_cat_prop_plot_docx
#' @inheritParams embed_cat_prop_plot
#' @inheritParams add_caption_attribute
#' @param docx_template  [\code{character(1) || officer::read_docx()}]\cr
#' Either a filepath to a template file, or a rdocx-object from \link[officer]{read_docx}.
#' @param chart_formatting [\code{character(1)}]\cr Template style to be used for formatting chart
#' @param caption_style [\code{character(1)}]\cr Template style to be used for formatting chart. Defaults to "Normal".
#' @param caption_autonum Object obtained from \link[officer]{run_autonum}.
#'
#' @importFrom tidyselect everything eval_select
#' @importFrom officer read_docx docx_dim block_caption body_add_caption
#' @importFrom mschart body_add_chart
#' @importFrom rlang enquo is_bare_character
#' @importFrom cli cli_abort
#' @importFrom stringr str_c str_replace
#' @importFrom stats ave
#' @importFrom purrr map_chr
#' @return rdocx object, which can be saved with print() after loading the officer-package
#' @export
#'
#' @examples
#' library(officer) # To save the rdocx object to disk
#' filepath <-
#'  ex_survey1 |>
#'  embed_cat_prop_plot_docx(cols = a_1:a_9, return_raw = FALSE) |>
#'  print(target = "test_docx_a19.docx")
#' file.remove(filepath)
#'
#'
#'
#'  test_docx_b13 <-
#'    ex_survey1 |>
#'    embed_cat_prop_plot_docx(cols = b_1:b_3,
#'                        plot_height_multiplier = .3,
#'                        plot_height_fixed_constant = 1,
#'                        return_raw = FALSE)
#' \dontrun{
#' print(test_docx_b13, target = "test_docx_b13.docx")
#' file.remove("test_docx_b13.docx")
#' }
embed_cat_prop_plot_docx <-
  function(data,
           ...,
           cols = tidyselect::everything(),
           by = NULL,
           summarized_data = NULL,
           label_separator = NULL,
           translations = .saros.env$defaults$translations) {

    dots <- rlang::list2(...)
    check_multiple_by(data, by = {{by}})

    cols_enq <- rlang::enquo(arg = cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data)
    by_enq <- rlang::enquo(arg = by)
    by_pos <- tidyselect::eval_select(by_enq, data = data)

    check_category_pairs(data = data, cols_pos = c(cols_pos))

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

    chart <-
      rlang::exec(
      prep_cat_prop_plot_docx,
        data = data_out,
        call = rlang::caller_env(),
        !!!dots)

    if(!rlang::is_null(label_separator)) {
      by_label <- unname(get_raw_labels(data = data, cols_pos = by_pos))
      attr(chart, "saros_caption") <-
        get_raw_labels(data = data, cols_pos = cols_pos) %>%
        get_main_question2(label_separator = label_separator) %>%
        add_caption_attribute(data_out = data_out, by_pos = by_label,
                              translations = translations)
    }

    if(FALSE) {
      chart
    } else {


    ## Consider moving all the below into prep_cat_prop_plot_docx
    ## so that embed_chart becomes one function
    docx_file <- use_docx(docx_template = dots$docx_template)

    docx_dims <-
      get_docx_dims(docx_file)
    determine_height <-
      get_docx_height(plot_height_fixed_constant = dots$plot_height_fixed_constant,
                      plot_height_multiplier = dots$plot_height_multiplier,
                      n_col = length(cols_pos),
                      minimum_height = docx_dims[["h"]])
    mschart::body_add_chart(
        x = docx_file,
        chart = chart,
        pos = "after",
        width = docx_dims[["w"]],
        height = determine_height)
    docx_file

      }
  }


