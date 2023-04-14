

#' Create Categorical Data Chart from Summarized Data
#'
#' @inheritParams chart_categorical_plot
#'
#' @importFrom officer fp_text fp_border
#' @importFrom mschart ms_barchart as_bar_stack chart_settings chart_data_fill chart_data_stroke chart_labels_text chart_labels chart_theme chart_ax_x chart_ax_y
#' @importFrom dplyr n_distinct
#'
#' @return mschart-object. Can be added to an rdocx, rpptx or rxlsx object.
#'
#' @examples
#' #chart_categorical_office(summarize_data(ex_survey1[paste0("b_", 1:3)]))
chart_categorical_office <-
  function(data,
           ...,
           label_font_size = 10,
           main_font_size = 8,
           font_family = "Calibri",
           colour_palette = NULL,
           colour_na = "gray90",
           colour_2nd_binary_cat = "#ffffff",
           vertical = FALSE,
           percentage = TRUE,
           digits = 1,
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
    dots <- rlang::list2(...)

    colour_palette <-
      get_colour_set(
        n_colours_needed = length(levels(data[[".category"]])),
        user_colour_set = colour_palette,
        names = levels(data[[".category"]]),
        colour_na = colour_na,
        colour_2nd_binary_cat = colour_2nd_binary_cat,
        seed = seed,
        call = call)

    multi <- length(colour_palette) > 2

    by_vars <- colnames(data)[!colnames(data) %in%
                                .saros.env$summary_data_sort2]

    fp_text_settings <-
      lapply(colour_palette,
             function(color) {
               officer::fp_text(font.size = label_font_size,
                                color = hex_bw(color, colour_2nd_binary_cat = if(!multi) colour_2nd_binary_cat),
                                font.family = font_family)
             })

    fp_text_settings <- fp_text_settings[seq_len(dplyr::n_distinct(data[[".category"]], na.rm = TRUE))]

    blank_border <- officer::fp_border(style = "none")

    main_text <- officer::fp_text(font.size = main_font_size, font.family = font_family)

    m <- mschart::ms_barchart(data = data,
                              y = ".count", x = ".variable_label",
                              group = ".category", labels = ".data_label")

    if(percentage) {
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
      overlap <- if(!percentage) -40 else 100
      gap_width <- if(!percentage) 150 else 50

      m <- mschart::chart_settings(x = m,
                                   dir = if(vertical) "vertical" else "horizontal",
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
    if(percentage) m <- mschart::chart_ax_y(x = m, num_fmt = "0%")
    m <- mschart::chart_theme(x = m,
                              legend_text = main_text,
                              axis_text_x = main_text,
                              axis_text_y = main_text,
                              grid_major_line_x = blank_border,
                              grid_major_line_y = blank_border,
                              grid_minor_line_x = blank_border,
                              grid_minor_line_y = blank_border)
    m
  }




#' Create Word Report with Univariates for Categorical Columns Sharing Same Categories
#'
#' @inheritParams summarize_data
#' @inheritParams chart_categorical_plot
#' @inheritParams embed_chart_categorical_ggplot
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
#'  embed_chart_categorical_office(cols = a_1:a_9) |>
#'  print(target = "test_docx_a19.docx")
#' file.remove(filepath)
#'
#'
#'   docx_template <-
#'       system.file("template","NIFUmal_tom.docx",
#'                    package = "saros", mustWork = TRUE)
#'   colour_palette <-
#'     readxl::read_excel(system.file("template", "NIFUmal_stiler.xlsx",
#'                                    package = "saros", mustWork = TRUE),
#'                        sheet = "NIFUblue") |>
#'     dplyr::pull(hex)
#' chart_format <-
#'  system.file("template", "NIFUmal_stiler.xlsx",
#'              package = "saros", mustWork = TRUE) |>
#'  readxl::read_excel(sheet = 1) |>
#'  dplyr::filter(saros_style == "figure") |>
#'  dplyr::pull(template_style)
#'
#'  test_docx_b13 <-
#'    ex_survey1 |>
#'    embed_chart_categorical_office(cols = b_1:b_3,
#'                        docx_template = docx_template,
#'                        colour_palette = colour_palette,
#'                        chart_formatting = chart_format,
#'                        height_per_col = .3,
#'                        height_fixed = 1)
#' #print(test_docx_b13, target = "test_docx_b13.docx")
#' #file.remove("test_docx_b13.docx")

embed_chart_categorical_office <-
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
           docx_template = NULL,
           chart_formatting = NULL,
           caption_style = NULL,
           caption_autonum = NULL,
           percentage = TRUE,
           digits = 1,
           percent_sign = TRUE,
           sort_by = NULL,
           vertical = FALSE,
           desc = FALSE,
           ignore_if_below = 1,
           label_separator = NULL,
           seed = 1,
           return_raw = FALSE) {

    dots <- rlang::list2(...)
    showNA <- rlang::arg_match(showNA, multiple = FALSE)
    check_data_frame(data)
    check_multiple_by(data, by = {{by}})
    check_string(label_separator, null.ok=TRUE)
    check_bool(return_raw)
    check_double(height_per_col, min = 0)
    check_double(height_fixed, min = 0)
    check_autonum(caption_autonum)

    data <- dplyr::select(data, {{cols}}, {{by}})


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
        percentage = percentage,
        showNA = showNA,
        digits = digits,
        percent_sign = percent_sign,
        sort_by = sort_by,
        desc = desc,
        ignore_if_below = ignore_if_below,
        label_separator = label_separator,
        call = call,
        !!!dots)



    chart <-
      rlang::exec(
      chart_categorical_office,
        data = data_out,
        label_font_size = label_font_size,
        main_font_size = main_font_size,
        font_family = font_family,
        colour_palette = colour_palette,
        colour_na = colour_na,
        colour_2nd_binary_cat = colour_2nd_binary_cat,
        vertical = vertical,
        percentage = percentage,
        digits = digits,
        seed = seed,
        call = rlang::caller_env(),
        !!!dots)


    if(return_raw) {
      chart
    } else {


    ## Consider moving all the below into chart_categorical_office
    ## so that embed_chart becomes one function
    docx_file <- use_docx(docx_template = docx_template)


    if(!is.null(label_separator)) {
      docx_file <-
        get_block_caption(
          data = data,
          cols_pos = cols_pos,
          docx_file = docx_file,
          label_separator = label_separator,
          caption_style = caption_style,
          caption_autonum = caption_autonum)
    }


    docx_dims <-
      get_docx_dims(docx_file)
    determine_height <-
      get_docx_height(height_fixed = height_fixed,
                      height_per_col = height_per_col,
                      n_col = length(cols_pos),
                      minimum_height = docx_dims[["h"]])
    mschart::body_add_chart(
        x = docx_file,
        chart = chart,
        style = chart_formatting,
        pos = "after",
        width = docx_dims[["w"]],
        height = determine_height)
      }
  }


