#' Create Categorical Data Chart from Summarized Data
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @inheritParams gen_qmd_chapters
#' @param inverse Flag, defaults to FALSE. If TRUE, swaps x-axis and faceting.
#'
#' @return mschart-object. Can be added to an rdocx, rpptx or rxlsx object.
#' @keywords internal
prep_cat_prop_plot_docx <-
  function(data,
           ...,
           colour_palette = NULL,
           inverse = FALSE,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    dots <- utils::modifyList(x = formals(draft_report)[!names(formals(draft_report)) %in% c("data", "chapter_overview", "...")],
                              val = dots[!names(dots) %in% c("...")], keep.null = TRUE)


    if(is.null(colour_palette)) {
      n <- length(levels(data[[".category"]]))
      hues <- seq(15, 375, length = n + 1)
      colour_palette <- grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
    }

    multi <- length(colour_palette) > 2

    indep_vars <- colnames(data)[!colnames(data) %in%
                                .saros.env$summary_data_sort2]

    hide_axis_text <-
      isTRUE(dots$hide_axis_text_if_single_variable) &&
      length(indep_vars) == 0 &&
      dplyr::n_distinct(data[[".variable_label"]]) == 1

    hide_legend <-
      dplyr::n_distinct(data[[".category"]], na.rm = TRUE) == 2 &&
      !rlang::is_null(dots$colour_na)

    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")

    fp_text_settings <-
      lapply(colour_palette,
             function(color) {
               officer::fp_text(font.size = dots$label_font_size,
                                color = hex_bw(color, colour_2nd_binary_cat = if(!multi) dots$colour_2nd_binary_cat),
                                font.family = dots$font_family)
             })

    fp_text_settings <- fp_text_settings[seq_len(dplyr::n_distinct(data[[".category"]], na.rm = TRUE))]


    blank_border <- officer::fp_border(style = "none")

    main_text <- officer::fp_text(font.size = dots$main_font_size,
                                  font.family = dots$font_family)

    m <- mschart::ms_barchart(data = data,
                              y = ".count",
                              x = if(length(indep_vars) == 1) indep_vars else ".variable_label",
                              group = ".category",
                              labels = ".data_label")

    if(percentage) {
      m <- mschart::as_bar_stack(x = m, percent = TRUE)
    }
    overlap <- if(!percentage) -40 else 100
    gap_width <- if(!percentage) 150 else 50

    m <- mschart::chart_settings(x = m,
                                 dir = if(dots$vertical) "vertical" else "horizontal",
                                 overlap = overlap, gap_width = gap_width)
    m <- mschart::chart_data_fill(x = m, values = colour_palette)
    m <- mschart::chart_data_stroke(x = m, values = colour_palette)
    if(length(fp_text_settings)>0) m <- mschart::chart_labels_text(x = m, values = fp_text_settings)
    m <- mschart::chart_labels(x = m, ylab = NULL, xlab = NULL, title = NULL)
    m <- mschart::chart_ax_x(x = m, major_tick_mark = "none")
    if(percentage) {
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
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @inheritParams gen_qmd_chapters
#' @inheritParams embed_cat_prop_plot
#'
#' @importFrom tidyselect everything eval_select
#' @importFrom officer read_docx docx_dim block_caption body_add_caption
#' @importFrom mschart body_add_chart
#' @importFrom rlang enquo is_bare_character
#' @importFrom cli cli_abort
#' @importFrom stats ave
#' @return rdocx object, which can be saved with print() after loading the officer-package
#' @export
#'
#' @examples
#' library(officer) # To save the rdocx object to disk
#'
#'  test_docx_b13 <-
#'    ex_survey1 |>
#'    embed_cat_prop_plot_docx(dep = b_1:b_3,
#'               showNA = "never",
#'               descend = TRUE,
#'               return_raw = FALSE,
#'               hide_label_if_prop_below=0,
#'               data_label = "percentage_bare",
#'               data_label_decimal_symbol = ",",
#'               digits = 1,
#'               label_font_size = 12,
#'               main_font_size = 12,
#'              plot_height_multiplier = .3,
#'              plot_height_fixed_constant = 1,
#'               vertical = FALSE,
#'               font_family = "sans")
#' \dontrun{
#' print(test_docx_b13, target = "test_docx_b13.docx")
#' file.remove("test_docx_b13.docx")
#' }
embed_cat_prop_plot_docx <-
  function(data,
           ...,
           dep = tidyselect::everything(),
           indep = NULL,
           colour_palette = NULL,
           mesos_group = NULL,
           inverse = FALSE) {

    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "cat_prop_plot")

    check_multiple_indep(data, indep = {{indep}})

    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data)

    check_category_pairs(data = data, cols_pos = c(dep_pos))

    data_out <-
      rlang::exec(
        summarize_data,
        data = data,
        dep = names(dep_pos),
        indep = names(indep_pos),
        # add_n_to_bygroup = TRUE,
        !!!dots)

    # if(length(indep_pos)>0) {
    #   data_out[[names(indep_pos)]] <- forcats::fct_rev(data_out[[names(indep_pos)]])
    # }
    if(length(indep_pos)==0) {
      data_out[[".variable_label"]] <- forcats::fct_rev(data_out[[".variable_label"]])
    }

    if(dplyr::n_distinct(data_out[[".category"]], na.rm = dots$showNA == "never") == 2 &&
       !rlang::is_null(dots$colour_2nd_binary_cat)) {
      data_out$.category <- forcats::fct_rev(data_out$.category)
    }

    chart <-
      rlang::exec(
        prep_cat_prop_plot_docx,
        data = data_out,
        inverse = inverse,
        colour_palette = colour_palette,
        call = rlang::caller_env(),
        !!!dots)

    if(!rlang::is_null(dots$label_separator)) {
      indep_label <- unname(get_raw_labels(data = data, col_pos = indep_pos))
      attr(chart, "saros_caption") <-
        get_raw_labels(data = data, col_pos = dep_pos) %>%
        get_main_question2(label_separator = dots$label_separator) %>%
        create_caption(data_out = data_out,
                              indep_pos = indep_label,
                              mesos_group = mesos_group,
                       filepath = NULL,
                              translations = dots$translations)
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
                      n_col = length(dep_pos),
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


