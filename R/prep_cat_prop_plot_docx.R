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
    dots <- utils::modifyList(x = formals(draft_report)[!names(formals(draft_report)) %in% .saros.env$ignore_args],
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
                                color = hex_bw(color),
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



