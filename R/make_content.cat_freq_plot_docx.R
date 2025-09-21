#' @export
make_content.cat_freq_plot_docx <-
  function(...) {
    dots <- rlang::list2(...)

    data <- dots$data_summary

    if (is.null(dots$colour_palette)) {
      n <- length(levels(data[[".category"]]))
      hues <- seq(15, 375, length = n + 1)
      dots$colour_palette <- grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
    }

    indep_vars <- colnames(data)[
      !colnames(data) %in%
        .saros.env$summary_data_sort2
    ]

    # hide_axis_text <-
    #  isTRUE(dots$hide_axis_text_if_single_variable) &&
    #  length(indep_vars) == 0 &&
    #  dplyr::n_distinct(data[[".variable_label"]]) == 1

    # hide_legend <-
    #  dplyr::n_distinct(data$.category, na.rm = TRUE) == 2 &&
    #  !is.null(dots$colour_na)

    # percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    # prop_family <- dots$data_label %in% c("percentage", "percentage_bare", "proportion")

    fp_text_settings <-
      lapply(
        dots$colour_palette,
        function(color) {
          officer::fp_text(
            font.size = dots$label_font_size,
            color = hex_bw(color),
            font.family = dots$font_family
          )
        }
      )

    fp_text_settings <- fp_text_settings[seq_len(dplyr::n_distinct(
      data[[".category"]],
      na.rm = TRUE
    ))]

    blank_border <- officer::fp_border(style = "none")

    main_text <- officer::fp_text(
      font.size = dots$main_font_size,
      font.family = dots$font_family
    )
    x <- if (length(indep_vars) == 1 && isFALSE(dots$inverse)) {
      indep_vars
    } else {
      ".variable_label"
    }

    m <- mschart::ms_barchart(
      data = data,
      y = ".count",
      x = x,
      group = ".category",
      labels = ".data_label"
    )

    if (dots$data_label %in% c("percentage", "percentage_bare")) {
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
    # overlap <- if(!dots$data_label %in% c("percentage", "percentage_bare")) -40 else 100
    # gap_width <- if(!dots$data_label %in% c("percentage", "percentage_bare")) 150 else 50

    m <- mschart::chart_settings(
      x = m,
      dir = if (dots$vertical) "vertical" else "horizontal"
    )
    # } else {
    #   m <- mschart::chart_settings(x = m,
    #                                dir = if(vertical) "vertical" else "horizontal",
    #                                overlap = 100, gap_width = 50)
    # }
    m <- mschart::chart_data_fill(x = m, values = dots$colour_palette)
    m <- mschart::chart_data_stroke(x = m, values = dots$colour_palette)
    if (length(fp_text_settings) > 0) {
      m <- mschart::chart_labels_text(x = m, values = fp_text_settings)
    }
    m <- mschart::chart_labels(x = m, ylab = NULL, xlab = NULL, title = NULL)
    m <- mschart::chart_ax_x(x = m, major_tick_mark = "none")
    if (dots$data_label %in% c("percentage", "percentage_bare")) {
      m <- mschart::chart_ax_y(x = m, num_fmt = "0%")
    }
    m <- mschart::chart_theme(
      x = m,
      legend_text = main_text,
      axis_text_x = main_text,
      axis_text_y = main_text,
      grid_major_line_x = blank_border,
      grid_major_line_y = blank_border,
      grid_minor_line_x = blank_border,
      grid_minor_line_y = blank_border
    )

    docx_file <- use_docx(docx_template = dots$docx_template)

    docx_dims <-
      get_docx_dims(docx_file)

    docx_file <-
      mschart::body_add_chart(
        x = docx_file,
        chart = m,
        pos = "after",
        width = docx_dims[["w"]],
        height = dots$plot_height
      )
    if (!is.null(dots$path)) print(docx_file, target = dots$path) else docx_file
  }
