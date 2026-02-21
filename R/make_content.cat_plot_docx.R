#' @export
make_content.cat_plot_docx <-
  function(...) {
    dots <- rlang::list2(...)

    data <- dots$data_summary

    # Pull girafe global settings for color consistency
    girafe_settings <- global_settings_get(fn_name = "girafe")

    # Get category levels
    cat_levels <- levels(data[[".category"]])

    # Resolve colors using shared logic from girafe-utils.R
    color_info <- resolve_category_colors(cat_levels, girafe_settings)

    # Extract resolved values
    checkbox <- color_info$checkbox
    cat_levels <- color_info$cat_levels
    colour_palette <- color_info$colour_palette

    # Apply checkbox transformations to data
    if (checkbox) {
      data <- data |>
        dplyr::mutate(
          .category = forcats::fct_relevel(
            .data$.category,
            cat_levels[1],
            cat_levels[2]
          ),
          .data_label = ifelse(
            .data$.category == girafe_settings$not_checked,
            "",
            .data$.data_label
          )
        )
    }

    indep_vars <- colnames(data)[
      !colnames(data) %in%
        .saros.env$summary_data_sort2
    ]

    hide_axis_text <-
      isTRUE(dots$hide_axis_text_if_single_variable) &&
      length(indep_vars) == 0 &&
      dplyr::n_distinct(data[[".variable_label"]]) == 1

    if (isTRUE(hide_axis_text)) {
      # Store original label before hiding for later retrieval
      data[[".variable_label_original"]] <- data[[".variable_label"]]
      data[[".variable_label"]] <- ""
    }

    percentage <- dots$data_label %in% c("percentage", "percentage_bare")
    prop_family <- dots$data_label %in%
      c("percentage", "percentage_bare", "proportion")

    fp_text_settings <-
      lapply(colour_palette, function(color) {
        officer::fp_text(
          font.size = dots$label_font_size,
          color = hex_bw(color, na_colour = dots$colour_na),
          font.family = dots$font_family
        )
      })

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

    # Determine chart style based on freq parameter
    # freq=FALSE means proportion/percentage plots (stacked)
    # freq=TRUE means frequency plots (dodged)
    if (percentage || isFALSE(dots$freq)) {
      m <- mschart::as_bar_stack(x = m, percent = percentage)
      overlap <- 100
      gap_width <- 50
    } else {
      # Frequency plot settings
      overlap <- -40
      gap_width <- 150
    }

    m <- mschart::chart_settings(
      x = m,
      dir = if (dots$vertical) "vertical" else "horizontal",
      overlap = overlap,
      gap_width = gap_width
    )

    m <- mschart::chart_data_fill(x = m, values = colour_palette)
    m <- mschart::chart_data_stroke(x = m, values = colour_palette)
    if (length(fp_text_settings) > 0) {
      m <- mschart::chart_labels_text(x = m, values = fp_text_settings)
    }
    m <- mschart::chart_labels(x = m, ylab = NULL, xlab = NULL, title = NULL)
    m <- mschart::chart_ax_x(x = m, major_tick_mark = "none")

    if (percentage) {
      m <- mschart::chart_ax_y(
        x = m,
        num_fmt = "0%",
        limit_min = 0,
        limit_max = 1
      )
      # Set major unit for axis breaks (0%, 25%, 50%, 75%, 100%)
      m$y_axis$major_unit <- 0.25
    }

    m <- mschart::chart_theme(
      x = m,
      legend_text = main_text,
      axis_text_x = main_text,
      axis_text_y = main_text,
      grid_major_line_x = blank_border,
      grid_major_line_y = blank_border,
      grid_minor_line_x = blank_border,
      grid_minor_line_y = blank_border,
      legend_position = if (checkbox) "n" else "b"
    )

    # Return mschart object directly if requested
    if (isTRUE(dots$docx_return_object)) {
      return(m)
    }

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
