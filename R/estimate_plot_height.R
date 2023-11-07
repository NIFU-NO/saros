estimate_plot_height <- function(data,
                                 y_col_pos,
                                 x_cols = NULL,
                                 label_separator = " - ",
                                 vertical = FALSE,
                                 x_axis_label_width = 20,
                                 element_name = "uni_cat_prop_plot",
                                 showNA = "never",
                                 plot_height_multiplier = .04,
                                 plot_height_fixed_constant = 1,
                                 plot_height_max = 20,
                                 plot_height_min = 1,
                                 vertical_height = 12
                                 ) {

  if(!vertical) {

    max_axis_lines_per_var <- get_raw_labels(data = data, col_pos = y_col_pos)
    max_axis_lines_per_var <- keep_subitem(max_axis_lines_per_var, label_separator = label_separator)
    max_axis_lines_per_var <- stringi::stri_wrap(max_axis_lines_per_var, width = x_axis_label_width)
    max_axis_lines_per_var <- max(nchar(max_axis_lines_per_var), na.rm=TRUE)

    if(stringi::stri_detect(element_name, fixed="freq")) {
      n_cats <- dplyr::n_distinct(as.vector(as.matrix(data[, y_col_pos])), na.rm = showNA == "never")
    } else {
      n_cats <- 1
    }
    x_max <- if(length(x_cols)>0 && all(!is.na(x_cols))) {
      dplyr::n_distinct(data[[x_cols]], na.rm = TRUE)
      }

    plot_height_estimate <-
      max_axis_lines_per_var *
      n_cats *
      length(y_col_pos) *
      max(c(1, x_max)) *
      plot_height_multiplier +
      plot_height_fixed_constant

    plot_height <-
      max(c(min(c(plot_height_estimate, plot_height_max)),
            plot_height_min))

  } else plot_height <- vertical_height
  plot_height
}

