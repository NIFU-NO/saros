estimate_plot_height <- function(y_col_pos,
                                 label_prefix,
                                 x_cols = NULL,
                                 vertical = FALSE,
                                 x_axis_label_width = 20,
                                 element_name = "uni_cat_prop_plot",
                                 data = data,
                                 showNA = "never",
                                 plot_height_multiplier = .16,
                                 plot_height_fixed_constant = 1.1,
                                 plot_height_max = 20,
                                 plot_height_min = 1,
                                 vertical_height = 12
                                 ) {

  if(!vertical) {
    n_var <- length(y_col_pos)

    if(n_var == 1) {
      max_axis_lines_per_var <- 2
    } else {
      max_axis_lines_per_var <- stringi::stri_wrap(label_prefix, width = x_axis_label_width, simplify = FALSE)
      max_axis_lines_per_var <- max(lengths(max_axis_lines_per_var), na.rm=TRUE)
    }

    if(stringi::stri_detect(element_name, fixed="freq")) {
      n_cats <- dplyr::n_distinct(as.vector(as.matrix(data[, y_col_pos])), na.rm = showNA == "never")
    } else {
      n_cats <- 1
    }

    plot_height_estimate <-
      max_axis_lines_per_var *
      n_cats *
      n_var *
      max(c(1, if(length(x_cols)>0) dplyr::n_distinct(data[[x_cols]], na.rm = TRUE))) *
      plot_height_multiplier +
      plot_height_fixed_constant

    plot_height <-
      max(c(min(c(plot_height_estimate, plot_height_max)),
            plot_height_min))

  } else plot_height <- vertical_height
}

