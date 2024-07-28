estimate_plot_height <- function(data,
                                 y_cols,
                                 x_cols = NULL,
                                 label_separator = " - ",
                                 is_vertical = FALSE,
                                 x_axis_label_width = 20,
                                 element_name = "uni_cat_prop_plot",
                                 showNA = "never",
                                 totals = FALSE,
                                 main_font_size = 8,
                                 plot_height_multiplier_per_horizontal_line = NA,
                                 plot_height_multiplier_per_vertical_letter = .15,
                                 plot_height_multiplier_per_facet = .95,
                                 plot_height_multiplier_per_legend_line = 1.5,
                                 plot_height_fixed_constant = 0,
                                 plot_height_max = 8,
                                 plot_height_min = 1,
                                 vertical_height = 12,
                                 strip_angle = 0) {

  if(is.na(plot_height_multiplier_per_horizontal_line)) {
    plot_height_multiplier_per_horizontal_line <- main_font_size/72.27
  }

  get_max_lines <- function(data, column, width, separator) {
    lines <- get_raw_labels(data = data, col_pos = column)
    lines <- keep_subitem(lines, label_separator = separator)
    lines <- stringi::stri_wrap(lines, width = width, simplify = FALSE)
    max(lengths(lines), na.rm = TRUE)
  }

  calculate_height <- function(strip_height,
                               x_axis_height,
                               n_facets = 1,
                               n_legend_lines,
                               plot_height_multiplier_per_facet,
                               plot_height_multiplier_per_legend_line,
                               plot_height_fixed_constant) {

    max(c(strip_height, x_axis_height), na.rm=TRUE) * n_facets * plot_height_multiplier_per_facet +
      n_legend_lines * plot_height_multiplier_per_legend_line +
      plot_height_fixed_constant
  }

  if(is_vertical) return(vertical_height)

  y_n_cols <- length(y_cols)
  max_lines_y <- get_max_lines(data, column = y_cols,
                               width = x_axis_label_width,
                               separator = label_separator)
  unique_y_cats <- unique(as.vector(as.matrix(data[, y_cols])))
  if(showNA == "never") unique_y_cats <- unique_y_cats[!is.na(unique_y_cats)]
  y_n_cats <- length(unique_y_cats)
  if(y_n_cats==0) unique_y_cats <- 1
  max_nchar_cat_y <- max(nchar(unique_y_cats))
  n_legend_lines <- ceiling(y_n_cats / 5)
  n_legend_lines <- max(c(ceiling(y_n_cats / 5),
                          (max_nchar_cat_y > 10)+1), na.rm = TRUE)

  if(stringi::stri_detect_fixed(element_name, pattern = "freq", negate = TRUE)) {
    y_n_cats <- 1
  }



  if (length(x_cols) > 0 && all(!is.na(x_cols))) {
    bivariate <- TRUE
    max_lines_x <- get_max_lines(data,
                                 column = x_cols,
                                 width = x_axis_label_width,
                                 separator = label_separator)
    unique_x_cats <- unique(as.vector(as.matrix(data[, x_cols])))
    if(showNA == "never") unique_x_cats <- unique_x_cats[!is.na(unique_x_cats)]
    x_n_cats <- length(unique_x_cats) + totals

    x_axis_height <-
      max(c(max_lines_x, y_n_cats), na.rm=TRUE) * plot_height_multiplier_per_horizontal_line * x_n_cats
    n_facets <- y_n_cols

    if (strip_angle >= 45 && strip_angle <= 135) { # vertical strip
      strip_height <-
        max_nchar_cat_y * plot_height_multiplier_per_vertical_letter

    } else { # horizontal strip
      strip_height <-
        max_lines_y * plot_height_multiplier_per_horizontal_line
    }

  } else { # Univariates
    x_axis_height <-
      max(c(max_lines_y, y_n_cats), na.rm=TRUE) *
      plot_height_multiplier_per_horizontal_line * y_n_cols
    strip_height <- NA
    n_facets <- 1

  }
  plot_height_estimate <- calculate_height(strip_height = strip_height,
                                           x_axis_height = x_axis_height,
                                           n_facets = n_facets,
                                           n_legend_lines = n_legend_lines,
                                           plot_height_multiplier_per_facet = plot_height_multiplier_per_facet,
                                           plot_height_multiplier_per_legend_line = plot_height_multiplier_per_legend_line,
                                           plot_height_fixed_constant = plot_height_fixed_constant)
  plot_height <- max(c(min(c(plot_height_estimate, plot_height_max)), plot_height_min))
  round(plot_height, digits=2)
}
