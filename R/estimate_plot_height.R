# estimate_plot_height <- function(data,
#                                  y_col_pos,
#                                  x_cols = NULL,
#                                  label_separator = " - ",
#                                  vertical = FALSE,
#                                  x_axis_label_width = 20,
#                                  element_name = "uni_cat_prop_plot",
#                                  showNA = "never",
#                                  plot_height_multiplier_per_horizontal_line = .2,
#                                  plot_height_multiplier_per_vertical_letter = .15,
#                                  plot_height_fixed_constant = 1,
#                                  plot_height_max = 8,
#                                  plot_height_min = 1,
#                                  vertical_height = 12,
#                                  strip_angle = 0
#                                  ) {
#
#   if(!vertical) {
#
#     max_axis_lines_per_dep <- get_raw_labels(data = data, col_pos = y_col_pos)
#     max_axis_lines_per_dep <- keep_subitem(max_axis_lines_per_dep, label_separator = label_separator)
#     max_axis_lines_per_dep <- stringi::stri_wrap(max_axis_lines_per_dep,
#                                                  width = x_axis_label_width,
#                                                  simplify = FALSE)
#     # if(any(stringr::str_detect(y_col_pos, "d_"))) browser()
#
#
#
#     n_cats <- dplyr::n_distinct(as.vector(as.matrix(data[, y_col_pos])), na.rm = showNA == "never")
#     multi_line_legend <- ceiling(n_cats/5)
#     if( !stringi::stri_detect(element_name, fixed="freq")) {
#       n_cats <- 1
#     }
#     x_max <- if(length(x_cols)>0 && all(!is.na(x_cols))) {
#
#       dplyr::n_distinct(data[[x_cols]], na.rm = TRUE)
#       }
#
#     if(rlang::is_null(x_cols) || all(is.na(x_cols))) { # Must include NA due to new chapter_overview setup
#
#       max_axis_lines_per_dep <- max(lengths(max_axis_lines_per_dep), na.rm=TRUE)
#
#
#     plot_height_estimate <-
#       max_axis_lines_per_dep *
#       n_cats *
#       length(y_col_pos) *
#       plot_height_multiplier_per_horizontal_line +
#       plot_height_fixed_constant +
#       multi_line_legend
#
#     # Bivariate plots, with dep as vertical facet
#     } else if(strip_angle >= 45 && strip_angle <= 135) {
#
#       max_axis_lines_per_dep <- max(nchar(max_axis_lines_per_dep), na.rm=TRUE)
#
#       deps_as_dominant <-
#         max_axis_lines_per_dep *
#         plot_height_multiplier_per_vertical_letter
#
#       indeps_as_dominant <-
#         x_max *
#         n_cats *
#         plot_height_multiplier_per_horizontal_line
#
#       plot_height_estimate <-
#         max(c(deps_as_dominant, indeps_as_dominant)) *
#         length(y_col_pos) +
#         plot_height_fixed_constant +
#         multi_line_legend
#
#     } else { # Bivariate plots, with dep as facet horizontally
#
#       max_axis_lines_per_dep <- max(lengths(max_axis_lines_per_dep), na.rm=TRUE)
#
#       plot_height_estimate <-
#         max(c(max_axis_lines_per_dep, x_max * n_cats)) *
#         length(y_col_pos) *
#         plot_height_multiplier_per_horizontal_line +
#         plot_height_fixed_constant +
#         multi_line_legend
#     }
#
#     plot_height <-
#       max(c(min(c(plot_height_estimate, plot_height_max)),
#             plot_height_min))
#
#   } else plot_height <- vertical_height
#   plot_height
# }

estimate_plot_height <- function(data,
                                 y_cols,
                                 x_cols = NULL,
                                  label_separator = " - ",
                                 is_vertical = FALSE,
                                  x_axis_label_width = 20,
                                 element_name = "uni_cat_prop_plot",
                                  showNA = "never",
                                  plot_height_multiplier_per_horizontal_line = .2,
                                  plot_height_multiplier_per_vertical_letter = .15,
                                  plot_height_fixed_constant = 0,
                                 plot_height_max = 8,
                                  plot_height_min = 1,
                                 vertical_height = 12,
                                  strip_angle = 0) {

  get_max_lines <- function(data, column, width, separator) {
    lines <- get_raw_labels(data = data, col_pos = column)
    lines <- keep_subitem(lines, label_separator = separator)
    lines <- stringi::stri_wrap(lines, width = width, simplify = FALSE)
    max(lengths(lines), na.rm = TRUE)
  }

  calculate_height <- function(max_lines, y_n_cats,
                               y_n_cols, multiplier, fixed, legend_lines) {
    max_lines * y_n_cats * y_n_cols * multiplier + fixed + legend_lines
  }

  if (!is_vertical) {
    max_lines <- get_max_lines(data, y_cols, x_axis_label_width, label_separator)
    y_n_cats <- dplyr::n_distinct(as.vector(as.matrix(data[, y_cols])), na.rm = showNA == "never")
    legend_lines <- ceiling(y_n_cats / 5)

    if (!stringi::stri_detect(element_name, fixed = "freq")) {
      y_n_cats <- 1
    }

    x_max <- if (length(x_cols) > 0 && all(!is.na(x_cols))) {
      dplyr::n_distinct(data[[x_cols]], na.rm = TRUE)
    } else 0

    if (rlang::is_null(x_cols) || all(is.na(x_cols))) {

      plot_height_estimate <- calculate_height(max_lines = max_lines,
                                               y_n_cats = y_n_cats,
                                               y_n_cols = length(y_cols),
                                               multiplier = plot_height_multiplier_per_horizontal_line,
                                               fixed = plot_height_fixed_constant,
                                               legend_lines = legend_lines)

    } else if (strip_angle >= 45 && strip_angle <= 135) {

      max_letters <- max(nchar(max_lines), na.rm = TRUE)
      deps_height <- max_letters * plot_height_multiplier_per_vertical_letter
      indeps_height <- x_max * y_n_cats * plot_height_multiplier_per_horizontal_line
      plot_height_estimate <- max(c(deps_height, indeps_height)) * length(y_cols) + plot_height_fixed_constant + legend_lines

    } else {

      plot_height_estimate <- calculate_height(max_lines = max(c(max_lines, x_max * y_n_cats)),
                                               y_n_cats = 1,
                                               y_n_cols = length(y_cols),
                                               multiplier = plot_height_multiplier_per_horizontal_line,
                                               fixed = plot_height_fixed_constant,
                                               legend_lines = legend_lines)

    }

    plot_height <- max(c(min(c(plot_height_estimate, plot_height_max)), plot_height_min))
  } else {
    plot_height <- vertical_height
  }

  plot_height
}
