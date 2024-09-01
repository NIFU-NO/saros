
# Function to estimate the number of categories that can fit on one line of a legend
estimate_categories_per_line <- function(figure_width_cm = 12,
                                         max_chars_cats = 20, # Maximum characters across the categories
                                         legend_key_chars = 5,
                                         font_size = 8,
                                         margin_cm = 0) {
  # Calculate the width of one character in cm, assuming a monospace font approximation
  char_width_cm <- font_size * 0.035

  # Estimate the width per category in cm
  width_per_category_cm <- (legend_key_chars + max_chars_cats) * char_width_cm

  # Calculate the total available width for the legend in cm
  available_width_cm <- figure_width_cm - margin_cm * 2

  # Calculate the number of categories that can fit in one line
  categories_per_line <- available_width_cm / width_per_category_cm

  # Return the estimated number of categories per line
  floor(categories_per_line)
}


get_max_lines <- function(max_cat_char, width) {
  ceiling(max_cat_char / width)
}



calculate_height <- function(strip_height,
                             x_axis_height,
                             n_facets = 1,
                             n_legend_lines,
                             multiplier_per_facet,
                             multiplier_per_legend_line,
                             fixed_constant) {

  max(c(strip_height, x_axis_height), na.rm=TRUE) * n_facets * multiplier_per_facet +
    n_legend_lines * multiplier_per_legend_line +
    fixed_constant
}


#' Estimate figure height for a horizontal bar chart
#'
#' This function estimates the height of a figure for a horizontal bar chart
#' based on several parameters including the number of dependent and independent
#' variables, number of categories, maximum characters in the labels, and
#' legend properties.
#'
#' @param n_y Integer. Number of dependent variables.
#' @param n_cats_y Integer. Number of categories across the dependent variables.
#' @param max_chars_y Integer. Maximum number of characters across the dependent variables.
#' @param n_x Integer. Number of independent variables.
#' @param n_cats_x Integer. Number of categories across the independent variables.
#' @param max_chars_x Integer. Maximum number of characters across the independent variables.
#' @param freq Logical. If TRUE, frequency plot with categories next to each other. If FALSE (default), proportion plot with stacked categories.
#' @param x_axis_label_width Numeric. Width allocated for x-axis labels.
#' @param strip_angle Integer. Angle of the strip text.
#' @param main_font_size Numeric. Font size for the main text.
#' @param legend_location Character. Location of the legend ("panel" or "plot").
#' @param n_legend_lines Integer. Number of lines in the legend.
#' @param legend_key_chars_equivalence Integer. Approximate number of characters the legend key equals.
#' @param max_chars_per_figure_width Integer. Maximum number of characters per figure width.
#' @param multiplier_per_horizontal_line Numeric. Multiplier per horizontal line.
#' @param multiplier_per_vertical_letter Numeric. Multiplier per vertical letter.
#' @param multiplier_per_facet Numeric. Multiplier per facet.
#' @param multiplier_per_legend_line Numeric. Multiplier per legend line.
#' @param fixed_constant Numeric. Fixed constant to be added to the height.
#' @param figure_width_in_cm Numeric. Width of the figure in centimeters.
#' @param margin_in_cm Numeric. Margin in centimeters.
#' @param max Numeric. Maximum height.
#' @param min Numeric. Minimum height.
#'
#' @return Numeric value representing the estimated height of the figure.
#' @export
#'
#' @examples
#' fig_height_h_barchart(n_y = 5,
#'                       n_cats_y = 3,
#'                       max_chars_y = 10,
#'                       n_x = 2,
#'                       n_cats_x = 4,
#'                       max_chars_x = 12,
#'                       freq = FALSE,
#'                       x_axis_label_width = 20,
#'                       strip_angle = 0,
#'                       main_font_size = 8,
#'                       legend_location = "panel",
#'                       n_legend_lines = 2,
#'                       legend_key_chars_equivalence = 5,
#'                       max_chars_per_figure_width = 100,
#'                       multiplier_per_horizontal_line = NULL,
#'                       multiplier_per_vertical_letter = .15,
#'                       multiplier_per_facet = .95,
#'                       multiplier_per_legend_line = 1.5,
#'                       fixed_constant = 0,
#'                       figure_width_in_cm = 16,
#'                       margin_in_cm = 0,
#'                       max = 8,
#'                       min = 1)
fig_height_h_barchart <- # Returns a numeric value
  function(n_y,
           n_cats_y,
           max_chars_y,
           n_x = NULL,
           n_cats_x = NULL,
           max_chars_x = NULL,
           freq = FALSE,
           x_axis_label_width = 20,
           strip_angle = 0,
           main_font_size = 7,
           legend_location = c("plot", "panel"),
           n_legend_lines = 2,
           legend_key_chars_equivalence = 5,
           max_chars_per_figure_width = 100,
           multiplier_per_horizontal_line = NULL,
           multiplier_per_vertical_letter = 1,
           multiplier_per_facet = 1,
           multiplier_per_legend_line = 1,
           fixed_constant = 0,
           figure_width_in_cm = 14,
           margin_in_cm = 0,
           max = 8,
           min = 1) {

    args <- check_options(call = match.call(),
                          ignore_args = .saros.env$ignore_args,
                          defaults_env = global_settings_get(fn_name="fig_height_h_barchart"),
                          default_values = formals(fig_height_h_barchart))

    n_x <- args$n_x
    n_cats_x <- args$n_cats_x
    max_chars_x <- args$max_chars_x
    freq <- args$freq
    x_axis_label_width <- args$x_axis_label_width
    strip_angle <- args$strip_angle
    main_font_size <- args$main_font_size
    legend_location <- args$legend_location
    n_legend_lines <- args$n_legend_lines
    legend_key_chars_equivalence <- args$legend_key_chars_equivalence
    max_chars_per_figure_width <- args$max_chars_per_figure_width
    multiplier_per_horizontal_line <- args$multiplier_per_horizontal_line
    multiplier_per_vertical_letter <- args$multiplier_per_vertical_letter
    multiplier_per_facet <- args$multiplier_per_facet
    multiplier_per_legend_line <- args$multiplier_per_legend_line
    fixed_constant <- args$fixed_constant
    figure_width_in_cm <- args$figure_width_in_cm
    margin_in_cm <- args$margin_in_cm
    max <- args$max
    min <- args$min

    check_integerish(n_y)
    check_integerish(n_cats_y)
    check_integerish(max_chars_y)
    check_integerish(n_x, null_allowed=TRUE)
    check_integerish(n_cats_x, null_allowed=TRUE)
    check_integerish(max_chars_x, null_allowed=TRUE)
    check_bool(freq)
    check_double(strip_angle)
    check_double(main_font_size)
    check_double(multiplier_per_horizontal_line, null_allowed=TRUE)
    check_double(multiplier_per_vertical_letter)
    check_double(multiplier_per_legend_line)
    check_integerish(legend_key_chars_equivalence)
    check_integerish(max_chars_per_figure_width)

    check_integerish(n_legend_lines, null_allowed= TRUE)
    check_integerish(fixed_constant)
    check_integerish(margin_in_cm)
    check_integerish(figure_width_in_cm)
    check_integerish(strip_angle)
    check_integerish(max)
    check_integerish(min)
    legend_location <- legend_location[1]



    if(is.null(multiplier_per_horizontal_line)) {
      multiplier_per_horizontal_line <- main_font_size/72.27
    }



    if(is.null(n_legend_lines)) {
      categories_per_line <-
        estimate_categories_per_line(figure_width_cm = figure_width_in_cm,
                                     max_chars_cats = max_chars_y, # Maximum characters across the categories
                                     font_size = main_font_size,
                                     legend_key_chars = legend_key_chars_equivalence,
                                     margin_cm = margin_in_cm)
      n_legend_lines <- ceiling(n_y / categories_per_line)
    }



    max_lines_y <- get_max_lines(max_cat_char = max_chars_y,
                                 width = x_axis_label_width)
    if(n_cats_y==0) unique_y_cats <- 1

    if(isFALSE(freq)) {
      n_cats_y <- 1
    }



    if(!is.null(n_x) && n_x > 0) {

      max_lines_x <- get_max_lines(max_cat_char = max_chars_x,
                                   width = x_axis_label_width)

      x_axis_height <-
        max(c(max_lines_x, n_cats_y), na.rm=TRUE) * multiplier_per_horizontal_line * n_cats_x
      n_facets <- n_y

      if (strip_angle >= 45 && strip_angle <= 135) { # vertical strip
        strip_height <-
          max_chars_y * multiplier_per_vertical_letter

      } else { # horizontal strip
        strip_height <-
          max_lines_y * multiplier_per_horizontal_line
      }

    } else { # Univariates
      x_axis_height <-
        max(c(max_lines_y, n_cats_y), na.rm=TRUE) *
        multiplier_per_horizontal_line * n_y
      strip_height <- NA_real_
      n_facets <- 1

    }
    estimate <- calculate_height(strip_height = strip_height,
                                 x_axis_height = x_axis_height,
                                 n_facets = n_facets,
                                 n_legend_lines = n_legend_lines,
                                 multiplier_per_facet = multiplier_per_facet,
                                 multiplier_per_legend_line = multiplier_per_legend_line,
                                 fixed_constant = fixed_constant)
    plot_height <- max(c(min(c(estimate, max)), min))
    round(plot_height, digits=2)
  }


#' Estimate figure height for a horizontal bar chart
#'
#' Taking an object from `makeme()`, this function estimates the height of a
#' figure for a horizontal bar chart.
#'
#' @param ggobj `ggplot2`-object
#' @inheritParams fig_height_h_barchart
#'
#' @inherit fig_height_h_barchart return
#' @export
#'
#' @examples
#' fig_height_h_barchart2(makeme(data=ex_survey, dep=b_1:b_3, indep=x1_sex))
fig_height_h_barchart2 <- # Returns a numeric value
  function(ggobj,
           freq = FALSE,
           x_axis_label_width = 20,
           strip_angle = 0,
           main_font_size = 8,
           legend_location = c("panel", "plot"),
           n_legend_lines = 2,
           legend_key_chars_equivalence = 5,
           max_chars_per_figure_width = 100,
           multiplier_per_horizontal_line = NULL,
           multiplier_per_vertical_letter = .01,
           multiplier_per_facet = .95,
           multiplier_per_legend_line = 1.5,
           fixed_constant = 0,
           figure_width_in_cm = 16,
           margin_in_cm = 0,
           max = 8,
           min = 1) {

    data <- ggobj$data
    if(!(inherits(data, "data.frame") && nrow(data)>0)) {
      cli::cli_warn("{.arg ggobj} must be a ggplot2-object with a nrow>0 data in it. Returning {.arg min}: {.val {min}}.")
      return(min)
    }
    indep_vars <- colnames(data)[!colnames(data) %in% .saros.env$summary_data_sort2]

    if(length(indep_vars)>1) {
      cli::cli_abort("{.arg fig_height_h_barchart2} only supports a single indep variable.")
    }
    if(length(indep_vars)==1) {
      # browser()
      data[[indep_vars]] <-
        stringi::stri_replace_all_regex(
          str = as.character(data[[indep_vars]]),
          pattern = "(.+)___.+",
          replacement = "$1")
    }



    args <- check_options(call = match.call(),
                          ignore_args = .saros.env$ignore_args,
                          defaults_env = global_settings_get(fn_name="fig_height_h_barchart"),
                          default_values = formals(fig_height_h_barchart2))


    n_y = dplyr::n_distinct(data$.variable_name)
    n_cats_y = dplyr::n_distinct(data$.category)
    max_chars_y = max(nchar(as.character(data$.variable_label)), na.rm=TRUE)
    n_x = if(length(indep_vars)==1) 1
    n_cats_x = if(length(indep_vars)==1) dplyr::n_distinct(data[[indep_vars]])
    max_chars_x = if(length(indep_vars)==1) max(nchar(as.character(data[[indep_vars]])), na.rm=TRUE)

    freq <- args$freq
    x_axis_label_width <- args$x_axis_label_width
    strip_angle <- args$strip_angle
    main_font_size <- args$main_font_size
    legend_location <- args$legend_location
    n_legend_lines <- args$n_legend_lines
    legend_key_chars_equivalence <- args$legend_key_chars_equivalence
    max_chars_per_figure_width <- args$max_chars_per_figure_width
    multiplier_per_horizontal_line <- args$multiplier_per_horizontal_line
    multiplier_per_vertical_letter <- args$multiplier_per_vertical_letter
    multiplier_per_facet <- args$multiplier_per_facet
    multiplier_per_legend_line <- args$multiplier_per_legend_line
    fixed_constant <- args$fixed_constant
    figure_width_in_cm <- args$figure_width_in_cm
    margin_in_cm <- args$margin_in_cm
    max <- args$max
    min <- args$min

    check_integerish(n_y)
    check_integerish(n_cats_y)
    check_integerish(max_chars_y)
    check_integerish(n_x, null_allowed=TRUE)
    check_integerish(n_cats_x, null_allowed=TRUE)
    check_integerish(max_chars_x, null_allowed=TRUE)
    check_bool(freq)
    check_double(strip_angle)
    check_double(main_font_size)
    check_double(multiplier_per_horizontal_line, null_allowed=TRUE)
    check_double(multiplier_per_vertical_letter)
    check_double(multiplier_per_legend_line)
    check_integerish(legend_key_chars_equivalence)
    check_integerish(max_chars_per_figure_width)

    check_integerish(n_legend_lines, null_allowed= TRUE)
    check_integerish(fixed_constant)
    check_integerish(margin_in_cm)
    check_integerish(figure_width_in_cm)
    check_integerish(strip_angle)
    check_integerish(max)
    check_integerish(min)
    legend_location <- legend_location[1]



    if(is.null(multiplier_per_horizontal_line)) {
      multiplier_per_horizontal_line <- main_font_size/72.27
    }



    if(is.null(n_legend_lines)) {
      categories_per_line <-
        estimate_categories_per_line(figure_width_cm = figure_width_in_cm,
                                     max_chars_cats = max_chars_y, # Maximum characters across the categories
                                     font_size = main_font_size,
                                     legend_key_chars = legend_key_chars_equivalence,
                                     margin_cm = margin_in_cm)
      n_legend_lines <- ceiling(n_y / categories_per_line)
    }



    max_lines_y <- get_max_lines(max_cat_char = max_chars_y,
                                 width = x_axis_label_width)
    if(n_cats_y==0) unique_y_cats <- 1

    if(isFALSE(freq)) {
      n_cats_y <- 1
    }



    if(!is.null(n_x) && n_x > 0) {

      max_lines_x <- get_max_lines(max_cat_char = max_chars_x,
                                   width = x_axis_label_width)

      x_axis_height <-
        max(c(max_lines_x, n_cats_y), na.rm=TRUE) * multiplier_per_horizontal_line * n_cats_x
      n_facets <- n_y

      if (strip_angle >= 45 && strip_angle <= 135) { # vertical strip
        strip_height <-
          max_chars_y * multiplier_per_vertical_letter

      } else { # horizontal strip
        strip_height <-
          max_lines_y * multiplier_per_horizontal_line
      }

    } else { # Univariates
      x_axis_height <-
        max(c(max_lines_y, n_cats_y), na.rm=TRUE) *
        multiplier_per_horizontal_line * n_y
      strip_height <- NA_real_
      n_facets <- 1

    }
    estimate <- calculate_height(strip_height = strip_height,
                                 x_axis_height = x_axis_height,
                                 n_facets = n_facets,
                                 n_legend_lines = n_legend_lines,
                                 multiplier_per_facet = multiplier_per_facet,
                                 multiplier_per_legend_line = multiplier_per_legend_line,
                                 fixed_constant = fixed_constant)
    plot_height <- max(c(min(c(estimate, max)), min))
    round(plot_height, digits=2)


  }

