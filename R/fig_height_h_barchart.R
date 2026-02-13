estimate_categories_per_line <- function(
  figure_width_cm = 12,
  max_chars_cats = 20, # Maximum characters across the categories
  legend_key_chars = 5,
  legend_location = "plot",
  x_axis_label_width = 25,
  strip_width = 25,
  font_size = 7,
  margin_cm = 0,
  conversion_font_size_to_char_width_in_cm = .035
) {
  # Calculate the width of one character in cm, assuming a monospace font approximation
  char_width_cm <- font_size * conversion_font_size_to_char_width_in_cm

  # Estimate the width per category in cm
  width_per_category_cm <- (legend_key_chars + max_chars_cats) * char_width_cm

  # Legend offset in cm
  legend_offset <- if (legend_location == "plot") {
    0
  } else if (legend_location == "panel") {
    char_width_cm * sum(c(x_axis_label_width, strip_width), na.rm = TRUE)
  }

  # Calculate the total available width for the legend in cm
  available_width_cm <- figure_width_cm - legend_offset - margin_cm * 2

  # Calculate the number of categories that can fit in one line
  categories_per_line <- available_width_cm / max(c(.01, width_per_category_cm))

  # Return the estimated number of categories per line
  floor(categories_per_line)
}

get_max_lines <- function(max_chars_per_unit, total_width_available) {
  ceiling(max_chars_per_unit / total_width_available)
}

estimate_legend_height <- function(
  n_cats_y,
  n_legend_lines = NULL,
  max_chars_cats_y,
  multiplier_per_legend_line = 1,
  figure_width_in_cm = 14,
  font_size = 7,
  legend_key_chars_equivalence = 5,
  legend_location = "plot",
  x_axis_label_width = 20,
  strip_width = 20,
  margin_in_cm = 0,
  conversion_font_size_to_char_width_in_cm = .035
) {
  ##
  if (is.null(n_legend_lines)) {
    categories_per_line <-
      estimate_categories_per_line(
        figure_width_cm = figure_width_in_cm,
        max_chars_cats = max_chars_cats_y, # Maximum characters across the categories
        font_size = font_size,
        legend_key_chars = legend_key_chars_equivalence,
        legend_location = legend_location,
        x_axis_label_width = x_axis_label_width,
        strip_width = strip_width,
        margin_cm = margin_in_cm,
        conversion_font_size_to_char_width_in_cm = conversion_font_size_to_char_width_in_cm
      )
    n_legend_lines <- get_max_lines(
      n_cats_y,
      max(c(categories_per_line, 1), na.rm = TRUE)
    )
  }
  n_legend_lines * multiplier_per_legend_line
}


combine_figure_heights <- function(
  panel_height,
  legend_height,
  multiplier_per_plot = 1,
  fixed_constant = 0
) {
  (panel_height + legend_height) * multiplier_per_plot + fixed_constant
}


#' Estimate figure height for a horizontal bar chart
#'
#' This function estimates the height of a figure for a horizontal bar chart
#' based on several parameters including the number of dependent and independent
#' variables, number of categories, maximum characters in the labels, and
#' legend properties.
#'
#' @param n_y,n_x Integer. Number of dependent/independent variables.
#' @param n_cats_y Integer. Number of categories across the dependent variables.
#' @param max_chars_labels_y Integer. Maximum number of characters across the dependent variables' labels.
#' @param max_chars_cats_y Integer. Maximum number of characters across the dependent variables' response categories (levels).
#' @param n_cats_x Integer or NULL. Number of categories across the independent variables.
#' @param max_chars_labels_x Integer or NULL. Maximum number of characters across the independent variables' labels.
#' @param max_chars_cats_x Integer or NULL. Maximum number of characters across the independent variables' response categories (levels).
#' @param freq Logical. If TRUE, frequency plot with categories next to each other. If FALSE (default), proportion plot with stacked categories.
#' @param x_axis_label_width,strip_width Numeric. Width allocated for x-axis labels and strip labels respectively.
#' @param strip_angle Integer. Angle of the strip text.
#' @param main_font_size Numeric. Font size for the main text.
#' @param legend_location Character. Location of the legend. "plot" (default) or "panel".
#' @param n_legend_lines Integer. Number of lines in the legend.
#' @param legend_key_chars_equivalence Integer. Approximate number of characters the legend key equals.
#' @param multiplier_per_horizontal_line Numeric. Multiplier per horizontal line.
#' @param multiplier_per_vertical_letter Numeric. Multiplier per vertical letter.
#' @param multiplier_per_bar Numeric. Multiplier per bar height (thickness).
#' @param multiplier_per_facet Numeric. Multiplier per facet height.
#' @param multiplier_per_legend_line Numeric. Multiplier per legend line.
#' @param multiplier_per_plot Numeric. Multiplier for entire plot estimates.
#' @param fixed_constant Numeric. Fixed constant to be added to the height.
#' @param figure_width_in_cm Numeric. Width of the figure in centimeters.
#' @param margin_in_cm Numeric. Margin in centimeters.
#' @param max Numeric. Maximum height.
#' @param min Numeric. Minimum height.
#' @param showNA String, one of "ifany", "always" or "never". Not yet in use.
#' @param hide_axis_text_if_single_variable Boolean. Whether the label is hidden for single dependent variable plots.
#' @param add_n_to_dep_label,add_n_to_indep_label Boolean. If TRUE, will add 10 characters to the max label lengths. This is
#'      primarily useful when obtaining these settings from the global environment,
#'      avoiding the need to compute this for each figure chunk.
#'
#' @return Numeric value representing the estimated height of the figure.
#' @export
#'
#' @examples
#' fig_height_h_barchart(
#'   n_y = 5,
#'   n_cats_y = 3,
#'   max_chars_labels_y = 20,
#'   max_chars_cats_y = 8,
#'   n_x = 1,
#'   n_cats_x = 4,
#'   max_chars_labels_x = 12,
#'   freq = FALSE,
#'   x_axis_label_width = 20,
#'   strip_angle = 0,
#'   main_font_size = 8,
#'   legend_location = "panel",
#'   n_legend_lines = 2,
#'   legend_key_chars_equivalence = 5,
#'   multiplier_per_horizontal_line = 1,
#'   multiplier_per_vertical_letter = .15,
#'   multiplier_per_facet = .95,
#'   multiplier_per_legend_line = 1.5,
#'   figure_width_in_cm = 16
#' )
fig_height_h_barchart <- # Returns a numeric value
  function(
    n_y,
    n_cats_y,
    max_chars_labels_y = 20,
    max_chars_cats_y = 20,
    n_x = NULL,
    n_cats_x = NULL,
    max_chars_labels_x = NULL,
    max_chars_cats_x = NULL,
    freq = FALSE,
    x_axis_label_width = 20,
    strip_width = 20,
    strip_angle = 0,
    main_font_size = 7,
    legend_location = c("plot", "panel"),
    n_legend_lines = NULL,
    legend_key_chars_equivalence = 5,
    multiplier_per_horizontal_line = 1,
    multiplier_per_vertical_letter = 1,
    multiplier_per_facet = 1,
    multiplier_per_bar = 1,
    multiplier_per_legend_line = 1,
    multiplier_per_plot = 1,
    fixed_constant = 0,
    margin_in_cm = 0,
    figure_width_in_cm = 14,
    max = 12,
    min = 2,
    hide_axis_text_if_single_variable = FALSE,
    add_n_to_dep_label = FALSE,
    add_n_to_indep_label = FALSE,
    showNA = c("ifany", "never", "always")
  ) {
    ######

    args <- check_options(
      call = match.call(),
      ignore_args = .saros.env$ignore_args,
      defaults_env = global_settings_get(fn_name = "fig_height_h_barchart"),
      default_values = formals(fig_height_h_barchart)
    )

    n_x <- args$n_x
    n_cats_x <- args$n_cats_x
    max_chars_labels_x <- args$max_chars_labels_x
    max_chars_cats_x <- args$max_chars_cats_x
    freq <- args$freq
    x_axis_label_width <- args$x_axis_label_width
    strip_width <- args$strip_width
    strip_angle <- args$strip_angle
    main_font_size <- args$main_font_size
    legend_location <- args$legend_location
    n_legend_lines <- args$n_legend_lines
    legend_key_chars_equivalence <- args$legend_key_chars_equivalence
    multiplier_per_horizontal_line <- args$multiplier_per_horizontal_line
    multiplier_per_vertical_letter <- args$multiplier_per_vertical_letter
    multiplier_per_facet <- args$multiplier_per_facet
    multiplier_per_legend_line <- args$multiplier_per_legend_line
    multiplier_per_bar <- args$multiplier_per_bar
    multiplier_per_plot <- args$multiplier_per_plot
    fixed_constant <- args$fixed_constant
    figure_width_in_cm <- args$figure_width_in_cm
    margin_in_cm <- args$margin_in_cm
    max <- args$max
    min <- args$min
    hide_axis_text_if_single_variable <- args$hide_axis_text_if_single_variable
    showNA <- args$showNA[[1]]

    check_integerish(n_y)
    check_integerish(n_cats_y)
    check_integerish(max_chars_labels_y)
    check_integerish(max_chars_cats_y)
    check_integerish(n_x, null_allowed = TRUE)
    check_integerish(n_cats_x, null_allowed = TRUE)
    check_integerish(max_chars_labels_x, null_allowed = TRUE)
    check_integerish(max_chars_cats_x, null_allowed = TRUE)
    check_bool(freq)
    check_double(strip_angle)
    check_double(main_font_size)
    check_double(multiplier_per_horizontal_line)
    check_double(multiplier_per_vertical_letter)
    check_double(multiplier_per_facet)
    check_double(multiplier_per_legend_line)
    check_double(multiplier_per_bar)
    check_double(multiplier_per_plot)
    check_double(fixed_constant)
    check_integerish(legend_key_chars_equivalence)

    check_integerish(n_legend_lines, null_allowed = TRUE)
    check_integerish(margin_in_cm)
    check_integerish(figure_width_in_cm)
    check_integerish(strip_angle)
    check_integerish(max)
    check_integerish(min)
    legend_location <- legend_location[1]
    check_bool(hide_axis_text_if_single_variable)
    check_bool(add_n_to_dep_label)
    check_bool(add_n_to_indep_label)

    multiplier_per_horizontal_line <-
      multiplier_per_horizontal_line * main_font_size / 72.27 * 2.54

    ################
    # Legend height
    ###############

    legend_height <-
      estimate_legend_height(
        n_cats_y = n_cats_y,
        n_legend_lines = n_legend_lines,
        figure_width_in_cm = figure_width_in_cm,
        max_chars_cats_y = max_chars_cats_y,
        font_size = main_font_size,
        legend_key_chars_equivalence = legend_key_chars_equivalence,
        legend_location = legend_location,
        x_axis_label_width = x_axis_label_width,
        strip_width = x_axis_label_width,
        margin_in_cm = margin_in_cm,
        conversion_font_size_to_char_width_in_cm = .035
      ) *
      multiplier_per_horizontal_line

    ################
    # Y-axis height
    ###############

    # A: No indep, single dep, hide_axis_text_if_single_variable = TRUE
    # B: No indep, other cases
    # C: Single indep, single dep, hide_axis_text_if_single_variable = TRUE
    # D: Single indep, other cases

    if (isFALSE(freq)) {
      n_cats_y <- 1
    }

    ## Dependent variables height (within a facet if any)

    maximum_characters_for_labels_in_y <-
      if (isTRUE(hide_axis_text_if_single_variable) && n_y == 1) {
        1 # Will make max_lines_y equal to 1
      } else {
        max_chars_labels_y + add_n_to_dep_label * 10
      }

    # Univariates
    if (is.null(n_x) || n_x == 0) {
      max_lines_y <- get_max_lines(
        max_chars_per_unit = maximum_characters_for_labels_in_y,
        total_width_available = max(c(x_axis_label_width, 1), na.rm = TRUE)
      )

      max_lines_y_per_facet <-
        max_lines_y *
        n_y *
        n_cats_y # To account for freq-plots. unsure if valid in bivariate plots

      n_facets <- 1
      max_lines_x_per_facet <- 0
      height_per_strip <- 0
    }
    # Bivariates
    if (!is.null(n_x) && n_x > 1) {
      cli::cli_abort(
        "This function only supports a single {.arg n_x} for now, not {n_x}."
      )
    }
    if (!is.null(n_x) && n_x == 1) {
      max_lines_y <- get_max_lines(
        max_chars_per_unit = maximum_characters_for_labels_in_y,
        total_width_available = max(c(strip_width, 1), na.rm = TRUE)
      )
      max_lines_y_per_facet <-
        max_lines_y *
        n_cats_y # To account for freq-plots. unsure if valid in bivariate plots

      n_facets <- n_y

      if (strip_angle >= 45 && strip_angle <= 135) {
        # vertical strip height (one strip)
        height_per_strip <-
          max_chars_labels_y * # Incorrect, should really be the longest line in the split variable labels
          multiplier_per_vertical_letter
      } else if (strip_angle < 45 || strip_angle > 135) {
        # horizontal strip height (one strip)
        height_per_strip <-
          max_lines_y *
          multiplier_per_horizontal_line
      }

      max_lines_x_per_bar <- get_max_lines(
        max_chars_per_unit = max_chars_cats_x + add_n_to_indep_label * 10,
        total_width_available = max(c(x_axis_label_width, 1), na.rm = TRUE)
      )
      max_lines_x_per_facet <-
        max_lines_x_per_bar *
        max(c(n_cats_x, 1), na.rm = TRUE)
    }

    height_per_facet <-
      max(c(max_lines_y_per_facet, max_lines_x_per_facet), na.rm = TRUE) *
      multiplier_per_horizontal_line *
      multiplier_per_bar

    panel_height <-
      max(c(height_per_facet, height_per_strip), na.rm = TRUE) *
      n_facets *
      multiplier_per_facet

    ### Put it all together
    plot_height <-
      combine_figure_heights(
        panel_height = panel_height,
        legend_height = legend_height,
        multiplier_per_plot = multiplier_per_plot,
        fixed_constant = fixed_constant
      )
    plot_height <- max(
      c(min(c(plot_height, max), na.rm = TRUE), min),
      na.rm = TRUE
    )
    # plot_height <- plot_height / 2.54
    round(plot_height, digits = 2)
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
#' fig_height_h_barchart2(makeme(data = ex_survey, dep = b_1:b_2, indep = x1_sex))
fig_height_h_barchart2 <- # Returns a numeric value
  function(
    ggobj,
    main_font_size = 7,
    strip_angle = 0,
    freq = FALSE,
    x_axis_label_width = 20,
    strip_width = 20,
    legend_location = c("plot", "panel"),
    n_legend_lines = NULL,
    showNA = c("ifany", "never", "always"),
    legend_key_chars_equivalence = 5,
    multiplier_per_horizontal_line = NULL,
    multiplier_per_vertical_letter = 1,
    multiplier_per_facet = 1,
    multiplier_per_legend_line = 1,
    fixed_constant = 0,
    figure_width_in_cm = 14,
    margin_in_cm = 0,
    max = 12,
    min = 1
  ) {
    data <- ggobj$data

    if (!(inherits(data, "data.frame") && nrow(data) > 0)) {
      cli::cli_warn(
        "{.arg ggobj} must be a ggplot2-object with a nrow>0 data in it. Returning {.arg min}: {.val {min}}."
      )
      return(min)
    }

    # Check if this is an int_plot_html (has .value instead of .category)
    if (is_int_plot_html(data)) {
      # int_plot_html uses simple default height
      # Forward to fig_height_h_barchart with minimal parameters
      max_value <- max
      min_value <- min

      n_y <- dplyr::n_distinct(data$.variable_name)
      max_chars_labels_y <- base::max(
        nchar(as.character(data$.variable_label)),
        na.rm = TRUE
      )

      return(
        fig_height_h_barchart(
          n_y = n_y,
          n_cats_y = 1, # int_plot_html doesn't have categories
          max_chars_labels_y = max_chars_labels_y,
          max_chars_cats_y = 0,
          n_x = NULL,
          n_cats_x = NULL,
          max_chars_labels_x = NULL,
          max_chars_cats_x = NULL,
          fixed_constant = max_value, # Use max as the base height
          multiplier_per_plot = 0, # Disable scaling
          max = max_value,
          min = min_value
        )
      )
    }

    # TODO: Should find a more robust way to identify the indep variable
    indep_vars <- colnames(data)[
      !colnames(data) %in% .saros.env$summary_data_sort2
    ]

    if (length(indep_vars) > 1) {
      cli::cli_abort(
        "{.arg fig_height_h_barchart2} only supports a single indep variable."
      )
    }
    if (length(indep_vars) == 1) {
      data[[indep_vars]] <-
        stringi::stri_replace_all_regex(
          str = as.character(data[[indep_vars]]),
          pattern = "(.+)___.+",
          replacement = "$1",
          dot_all = TRUE
        )
    }

    call <- match.call()

    args <- check_options(
      call = call,
      ignore_args = .saros.env$ignore_args,
      defaults_env = global_settings_get(fn_name = "fig_height_h_barchart"),
      default_values = formals(fig_height_h_barchart)
    )

    freq <- args$freq
    x_axis_label_width <- args$x_axis_label_width
    strip_width <- args$strip_width
    strip_angle <- args$strip_angle
    main_font_size <- args$main_font_size
    legend_location <- args$legend_location
    n_legend_lines <- args$n_legend_lines
    legend_key_chars_equivalence <- args$legend_key_chars_equivalence
    multiplier_per_horizontal_line <- args$multiplier_per_horizontal_line
    multiplier_per_vertical_letter <- args$multiplier_per_vertical_letter
    multiplier_per_facet <- args$multiplier_per_facet
    multiplier_per_legend_line <- args$multiplier_per_legend_line
    multiplier_per_bar <- args$multiplier_per_bar
    fixed_constant <- args$fixed_constant
    figure_width_in_cm <- args$figure_width_in_cm
    margin_in_cm <- args$margin_in_cm
    max_value <- args$max
    min_value <- args$min
    hide_axis_text_if_single_variable <- args$hide_axis_text_if_single_variable
    showNA <- args$showNA[[1]]

    n_y <- dplyr::n_distinct(data$.variable_name)
    n_cats_y <- dplyr::n_distinct(data$.category)
    max_chars_cats_y <- max(nchar(as.character(data$.category)), na.rm = TRUE)
    max_chars_labels_y <- max(
      nchar(as.character(data$.variable_label)),
      na.rm = TRUE
    )
    n_x <- if (length(indep_vars) == 1) 1
    n_cats_x <- if (length(indep_vars) == 1) {
      dplyr::n_distinct(data[[indep_vars]])
    }
    max_chars_cats_x <- if (length(indep_vars) == 1) {
      max(nchar(as.character(data[[indep_vars]])), na.rm = TRUE)
    }
    max_chars_labels_x <- if (length(indep_vars) == 1) {
      nchar(as.character(attr(data[[indep_vars]], "label")))
    }
    if (length(max_chars_labels_x) == 0) {
      max_chars_labels_x <- 0
    }

    fig_height_h_barchart(
      n_y = n_y,
      n_cats_y = n_cats_y,
      max_chars_labels_y = max_chars_labels_y,
      max_chars_cats_y = max_chars_cats_y,
      n_x = n_x,
      n_cats_x = n_cats_x,
      max_chars_labels_x = max_chars_labels_x,
      max_chars_cats_x = max_chars_cats_x,
      freq = freq, # In makeme
      x_axis_label_width = x_axis_label_width, # in makeme
      strip_width = strip_width, # In makeme
      strip_angle = strip_angle,
      main_font_size = main_font_size,
      legend_location = legend_location,
      n_legend_lines = n_legend_lines,
      showNA = showNA,
      hide_axis_text_if_single_variable = hide_axis_text_if_single_variable,
      legend_key_chars_equivalence = legend_key_chars_equivalence,
      multiplier_per_horizontal_line = multiplier_per_horizontal_line,
      multiplier_per_vertical_letter = multiplier_per_vertical_letter,
      multiplier_per_facet = multiplier_per_facet,
      multiplier_per_bar = multiplier_per_bar,
      multiplier_per_legend_line = multiplier_per_legend_line,
      fixed_constant = fixed_constant,
      margin_in_cm = margin_in_cm,
      figure_width_in_cm = figure_width_in_cm,
      max = max_value,
      min = min_value
    )
  }
