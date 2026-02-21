#' Extract Fill Levels from ggplot Object
#'
#' Evaluates the fill aesthetic mapping in the context of the plot data
#' to extract fill levels. Handles both simple column references (e.g., `fill = cyl`)
#' and expressions (e.g., `fill = factor(cyl)`).
#'
#' @param ggobj A ggplot2 object
#' @return Character vector of fill levels, or NULL if no fill mapping exists
#' @keywords internal
get_fill_levels <- function(ggobj) {
  if (is.null(ggobj$mapping$fill)) {
    return(NULL)
  }

  # Evaluate the fill mapping in the context of the plot data
  fill_values <- tryCatch(
    rlang::eval_tidy(ggobj$mapping$fill, data = ggobj$data),
    error = function(e) NULL
  )

  if (is.null(fill_values)) {
    return(NULL)
  }

  # Extract levels
  if (is.factor(fill_values)) {
    levels(fill_values)
  } else {
    unique(fill_values)
  }
}

#' Build Custom Palette Function
#'
#' Creates a palette function that matches colors to factor levels based on
#' palette_codes and priority_palette_codes.
#'
#' @param palette_codes List of character vectors containing colours. Vectors can
#'   optionally be named for exact level matching.
#' @param fct_levels Character vector of factor levels to assign colors to.
#' @param priority_palette_codes Optional named character vector where names are
#'   categories and values are colours to use first. Defaults to `NULL`.
#'
#' @return A palette function that takes n (number of colors) and lvls (levels)
#'   and returns a named character vector of colors.
#' @keywords internal
build_custom_palette <- function(
  palette_codes,
  fct_levels,
  priority_palette_codes = NULL
) {
  function(n, lvls = fct_levels) {
    matched_palette <- NULL
    for (i in seq_along(palette_codes)) {
      if (all(lvls %in% names(palette_codes[[i]]))) {
        matched_palette <- palette_codes[[i]]
        break
      }
    }

    if (
      is.null(matched_palette) &&
        (length(palette_codes[[length(palette_codes)]]) >= length(lvls) ||
          !is.null(priority_palette_codes))
    ) {
      # If no match, pick the last vector in list
      # Allow using priority_palette_codes even if base palette is insufficient

      # First, handle priority_palette_codes - extract both named and unnamed elements
      if (!is.null(priority_palette_codes)) {
        # Named elements that match our levels
        matched_palette <- priority_palette_codes[
          !is.na(names(priority_palette_codes)) &
            names(priority_palette_codes) != "" &
            names(priority_palette_codes) %in% lvls
        ]
        # Unnamed elements (NA names or empty string names) are reserved for "NA" category
        unnamed_idx <- is.na(names(priority_palette_codes)) |
          names(priority_palette_codes) == ""
        if (any(unnamed_idx) && "NA" %in% lvls) {
          na_colors <- priority_palette_codes[unnamed_idx]
          matched_palette["NA"] <- na_colors[1]
        }
      } else {
        matched_palette <- character(0)
      }

      # Fill in remaining levels from available_palette
      available_palette <- palette_codes[[length(palette_codes)]]
      remaining_lvls <- lvls[!lvls %in% names(matched_palette)]
      if (length(remaining_lvls) > 0) {
        available_colors <- available_palette[
          !unname(available_palette) %in% unname(matched_palette)
        ]
        # Only use available colors that exist
        n_colors_needed <- length(remaining_lvls)
        n_colors_available <- length(available_colors)
        if (n_colors_available >= n_colors_needed) {
          matched_palette <- c(
            matched_palette,
            stats::setNames(
              available_colors[seq_along(remaining_lvls)],
              remaining_lvls
            )
          )
        } else {
          # Not enough colors - will fall through to generate colors
          matched_palette <- NULL
        }
      }
    }
    if (
      is.null(matched_palette) &&
        length(palette_codes[[length(palette_codes)]]) < length(lvls)
    ) {
      # If no match and insufficient palettes, then generate
      matched_palette <- scales::hue_pal()(length(lvls))
    }

    return(matched_palette)
  }
}

#' Resolve Category Colors for Charts
#'
#' Centralizes color resolution logic for both HTML and DOCX charts.
#' Handles checkbox detection, level reordering, and priority palette setup.
#'
#' @param cat_levels Character vector of category levels
#' @param girafe_settings List of girafe settings from global_settings_get("girafe")
#'
#' @return List with:
#'   - checkbox: Logical indicating if this is a checkbox plot
#'   - cat_levels: Character vector (reordered if checkbox)
#'   - priority_palette_codes: Named character vector for priority colors
#'   - colour_palette: Named character vector of resolved colors
#' @keywords internal
resolve_category_colors <- function(cat_levels, girafe_settings) {
  # Detect checkbox scenario (binary plot matching checked/not_checked)
  checkbox <- length(cat_levels) == 2 &&
    !is.null(girafe_settings$checked) &&
    !is.null(girafe_settings$not_checked) &&
    all(cat_levels %in% c(girafe_settings$checked, girafe_settings$not_checked))

  # Initialize priority palette
  priority_palette_codes <- girafe_settings$priority_palette_codes

  # Handle checkbox plots
  if (checkbox && !is.null(girafe_settings$colour_2nd_binary_cat)) {
    # Match girafe.R behavior: reverse order when colour_2nd_binary_cat is set
    # This makes not_checked come first in the levels list
    cat_levels <- c(girafe_settings$not_checked, girafe_settings$checked)

    # Get second color from palette for checked (now comes second)
    second_palette_color <- if (!is.null(girafe_settings$palette_codes)) {
      palette_codes_last <- girafe_settings$palette_codes[[length(
        girafe_settings$palette_codes
      )]]
      if (length(palette_codes_last) > 1) {
        palette_codes_last[2]
      } else if (length(palette_codes_last) == 1) {
        # If only one color in palette, use it for checked
        palette_codes_last[1]
      } else {
        scales::hue_pal()(2)[2]
      }
    } else {
      scales::hue_pal()(2)[2]
    }

    # Priority palette:
    # - not_checked (first): gets colour_2nd_binary_cat (de-emphasized)
    # - checked (second): gets second color from palette (prominent)
    priority_palette_codes <- c(
      stats::setNames(
        girafe_settings$colour_2nd_binary_cat,
        girafe_settings$not_checked
      ),
      stats::setNames(second_palette_color, girafe_settings$checked)
    )
  } else if (checkbox) {
    # Default checkbox order: checked first, not_checked second
    cat_levels <- c(girafe_settings$checked, girafe_settings$not_checked)
  }

  # Build color palette using shared logic
  palette_fn <- build_custom_palette(
    palette_codes = girafe_settings$palette_codes,
    fct_levels = cat_levels,
    priority_palette_codes = priority_palette_codes
  )

  colour_palette <- palette_fn(length(cat_levels))
  names(colour_palette) <- cat_levels

  list(
    checkbox = checkbox,
    cat_levels = cat_levels,
    priority_palette_codes = priority_palette_codes,
    colour_palette = colour_palette
  )
}

guess_legend_ncols <- function(ggobj, char_limit = 100) {
  fill_var <- rlang::as_label(ggobj$mapping$fill)
  if (!is.null(fill_var)) {
    # Use get_fill_levels to handle expressions like factor(cyl)
    lvls <- get_fill_levels(ggobj)
    if (!is.null(lvls) && length(lvls) > 0) {
      lvls <- as.character(lvls)
      max_chars <- max(nchar(lvls), na.rm = TRUE)
      for (i in 2:15) {
        if ((max_chars + 5) * i >= char_limit) {
          return(i - 1)
        }
      }
    }
  }
  return(NULL)
}

scale_discrete_special <- function(
  aesthetics = "fill",
  palette_codes,
  lvls = NULL,
  ncol = NULL,
  byrow = TRUE,
  label_wrap_width = 80,
  priority_palette_codes = NULL,
  ...
) {
  if (is.null(lvls)) {
    ggiraph::scale_discrete_manual_interactive(
      aesthetics = aesthetics,
      name = "",
      values = palette_codes[[length(palette_codes)]],
      guide = ggiraph::guide_legend_interactive(
        title = "",
        ncol = ncol,
        byrow = byrow
      ),
      labels = ggplot2::label_wrap_gen(
        width = label_wrap_width,
        multi_line = TRUE
      ),
      ...
    )
  } else {
    ggplot2::discrete_scale(
      aesthetics = aesthetics,
      name = "",
      palette = build_custom_palette(
        palette_codes = palette_codes,
        fct_levels = lvls,
        priority_palette_codes = priority_palette_codes
      ),
      guide = ggiraph::guide_legend_interactive(
        title = "",
        ncol = ncol,
        byrow = byrow
      ),
      labels = ggplot2::label_wrap_gen(
        width = label_wrap_width,
        multi_line = TRUE
      ),
      ...
    )
  }
}

convert_to_checkbox_plot <- function(
  ggobj,
  checked = "Selected",
  not_checked = "Not selected",
  colour_2nd_binary_cat = NULL
) {
  # If colour_2nd_binary_cat is set, reverse the category order
  # so the second category (not_checked) gets the specified color
  if (!is.null(colour_2nd_binary_cat)) {
    if (".data_label" %in% names(ggobj$data)) {
      ggobj$data <-
        ggobj$data |>
        dplyr::mutate(
          .category = forcats::fct_relevel(
            .data$.category,
            .env$not_checked,
            .env$checked
          ),
          .data_label = ifelse(
            .data$.category == .env$not_checked,
            "",
            .data$.data_label
          )
        )
    } else {
      ggobj$data <-
        ggobj$data |>
        dplyr::mutate(
          .category = forcats::fct_relevel(
            .data$.category,
            .env$not_checked,
            .env$checked
          )
        )
    }
  } else {
    if (".data_label" %in% names(ggobj$data)) {
      ggobj$data <-
        ggobj$data |>
        dplyr::mutate(
          .category = forcats::fct_relevel(
            .data$.category,
            .env$checked,
            .env$not_checked
          ),
          .data_label = ifelse(
            .data$.category == .env$not_checked,
            "",
            .data$.data_label
          )
        )
    } else {
      ggobj$data <-
        ggobj$data |>
        dplyr::mutate(
          .category = forcats::fct_relevel(
            .data$.category,
            .env$checked,
            .env$not_checked
          )
        )
    }
  }
  ggobj
}
