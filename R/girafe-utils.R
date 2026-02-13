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

guess_legend_ncols <- function(ggobj, char_limit = 100) {
  fill_var <- rlang::as_label(ggobj$mapping$fill)
  if (!is.null(fill_var)) {
    lvls <- as.character(unique(ggobj$data[[fill_var]]))
    max_chars <- max(nchar(lvls), na.rm = TRUE)
    for (i in 2:15) {
      if ((max_chars + 5) * i >= char_limit) {
        return(i - 1)
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
