#' Pull global plotting settings before displaying plot
#'
#' This function extends [ggiraph::girafe] by allowing colour palettes to be globally specified.
#' @param ggobj ggplot2-object.
#' @param ... Dots forwarded to [ggiraph::girafe()]
#' @param char_limit Integer. Number of characters to fit on a line of plot (legend-space). Will be
#'    replaced in the future with a function that guesses this.
#' @param label_wrap_width Integer. Number of characters fit on the axis text space before wrapping.
#' @param interactive Boolean. Whether to produce a ggiraph-plot with interactivity (defaults to TRUE)
#'    or a static ggplot2-plot.
#' @param palette_codes Optional list of named character vectors with names being categories and values being colours. The final character vector of the list is taken as a final resort. Defaults to `NULL`.
#' @param priority_palette_codes Optional named character of categories (as names) with corresponding colours (as values) which are used first, whereupon the remaining unspecified categories are pulled from the last vector of `palette_codes`. Defaults to `NULL`.
#' @param ncol Optional integer or NULL.
#' @param byrow Whether to display legend keys by row or by column.
#' @param checked,not_checked Optional string. If specified and the fill categories of the plot matches these, a special plot is returned where not_checked is hidden. Its usefulness comes in plots which are intended for checkbox responses where unchecked is not always a conscious choice.
#' @param colour_2nd_binary_cat Optional string. Color for the second category in binary checkbox plots. When set together with `checked` and `not_checked`, reverses the category order so that `not_checked` appears second and receives this color. Ignored if checkbox criteria are not met.
#' @param pointsize,height_svg,width_svg See [ggiraph::girafe()].
#' @return If interactive, only side-effect of generating ggiraph-plot. If interactive=FALSE, returns modified ggobj.
#' @export
#'
#' @examples
#' plot <- makeme(data = ex_survey, dep = b_1)
#' girafe(plot)
girafe <- function(
  ggobj,
  ...,
  char_limit = 200,
  label_wrap_width = 80,
  interactive = TRUE,
  palette_codes = NULL,
  priority_palette_codes = NULL,
  ncol = NULL,
  byrow = TRUE,
  colour_2nd_binary_cat = NULL,
  checked = NULL,
  not_checked = NULL,
  width_svg = NULL,
  height_svg = NULL,
  pointsize = 12
) {
  # Check global settings, then provided settings, and finally resort to defaults.
  args <- check_options(
    call = match.call(),
    ignore_args = .saros.env$ignore_args,
    defaults_env = global_settings_get(fn_name = "girafe"),
    default_values = formals(girafe)
  )

  # Validate parameters
  check_integerish(args$char_limit, min = 1, arg = "char_limit")
  check_integerish(args$label_wrap_width, min = 1, arg = "label_wrap_width")
  check_bool(args$interactive, arg = "interactive")
  check_string(args$checked, null_allowed = TRUE, arg = "checked")
  check_string(args$not_checked, null_allowed = TRUE, arg = "not_checked")
  check_string(
    args$colour_2nd_binary_cat,
    null_allowed = TRUE,
    arg = "colour_2nd_binary_cat"
  )

  validate_palette_params(
    palette_codes = args$palette_codes,
    priority_palette_codes = args$priority_palette_codes,
    label_wrap_width = args$label_wrap_width,
    ncol = args$ncol,
    byrow = args$byrow
  )

  if (is.null(ggobj) || length(ggobj$data) == 0) {
    return(invisible(data.frame()))
  }
  if (!ggplot2::is_ggplot(ggobj)) {
    if (
      is.list(ggobj) && length(ggobj) == 1 && ggplot2::is_ggplot(ggobj[[1]])
    ) {
      ggobj <- ggobj[[1]]
    } else {
      cli::cli_abort(
        "{.arg ggobj} must be either a ggplot2 object, list of ggplot2 objects (or NULL)."
      )
    }
  }

  fill_levels <- get_fill_levels(ggobj)

  checkbox <- FALSE
  if (!is.null(fill_levels)) {
    if (all(fill_levels %in% c(args$checked, args$not_checked))) {
      checkbox <- TRUE
      ggobj <- convert_to_checkbox_plot(
        ggobj,
        checked = args$checked,
        not_checked = args$not_checked,
        colour_2nd_binary_cat = args$colour_2nd_binary_cat
      )
      # Update fill_levels to match the new order
      if (rlang::is_string(args$colour_2nd_binary_cat)) {
        fill_levels <- c(args$not_checked, args$checked)
      } else {
        fill_levels <- c(args$checked, args$not_checked)
      }
    }

    ggobj <-
      suppressMessages(
        ggobj +
          scale_discrete_special(
            palette_codes = args$palette_codes,
            lvls = fill_levels,
            ncol = args$ncol,
            byrow = args$byrow,
            label_wrap_width = args$label_wrap_width,
            priority_palette_codes = args$priority_palette_codes
          ) +
          ggplot2::guides(
            fill = if (isTRUE(checkbox)) {
              "none"
            } else {
              ggplot2::guide_legend(
                ncol = guess_legend_ncols(
                  ggobj = ggobj,
                  char_limit = args$char_limit
                )
              )
            }
          ),
        classes = "rlang_message"
      )

    if (isTRUE(interactive)) {
      ggiraph::girafe(
        ggobj = ggobj,
        pointsize = args$pointsize,
        width_svg = args$width_svg,
        height_svg = args$height_svg,
        ...
      )
    } else {
      ggobj
    }
  } else {
    ggobj
  }
}
