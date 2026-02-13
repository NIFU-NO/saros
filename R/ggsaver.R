#' Wrapper Function for [ggplot2::ggsave()] with Palette Support
#'
#' Saves ggplot2 objects with automatic palette application from global settings.
#' Inherits palette settings from [girafe()] global options, ensuring saved plots
#' match the appearance of interactive plots.
#'
#' @param plot A ggplot2 object to save.
#' @param filename File path where the plot should be saved.
#' @param palette_codes Optional list of named character vectors with names being
#'   categories and values being colours. Inherits from [girafe()] global settings
#'   if not specified. The final character vector of the list is taken as a final
#'   resort. Defaults to `NULL`.
#' @param priority_palette_codes Optional named character of categories (as names)
#'   with corresponding colours (as values) which are used first. Inherits from
#'   [girafe()] global settings if not specified. Defaults to `NULL`.
#' @param label_wrap_width Integer. Number of characters fit on the legend labels
#'   before wrapping. Inherits from [girafe()] global settings. Defaults to `80`.
#' @param ncol Optional integer or NULL for legend columns. Inherits from
#'   [girafe()] global settings. Defaults to `NULL`.
#' @param byrow Whether to display legend keys by row or by column. Inherits from
#'   [girafe()] global settings. Defaults to `TRUE`.
#' @param ... Arguments forwarded to [ggplot2::ggsave()]
#'
#' @details
#' This function extends [ggplot2::ggsave()] by applying colour palettes before
#' saving, ensuring consistency between interactive plots (via [girafe()]) and
#' saved static images. Palette settings are inherited from global settings set via
#' [global_settings_set()] for the "girafe" function.
#'
#' If `palette_codes` is provided (either directly or via global settings), the
#' function applies the same palette transformation that [girafe()] uses for
#' interactive plots.
#'
#' @export
#' @returns No return value, called for side effects (saves plot to file)
#'
#' @seealso
#' - [girafe()] for interactive plots with palette support
#' - [global_settings_set()] for setting default palettes
#' - [ggplot2::ggsave()] for the underlying save function
#'
#' @examples
#' library(ggplot2)
#' my_plot <- ggplot(data=mtcars, aes(x=hp, y=mpg, fill=factor(cyl))) + geom_point()
#'
#' \dontrun{
#' # Save with default settings
#' ggsaver(my_plot, tempfile(fileext = ".png"))
#'
#' # Set global palette and save
#' global_settings_set(
#'   fn_name = "girafe",
#'   new = list(palette_codes = list(c("red", "blue", "green")))
#' )
#' ggsaver(my_plot, tempfile(fileext = ".png"))
#'
#' # Override global palette for specific save
#' ggsaver(
#'   my_plot,
#'   tempfile(fileext = ".png"),
#'   palette_codes = list(c("purple", "orange", "yellow"))
#' )
#' }
ggsaver <- function(
  plot,
  filename,
  palette_codes = NULL,
  priority_palette_codes = NULL,
  label_wrap_width = 80,
  ncol = NULL,
  byrow = TRUE,
  ...
) {
  # Validate palette parameters first (before check_options processes them)
  validate_palette_params(
    palette_codes = palette_codes,
    priority_palette_codes = priority_palette_codes,
    label_wrap_width = label_wrap_width,
    ncol = ncol,
    byrow = byrow
  )

  # Check global settings from girafe, then provided settings, and finally defaults
  args <- check_options(
    call = match.call(),
    ignore_args = c(.saros.env$ignore_args, "plot", "filename"),
    defaults_env = global_settings_get(fn_name = "girafe"),
    default_values = formals(ggsaver)
  )

  # Apply palette if specified and plot has a fill aesthetic
  if (!is.null(args$palette_codes) && ggplot2::is_ggplot(plot)) {
    fill_var <- rlang::as_label(plot$mapping$fill)

    if (!is.null(fill_var) && fill_var != "NULL") {
      # Check if fill_var exists as a column in the data
      if (fill_var %in% colnames(plot$data)) {
        fill_levels <-
          if (is.factor(plot$data[[fill_var]])) {
            levels(plot$data[[fill_var]])
          } else {
            unique(plot$data[[fill_var]])
          }

        plot <- suppressMessages(
          plot +
            scale_discrete_special(
              palette_codes = args$palette_codes,
              lvls = fill_levels,
              ncol = args$ncol,
              byrow = args$byrow,
              label_wrap_width = args$label_wrap_width,
              priority_palette_codes = args$priority_palette_codes
            )
        )
      }
    }
  }

  suppressMessages(ggplot2::ggsave(
    filename = filename,
    plot = plot,
    dpi = "retina",
    create.dir = TRUE,
    ...
  ))
}
