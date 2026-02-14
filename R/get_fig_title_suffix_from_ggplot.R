#' Generate Figure Title Suffix with N Range and Optional Download Links
#'
#' @description
#' Creates a formatted suffix for figure titles that includes the sample size (N) range
#' from a ggplot object. Optionally generates markdown download links for both the
#' plot data and the plot image.
#'
#' @param plot A `ggplot2` object, typically created by [makeme()].
#' @param save Logical flag. If `TRUE`, generates download links for the plot data (CSV)
#'   and plot image (PNG). If `FALSE` (default), only returns the N range text.
#' @param n_equals_string String. Prefix text for the sample size display
#'   (default: `"N = "`).
#' @param folder String. Folder path where files should be saved. If `NULL`, uses global
#'   settings or defaults to `"."` (current directory).
#' @param file_prefix String. Prefix for saved filenames. If `NULL`, uses global
#'   settings or defaults to `""` (no prefix).
#' @param file_suffixes Character vector. File extensions for the saved plot images
#'   (default: `c(".csv", ".png")`). Should include the dot.
#'   (default: `c(".csv", ".png")`). Should include the dot.
#' @param link_prefixes Character vector. Markdown link text prefixes for the plot download links
#'   (default: `c("[CSV](", "[PNG](")`).
#'   (default: `c("[CSV](", "[PNG](")`).
#' @param save_fns List of functions. Functions to save the plot data and images.
#' @param sep String. Separator between N range text and download links
#'   (default: `", "`).
#'
#' @return An `AsIs` object (using [I()]) containing a character string with:
#'   - Sample size range formatted as "\{n_equals_string\}\{range\}"
#'   - If `save = TRUE`: additional download links for plot data and image, separated by `sep`
#'   - Empty string if `plot` is not a valid ggplot object or has no data
#'
#' @details
#' This function is particularly useful for adding informative captions to plots in
#' reports. The N range is calculated using [n_range2()], which extracts the sample
#' size from the plot data. When `save = TRUE`, the function creates downloadable
#' files using [make_link()]:
#' - Plot data as CSV (via `utils::write.csv`)
#' - Plot image as PNG (via [ggsaver()])
#'
#' The function returns an `AsIs` object to prevent automatic character escaping
#' in markdown/HTML contexts.
#'
#' @seealso
#' - [n_range2()] for extracting N range from ggplot objects
#' - [make_link()] for creating download links
#' - [ggsaver()] for saving ggplot objects
#'
#' @export
#'
#' @examples
#' # Create a sample plot
#' plot <- makeme(data = ex_survey, dep = b_1:b_3)
#'
#' # Get just the N range text
#' get_fig_title_suffix_from_ggplot(plot)
#'
#' # Custom N prefix
#' get_fig_title_suffix_from_ggplot(plot, n_equals_string = "Sample size: ")
#'
#' \dontrun{
#' # Generate with download links (saves files to disk)
#' get_fig_title_suffix_from_ggplot(plot, save = TRUE)
#'
#' # Custom separator and link prefix
#' get_fig_title_suffix_from_ggplot(
#'   plot,
#'   save = TRUE,
#'   sep = " | ",
#'   link_prefix = "[Download PNG]("
#' )
#' }
get_fig_title_suffix_from_ggplot <- function(
  plot,
  save = FALSE,
  n_equals_string = "N = ",
  folder = NULL,
  file_prefix = NULL,
  file_suffixes = c(".csv", ".png"),
  link_prefixes = c("[CSV](", "[PNG]("),
  save_fns = NULL,
  sep = ", "
) {
  # Check global options and merge with arguments
  args <- check_options(
    call = match.call(),
    ignore_args = c(.saros.env$ignore_args, "plot", "save_fns"),
    defaults_env = global_settings_get(
      fn_name = "get_fig_title_suffix_from_ggplot"
    ),
    default_values = formals(get_fig_title_suffix_from_ggplot)
  )

  # Handle save_fns manually to preserve list structure
  if (!is.null(save_fns)) {
    # Use explicitly provided save_fns
    args$save_fns <- save_fns
  } else {
    # Try to get from global settings
    global_save_fns <- global_settings_get("get_fig_title_suffix_from_ggplot")[["save_fns"]]
    if (!is.null(global_save_fns)) {
      args$save_fns <- global_save_fns
    } else {
      # Use defaults
      args$save_fns <- list(utils::write.csv, ggsaver)
    }
  }

  # Validate plot and data
  if (!ggplot2::is_ggplot(plot)) {
    return(I(""))
  }
  # Check for NULL, waiver, or zero-row data
  if (is.null(plot$data) || !is.data.frame(plot$data) || nrow(plot$data) == 0) {
    return(I(""))
  }

  if (length(args$save_fns) == 1 && rlang::is_function(args$save_fns)) {
    args$save_fns <- list(args$save_fns)
  }

  # Validate that vectorized parameters have matching lengths
  if (isTRUE(args$save)) {
    len_suffixes <- length(args$file_suffixes)
    len_prefixes <- length(args$link_prefixes)
    len_fns <- length(args$save_fns)

    if (len_suffixes != len_prefixes || len_suffixes != len_fns) {
      cli::cli_abort(c(
        "Vectorized parameters must have equal lengths.",
        "x" = "file_suffixes has length {len_suffixes}",
        "x" = "link_prefixes has length {len_prefixes}",
        "x" = "save_fns has length {len_fns}",
        "i" = "All three must have the same length."
      ))
    }
  }

  x <- stringi::stri_c(args$n_equals_string, n_range2(plot))
  if (isTRUE(args$save)) {
    links <- lapply(seq_along(args$file_suffixes), function(i) {
      if (
        args$file_suffixes[i] %in% c(".png", ".jpg", ".jpeg", ".pdf", ".svg")
      ) {
        dat <- plot
      } else {
        dat <- plot$data
      }
      make_link(
        data = dat,
        folder = args$folder,
        file_prefix = args$file_prefix,
        file_suffix = args$file_suffixes[i],
        link_prefix = args$link_prefixes[i],
        save_fn = args$save_fns[[i]]
      )
    }) |>
      unlist()
    I(paste0(c(x, links), collapse = args$sep))
  } else {
    I(x)
  }
}
