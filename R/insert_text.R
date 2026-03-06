#' Insert Text from a Data Frame by Chunk Name
#'
#' @description
#' Looks up a text string from a data frame based on a chunk name and
#' position (before/after). Optionally expands knitr templating syntax
#' (`{{...}}`) found in the text.
#'
#' @param data A data frame with columns `chunk`, `before`, and `text`.
#' @param chunk Character. The chunk name to look up in `data`.
#'   If the file extension is `"rmarkdown"`, it is stripped automatically.
#' @param before Logical. Whether to retrieve the text marked as "before"
#'   (`TRUE`, default) or "after" (`FALSE`) the chunk.
#' @param error_on_empty Controls behaviour when no matching text is found:
#'   - `TRUE`: throws an error via [cli::cli_abort()].
#'   - `FALSE`: issues a warning via [cli::cli_warn()].
#'   - `NULL` (default): silently returns an empty `AsIs` object.
#' @param enabled Logical. If `FALSE`, the function returns `I(character(0))`
#'   immediately without any lookup. Can be set globally via
#'   `global_settings_set(new = list(enabled = FALSE), fn_name = "insert_text")`.
#'
#' @return An [I()]-wrapped character string. If no match is found and
#'   `error_on_empty` is `NULL`, an empty `AsIs` character vector.
#'
#' @details
#' The function filters `data` for rows matching the given `chunk` and
#' `before` values. If exactly one row matches, its `text` column is
#' returned. If the text contains knitr templating delimiters (`{{`),
#' it is expanded with [knitr::knit_expand()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' texts <- data.frame(
#'   chunk = c("intro", "intro"),
#'   before = c(TRUE, FALSE),
#'   text = c("Before the chunk.", "After the chunk.")
#' )
#' insert_text(texts, "intro", before = TRUE)
#' insert_text(texts, "intro", before = FALSE)
#' }
insert_text <- function(data, chunk, before = TRUE, error_on_empty = NULL, enabled = TRUE) {
  args <-
    check_options(
      call = match.call(),
      defaults_env = global_settings_get(fn_name = "insert_text"),
      default_values = formals(insert_text)
    )
  if (isFALSE(args$enabled)) return(I(character(0)))
  if (fs::path_ext(chunk) == "rmarkdown") {
    chunk <- fs::path_ext_remove(chunk)
  }
  out <- data |>
    dplyr::filter(.data$chunk == .env$chunk & .data$before == .env$args$before) |>
    dplyr::pull(.data$text)
  if (length(out) > 1) {
    cli::cli_abort(
      "Duplicate chunk-texts for {chunk} and before={args$before}: {cli::ansi_collapse(out)}."
    )
  }
  nothing_msg <- "No text found for chunk {chunk} and before={args$before}."
  if (length(out) == 0 && isTRUE(args$error_on_empty)) {
    cli::cli_abort(nothing_msg)
  }
  if (length(out) == 0 && isFALSE(args$error_on_empty)) {
    cli::cli_warn(nothing_msg)
  }
  if (
    length(out) > 0 &&
      stringi::stri_detect_fixed(out, pattern = "{{", max_count = 1)
  ) {
    out <- knitr::knit_expand(text = out)
  }
  I(out)
}
