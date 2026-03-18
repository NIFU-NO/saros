#' Detect the Current Output Format
#'
#' @description
#' Returns the output format of the current rendering context. When called
#' inside a Quarto/knitr document, delegates to [knitr::pandoc_to()].
#' When called outside of Quarto (e.g. in an officer-based script), returns
#' `"officer"`.
#'
#' @return A character string: `"html"`, `"docx"`, `"typst"`, `"officer"`,
#'   or another format reported by [knitr::pandoc_to()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' output_format()
#' }
output_format <- function() {
  fmt <- knitr::pandoc_to()
  if (is.null(fmt)) "officer" else fmt
}

#' Check if current output format does not support page breaks
#'
#' Returns `TRUE` for formats where `\\newpage` page breaks should be
#' suppressed: HTML-based formats, Typst (where page breaks inside
#' containers cause errors), and officer contexts.
#'
#' Uses [knitr::is_html_output()] when knitr is in progress (covers
#' revealjs, slidy, and other HTML-based Pandoc formats). Also returns
#' `TRUE` for Typst, since Typst does not support page breaks inside
#' containers and the Quarto `pagebreak` shortcode lacks native Typst
#' support. Outside of knitr (officer context), returns `TRUE` so that
#' page breaks are not inserted by default.
#'
#' @return Logical scalar.
#' @keywords internal
is_html_output_or_officer <- function() {
  if (isTRUE(getOption("knitr.in.progress"))) {
    if (identical(knitr::pandoc_to(), "typst")) return(TRUE)
    knitr::is_html_output()
  } else {
    TRUE
  }
}
