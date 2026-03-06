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
