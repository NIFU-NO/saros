#' Return character vector of manually picked data columns.
#'
#' Only works in an interactive session. Copies to the clipboard if to_clipboard = TRUE, and if clipboard is available on system.
#'
#' @param data A dataset; data frame.
#' @param to_clipboard Boolean. Defaults to FALSE. If TRUE, overwrites what you already have copied. Use with caution.
#' @param bare Flag, defaults to FALSE. If TRUE, returns a tidyselect-style bare list of columns, without quotation marks.
#'
#' @return Character vector. Prints to console.
#' @export
#'
#' @examples
#' if(interactive()) handpick(mtcars)

handpick <- function(data, to_clipboard = FALSE, bare=FALSE) {
  if(interactive()) {
  x <-
    utils::select.list(colnames(data),
                   multiple=TRUE,
                   title='Pick columns in dataset',
                   graphics=TRUE)
  y <- dput(x)
  if(bare) y <- gsub("\"|'", "", y)
  if(to_clipboard && clipr::clipr_available()) {
    clipr::write_clip(content = as.character(deparse(x)), object_type = "character")
    }
  x
  }
}
