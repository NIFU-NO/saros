#' Return character vector of manually picked data columns.
#'
#' Only works if in interactive session. Copies to the clipboard if to_clipboard = TRUE, and if clipboard is available on system.
#'
#' @param data A dataset; tibble or data frame.
#' @param to_clipboard Boolean. Defaults to FALSE. If TRUE, overwrites what you already have copied. Use with caution.
#'
#' @return Character vector. Prints to console.
#' @importFrom utils select.list
#' @importFrom clipr write_clip
#' @export
#'
#' @examples \dontrun{if(interactive()) handpick(mtcars)}
handpick <- function(data, to_clipboard = FALSE) {
  if(interactive()) {
  x <-
    select.list(colnames(data),
                   multiple=TRUE,
                   title='Pick columns in dataset',
                   graphics=TRUE)
  y <- dput(x)
  if(to_clipboard && clipr::clipr_available()) {
    clipr::write_clip(content = as.character(deparse(x)), object_type = "character")
    }
  x
  }
}
