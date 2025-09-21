#' Is x A String?
#'
#' Returns TRUE if object is a character of length 1.
#'
#' @param x Object
#'
#' @return Logical value.
#' @keywords internal
is_string <- function(x) {
  is.character(x) && length(x) == 1
}

#' Helper function to extract raw variable labels from the data
#'
#' @param data Dataset
#' @param col_pos Optional, character vector of column names or integer vector of positions
#' @param return_as_list Flag, whether to return as list or character vector
#'
#' @return List or character vector
#' @keywords internal
get_raw_labels <-
  function(data, col_pos = NULL, return_as_list = FALSE) {
    if (is.null(col_pos)) {
      col_pos <- colnames(data)
    }
    out <- lapply(
      X = stats::setNames(col_pos, nm = col_pos),
      FUN = function(.x) {
        y <- attr(data[[.x]], "label")
        if (is_string(y)) y else NA_character_
      }
    )
    if (isFALSE(return_as_list)) {
      out <- unlist(out)
    }
    out
  }
