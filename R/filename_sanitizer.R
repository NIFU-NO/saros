#' File/folder name sanitizer replacing space and punctuation with underscore
#'
#' @param x Character vector of file/folder names
#' @param max_chars Maximum character length
#' @param accept_hyphen Flag, whether a hyphen - is acceptable.
#'
#' @return Character vector of same length as x
#' @export
#'
#' @examples
#' filename_sanitizer(c("Too long a name", "with invalid *^/&#"))
filename_sanitizer <- function(x, max_chars = NA_integer_, accept_hyphen = FALSE) {
  pattern <- if(isTRUE(accept_hyphen)) "[^[:alnum:]-+]+" else "[^[:alnum:]+]+"
  out <-
    stringi::stri_replace_all_regex(x,
                                    pattern = pattern,
                                    replacement = "_")
  out <- iconv(out, from ="UTF-8", to="ASCII//TRANSLIT", sub='')

  if(!is.na(max_chars)) out <- stringi::stri_sub(out, from = 1, to = max_chars)

  # out <- make.names(out)
  out
}
