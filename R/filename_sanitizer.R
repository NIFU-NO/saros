#' File/folder name sanitizer replacing space and punctuation with underscore
#'
#' @param x Character vector of file/folder names
#' @param max_chars Maximum character length
#'
#' @return Character vector of same length as x
#' @export
#'
#' @examples filename_sanitizer(c("Too long a name", "with invalid *^/&#"))
filename_sanitizer <- function(x, max_chars = NA_integer_) {
  out <-
    stringi::stri_replace_all_regex(x,
                                    pattern = "[^[:alnum:]-+]",
                                    replacement = "_")
  out <- iconv(out, from ="UTF-8", to="ASCII//TRANSLIT", sub='')

  if(!is.na(max_chars)) out <- stringi::stri_sub(out, from = 1, to = max_chars)

  # out <- make.names(out)
  out
}
