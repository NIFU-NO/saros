string_wrap <- function(str, width) {
  unlist(
    lapply(
      stringi::stri_wrap(str = str, width = width, simplify = F),
      FUN = function(wrapped_chr) paste0(wrapped_chr, collapse="\n")))
}
