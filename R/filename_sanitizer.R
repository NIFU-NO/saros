filename_sanitizer <- function(x) {
  out <-
    stringi::stri_replace_all_regex(x,
                                    pattern = "[[:punct:]]",
                                    replacement = "_")
  out <- iconv(out, from ="UTF-8", to="windows-1250")
  # out <- make.names(out)
  out
}
