filename_sanitizer <- function(x, max_clean_folder_name = 8) {
  out <-
    stringi::stri_replace_all_regex(x,
                                    pattern = "[[:punct:][:space:]]",
                                    replacement = "_")
  out <- iconv(out, from ="UTF-8", to="windows-1250")
  out <- stringi::stri_sub(out, from = 1, to = max_clean_folder_name)

  # out <- make.names(out)
  out
}
