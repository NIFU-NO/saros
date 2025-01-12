convert_trailing_chars <- function(x, from_char = " ", to_char = "-") {
    match <- stringi::stri_extract_last_regex(x, paste0(from_char, "+$"))

    match[is.na(match)] <- 0

    if (!all(is.na(match))) {
        replacement <- stringi::stri_dup(str = to_char, times = stringi::stri_length(match))
        return(stringi::stri_replace_last_regex(str = x, pattern = paste0(from_char, "+$"), replacement = replacement))
    }

    x
}
