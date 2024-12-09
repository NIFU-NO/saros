keep_subitem <- function(fct, label_separator = NULL,
                         call = rlang::caller_env()) {
  lvls <- unique(as.character(fct)) # The items (including main question)
  lbls <-
    if (!is.null(label_separator)) {
      stringi::stri_replace(
        str = lvls,
        regex = stringi::stri_c(ignore_null = TRUE, "^(.*)", label_separator, "(.*)$"), # Assumes that the main question always comes first, and subitem always last
        replacement = "$2",
        dot_all = TRUE
      )
    } else {
      lvls
    }

  factor(
    x = fct,
    levels = lvls,
    labels = lbls,
    ordered = TRUE
  )
}
