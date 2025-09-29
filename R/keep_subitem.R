keep_subitem <- function(
  fct,
  label_separator = NULL,
  ordered = TRUE,
  call = rlang::caller_env()
) {
  # Convert character to factor if needed
  if (is.character(fct)) {
    fct <- factor(fct)
  }
  
  # Use factor levels, not unique values, to handle NA correctly
  fct_levels <- levels(fct)
  
  lbls <-
    if (!is.null(label_separator)) {
      stringi::stri_replace(
        str = fct_levels,
        regex = stringi::stri_c(
          ignore_null = TRUE,
          "^(.*)",
          label_separator,
          "(.*)$"
        ), # Assumes that the main question always comes first, and subitem always last
        replacement = "$2",
        dot_all = TRUE
      )
    } else {
      fct_levels
    }

  factor(
    x = fct,
    levels = fct_levels,
    labels = lbls,
    ordered = ordered
  )
}
