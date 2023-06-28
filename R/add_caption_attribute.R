#' Adds caption attribute
#'
#' @param main_question String from get_main_question2
#' @param data_out Output from summarize_data
#' @param by_pos Named integer for the by-variable.
#' @param translations List of named strings for by and (N=, etc. see getOption("saros")$translations.
#' @param tailored_group String, indicating the name of the tailored group
#'
#' @return String
add_caption_attribute <- function(main_question,
                                  data_out,
                                  by_pos,
                                  tailored_group = NULL,
                                  translations = .saros.env$defaults$translations) {


  N <- data_out %>%
    dplyr::group_by(.data$.variable_label) %>%
    dplyr::summarize(N = sum(.data$.count, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(Max = max(N), Min = min(N))
  N <- if(N$Max == N$Min) glue::glue_data(N, "{Min}") else glue::glue_data(N, "[{Min}-{Max}]")
  n_equal_prefix <- translations$n_equal_prefix
  n_equal_suffix <- translations$n_equal_suffix
  by_breakdown <- if(length(by_pos>0)) translations$by_breakdown
  by_text <- if(length(by_pos>0)) create_text_collapse(by_pos, last_sep = translations$last_sep)
  tailored <- if(rlang::is_string(tailored_group)) stringr::str_c(translations$tailored_group_prefix, tailored_group, translations$tailored_group_suffix)
  contents <- c("_", main_question, "_ ", tailored, by_breakdown, by_text, n_equal_prefix, N, n_equal_suffix)
  contents <- contents[!is.na(contents)]
  caption <- stringr::str_c(contents, collapse="")
  caption <- stringi::stri_replace_all(str = caption, regex = "\n", replacement = " ")
  caption
}
