add_caption_attribute <- function(main_question, data_out, by_pos,
                                  translations = getOption("saros")$translations) {

  N <- data_out %>%
    dplyr::group_by(.data$.variable_label) %>%
    dplyr::summarize(N = sum(.data$.count, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(Max=max(N), Min = min(N)) %>%
    glue::glue_data("[{Min}-{Max}]")
  n_equal_prefix <- translations$n_equal_prefix
  n_equal_suffix <- translations$n_equal_suffix
  by_breakdown <- if(length(by_pos>0)) translations$by_breakdown
  by_text <- create_text_collapse(names(by_pos))
  caption <- stringr::str_c("_", main_question, "_ ", by_breakdown, by_text, n_equal_prefix, N, n_equal_suffix)

  caption
}
