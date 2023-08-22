#' Adds caption attribute
#'
#' @param main_question String from get_main_question2
#' @param data_out Output from summarize_data
#' @param indep_pos Named integer for the by-variable.
#' @param translations List of named strings for by and (N=, etc. see getOption("saros")$translations.
#' @param mesos_group String, indicating the name of the mesos group
#' @param filepath String, xlsx-file
#'
#' @return String
#' @export
create_caption <- function(main_question,
                           data_out,
                           indep_pos = NULL,
                           mesos_group = NULL,
                           filepath = NULL,
                           translations = NULL) {

  if(rlang::is_null(translations)) translations <- eval(formals(draft_report)$translations)

  N <-
    data_out %>%
    dplyr::group_by(.data$.variable_label) %>%
    dplyr::summarize(N = sum(.data$.count, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(Max = max(N), Min = min(N))

  N <- if(N$Max == N$Min) glue::glue_data(N, "{Min}") else glue::glue_data(N, "[{Min}-{Max}]")
  n_equal_prefix <- translations$n_equal_prefix
  n_equal_suffix <- translations$n_equal_suffix
  by_breakdown <- if(length(indep_pos)>0) translations$by_breakdown
  by_text <- if(length(indep_pos)>0) create_text_collapse(indep_pos, last_sep = translations$last_sep)
  if(rlang::is_character(mesos_group)) {
    mesos_group <- create_text_collapse(mesos_group, last_sep = translations$last_sep)
    mesos <- stringi::stri_c(translations$mesos_group_prefix,
                               mesos_group,
                               translations$mesos_group_suffix,
                               ignore_null = TRUE)
  } else mesos <- NULL

  contents <- c("_", main_question, "_ ", mesos,
                by_breakdown, by_text,
                n_equal_prefix, N, n_equal_suffix)
  contents <- contents[!is.na(contents)]

  caption <- stringi::stri_c(contents, ignore_null=TRUE, collapse = "")
  caption <- stringi::stri_replace_all(str = caption, regex = "\n", replacement = " ")
  # if(rlang::is_string(filepath)) {
  #   filepath_xlsx <- stringi::stri_replace_all_regex(str = filepath,
  #                                                    pattern = "(.*)\\.[[:alnum:]]+$",
  #                                                    replacement = "$1.xlsx")
  #   caption <- stringi::stri_c(ignore_null=TRUE, caption, " [xlsx](", filepath_xlsx, ")")
  # }

  caption
}
