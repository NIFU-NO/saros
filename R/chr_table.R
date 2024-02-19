#' Interactive table of text data
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @inheritParams gen_qmd_chapters
#' @param dep *Variable selections*
#'
#'  <`tidyselect`> // *Default:* `NULL`, meaning everything for dep, nothing for by.
#'
#'  Columns in `data`. Currently allows tidyselect-syntax, which will be removed.
#'
#' @return Data frame
#'
embed_chr_table_html <-
  function(
    data,
    dep = colnames(data),
    ...,
    mesos_group = NULL,
    call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "chr_table")

  dep_enq <- rlang::enquo(arg = dep)
  dep_pos <- tidyselect::eval_select(dep_enq, data = data, error_call = call)

  if(length(dep_pos)>1) cli::cli_abort("Too many chr-dep in one section. Refine your chapter_overview so that only one txt-col is submitted at once. Problem with {.arg {dep}}")
  out <- dplyr::distinct(data, dplyr::pick({{dep}}))
  names(out) <- " "

  mesos <- if(is_string(mesos_group)) stringi::stri_c(ignore_null=TRUE, dots$translations$mesos_group_prefix,
                                                                  mesos_group,
                                                            dots$translations$mesos_group_suffix)
  attr(out, "saros_caption") <-
    get_raw_labels(data = data, col_pos = dep_pos) %>%
    stringi::stri_c(ignore_null=TRUE, ., mesos,
                   dots$translations$n_equal_prefix,
                   sum(!is.na(data[[unname(dep_pos)]])),
                   dots$translations$n_equal_suffix)
  out

}
