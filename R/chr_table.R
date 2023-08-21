embed_chr_table_html <- function(
    data,
    cols,
    ...,
    summarized_data = NULL,
    tailored_group = NULL,
    label_separator = NULL,
    translations = .saros.env$defaults$translations,
    call = rlang::caller_env()) {

  dots <- rlang::list2(...)
  cols_enq <- rlang::enquo(arg = cols)
  cols_pos <- tidyselect::eval_select(cols_enq, data = data, error_call = call)

  if(length(cols_pos)>1) cli::cli_abort("Too many chr-cols in one section. Refine your data_overview so that only one txt-col is submitted at once. Problem with {.arg {cols}}")
  out <- dplyr::distinct(data, dplyr::pick({{cols}}))
  names(out) <- " "

  tailored <- if(rlang::is_string(tailored_group)) stringr::str_c(translations$tailored_group_prefix, tailored_group, translations$tailored_group_suffix)
  attr(out, "saros_caption") <-
    get_raw_labels(data = data, cols_pos = cols_pos) %>%
    stringr::str_c(., tailored, translations$n_equal_prefix, sum(!is.na(data[[unname(cols_pos)]])), translations$n_equal_suffix)
  out

}
