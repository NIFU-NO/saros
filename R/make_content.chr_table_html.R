#' @export
make_content.chr_table_html <-
  function(...) {

    dots <- rlang::list2(...)
    data <- dots$data

  if(length(dots$dep)>1) cli::cli_abort(c(x="{.fun make_content.chr_table_html()} currently only accepts one variable.",
                                          i="Problems with: {.var {dots$dep}}."))
  out <- dplyr::distinct(data, dplyr::pick(tidyselect::all_of(dots$dep)))
  out <- dplyr::filter(out, !is.na(.data[[dots$dep]]), .data[[dots$dep]] != "")
  names(out) <- " "

  out

}
