#' Interactive table of text data (DEPRECATED)
#'
#' This function has been deprecated.
#' Use instead [saros.contents::makeme()]
#'
#' @inheritParams embed_cat_prop_plot
#' @importFrom rlang !!!
#' @export
#'
embed_chr_table_html <-
  function(
    data,
    dep,
    ...,
    mesos_group = NULL) {


    dots <- rlang::list2(...)

    lifecycle::deprecate_soft(
      when = "1.1.0",
      what = "embed_chr_table_html()",
      with = "saros.contents::makeme(type = 'chr_table_html')"
    )

    saros.contents::makeme(
      data = data,
      type = "chr_table_html",
      !!!dots)
}
