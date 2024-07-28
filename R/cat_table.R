
#' Embed Reactable Table (DEPRECATED!)
#'
#' This function has been deprecated.
#' Use instead [saros.contents::makeme()]
#'
#' @inheritParams embed_cat_prop_plot
#' @importFrom rlang !!!
#'
#' @export
embed_cat_table <-
  function(data,
           ...,
           dep = tidyselect::everything(),
           indep = NULL,
           mesos_group = NULL) {

    dots <- rlang::list2(...)


    lifecycle::deprecate_soft(
      when = "1.1.0",
      what = "embed_cat_prop_plot()",
      with = "saros.contents::makeme(type='cat_table_html')"
    )

    saros.contents::makeme(
      data = data,
      type = "cat_table_html",
      !!!dots)
  }

