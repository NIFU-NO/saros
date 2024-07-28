#' Embed Interactive Categorical Plot (DEPRECATED!)
#'
#' This function has been deprecated.
#' Use instead [saros.contents::makeme()]
#'
#' @param data `data.frame`, `tibble` or potentially a `srvyr`-object.
#' @param dep `tidyselect`-syntax for dependent variable(s).
#' @param indep `tidyselect`-syntax for an optional independent variable.
#' @param colour_palette Character vector. Avoid using this.
#' @param mesos_group String
#' @param html_interactive Flag, whether to include interactivity.
#' @param inverse Flag, whether to flip plot or table.
#' @param ... Dynamic dots, arguments forwarded to underlying function(s).
#'
#' @importFrom rlang !!!
#' @export
#'
embed_cat_prop_plot <-
  function(data,
           ...,
           dep = tidyselect::everything(),
           indep = NULL,
           colour_palette = NULL,
           mesos_group = NULL,
           html_interactive = TRUE,
           inverse = FALSE) {

    dots <- rlang::list2(...)

    lifecycle::deprecate_soft(
      when = "1.1.0",
      what = "embed_cat_prop_plot()",
      with = "saros.contents::makeme(type = 'cat_prop_plot_html')"
    )

    saros.contents::makeme(
      data = data,
      type = "cat_prop_plot_html",
      !!!dots)

  }
