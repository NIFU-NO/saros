#' Title
#'
#' @param x Crosstable
#' @param label_separator [\code{character(1)}]\cr If not NULL (default), will split labels into main- and sub-questions and create figure caption.
#' @param docx_template  [\code{character(1) || officer::read_docx()}]\cr
#' @param caption_style [\code{character(1)}]\cr Word template style to be used for formatting chart. Defaults to "Normal".
#' @param caption_autonum Object obtained from \link[officer]{run_autonum}.
#' @param body_style [\code{character(1)}]\cr Word style for table body cells.
#' @param table_header_style [\code{character(1)}]\cr Word style for table header cells.
#' @param footer_style [\code{character(1)}]\cr Word style for table footer.
#' @param return_docx [\code{logical(1)}]\cr If FALSE, will return the table
#' rather than a rdocx object with the table inside it. Set to TRUE (default)
#' if piping multiple tables and charts together. Set to FALSE if you want to
#' continue modify the table.
#' @param topcaption [\code{logical(1)}]\cr Place caption above (TRUE, default) or below table.
#'
#' @importFrom flextable border_inner_h border_remove hline_bottom font fontsize add_footer_lines set_caption body_add_flextable
#' @importFrom officer fp_border fp_par body_add_fpar
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @importFrom rlang %||%
#' @importFrom crosstable as_flextable
#' @return rdocx object, which can be saved with print() after loading the officer-package
#' @export
#'
#' @examples
#' library(officer)
#' embed_table_cat_docx(x=crosstable::crosstable(ex_survey1,
#' cols = b_1:b_3, percent_pattern = "{p_col}"),
#'  label_separator=" - ")
embed_table_cat_docx <- function(x,
                                 label_separator = NULL,
                                 docx_template = NULL,
                                 caption_style = NULL,
                                 body_style = NULL,
                                 table_header_style = NULL,
                                 footer_style = NULL,
                                 caption_autonum = NULL,
                                 topcaption = TRUE,
                                 return_docx = TRUE) {

  docx_file <- use_docx(docx_template = docx_template)
  caption_style <- caption_style %||% "Normal"
  body_style <- body_style %||% "Normal"
  table_header_style <- table_header_style %||% "Normal"
  footer_style <- footer_style %||% "Normal"


  if(!is.null(label_separator)) {
    main_question <- get_main_question2(x$label, label_separator = label_separator)


    sep_pat <- paste0("^(.*)", label_separator, "(.*)$")
    x <- dplyr::mutate(.data = x,
                       label = stringr::str_replace(string = .data$label,
                                                    pattern = .env$sep_pat,
                                                    replacement = "\\2"))
  }

  y <- crosstable::as_flextable(x)
  if(!is.null(label_separator)) {
    y <- flextable::set_caption(x = y,
                                align_with_table = TRUE,
                                caption = main_question,
                                autonum = caption_autonum,
                                word_stylename = caption_style)
  }
  y <- flextable::style(x = y, pr_p = officer::fp_par(word_style = body_style), part = "body")
  y <- flextable::style(x = y, pr_p = officer::fp_par(word_style = table_header_style), part = "header")
  y <- flextable::border_remove(y)
  y <- flextable::hline(x = y, border = officer::fp_border(), part = "all")
  y <- flextable::hline_top(x = y, border = officer::fp_border(), part = "all")
  y <- flextable::hline_bottom(x = y, border = officer::fp_border(), part = "body")

  docx_file <-
    flextable::body_add_flextable(x = docx_file, value = y,
                                  align = "left",
                                  split = FALSE,
                                  topcaption = topcaption)

  docx_file <-
    officer::body_add_par(x = docx_file,
                          value = paste0("Note. N="), style = footer_style)

  if(return_docx) docx_file else y
}

