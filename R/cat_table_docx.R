#' Title
#'
#' @inheritParams summarize_data
#' @inheritParams prep_cat_table_docx
#' @param body_style [\code{character(1)}]\cr Word style for table body cells.
#' @param table_header_style [\code{character(1)}]\cr Word style for table header cells.
#' @param footer_style [\code{character(1)}]\cr Word style for table footer.
#' rather than a rdocx object with the table inside it. Set to TRUE (default)
#' if piping multiple tables and charts together. Set to FALSE if you want to
#' continue modify the table.
#'
#' @return rdocx object, which can be saved with print() after loading the officer-package
#'
prep_cat_table_docx <-
  function(data,
           body_style = NULL,
           table_header_style = NULL,
           footer_style = NULL) {
    data %>%
      crosstable::as_flextable() %>%
      flextable::style(pr_p = officer::fp_par(word_style = body_style), part = "body") %>%
      flextable::style(pr_p = officer::fp_par(word_style = table_header_style), part = "header") %>%
      flextable::border_remove() %>%
      flextable::hline(border = officer::fp_border(), part = "all") %>%
      flextable::hline_top(border = officer::fp_border(), part = "all") %>%
      flextable::hline_bottom(border = officer::fp_border(), part = "body")
  }


#' Embed Table into Docx
#'
#' @inheritParams summarize_data
#' @inheritParams prep_cat_prop_plot_docx
#' @inheritParams embed_cat_prop_plot_docx
#' @inheritParams prep_cat_table_docx
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
#' embed_cat_table_docx(data = ex_survey1,
#'                      cols = b_1:b_3,
#'                      label_separator = " - ")
embed_cat_table_docx <-
  function(data,
           ...,
           cols = tidyselect::everything(),
           by = NULL,
           showNA = c("ifany", "always", "never"),
           data_label = c("proportion", "percentage", "percentage_bare", "count", "mean", "median"),
           digits = if(data_label == "proportion") 2 else if(data_label == "count") 0 else 1,
           sort_by = NULL,
           vertical = FALSE,
           descend = FALSE,
           ignore_if_below = 0,
           label_separator = NULL,
           docx_template = NULL,
           body_style = NULL,
           table_header_style = NULL,
           footer_style = NULL,
           caption_autonum = NULL,
           topcaption = TRUE,
           return_raw = TRUE) {


    dots <- rlang::list2(...)
    showNA <- rlang::arg_match(showNA)
    data_label <- rlang::arg_match(data_label, error_call = call)
    check_data_frame(data)
    check_multiple_by(data, by = {{by}})
    check_string(label_separator, null.ok=TRUE)
    check_bool(return_raw)
    check_autonum(caption_autonum)

    body_style <- body_style %||% "Normal"
    table_header_style <- table_header_style %||% "Normal"
    footer_style <- footer_style %||% "Normal"


    cols_enq <- rlang::enquo(arg = cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data)
    by_enq <- rlang::enquo(arg = by)
    by_pos <- tidyselect::eval_select(by_enq, data = data)

    check_category_pairs(data = data, cols_pos = c(cols_pos))


    data_out <-
      rlang::exec(
        summarize_data,
        data = data,
        cols = cols_pos,
        by = by_pos,
        data_label = data_label,
        showNA = showNA,
        digits = digits,
        sort_by = sort_by,
        descend = descend,
        ignore_if_below = ignore_if_below,
        label_separator = label_separator,
        call = call,
        !!!dots)
    total_count <- sum(data_out$.count)


    table <-  # Turn into separate function.
      prep_cat_table_docx(
        data = data_out,
        body_style = body_style,
        table_header_style = table_header_style,
        footer_style = footer_style)

    if(return_raw) {
      table
    } else {


      ## Consider moving all the below into table_categorical_docx
      ## so that embed_chart becomes one function
      docx_file <- use_docx(docx_template = docx_template)


      # if(!is.null(label_separator)) {
      #   docx_file <-
      #     get_block_caption(
      #       data = data,
      #       cols_pos = cols_pos,
      #       docx_file = docx_file,
      #       label_separator = label_separator,
      #       caption_style = caption_style,
      #       caption_autonum = caption_autonum)
      # }

      docx_file <-
        flextable::body_add_flextable(x = docx_file, value = table,
                                      align = "left",
                                      split = FALSE)

      docx_file <-
        officer::body_add_par(x = docx_file,
                              value = paste0("Note. N=", total_count),
                              style = footer_style)
    }
  }

