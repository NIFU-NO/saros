
#' Embed Reactable Table
#'
#' @inheritParams summarize_data
#' @param vertical [\code{logical(1)}] Logical. If FALSE (default), then horizontal.
#' @param information Which pre-computed information for each variable-category to display.
#' @param ... Further arguments passed to reactable()
#' @importFrom rlang !!! exec
#'
#' @return
#' @export
#'
#' @examples
#' embed_table_cat_html(data=ex_survey1, cols = a_1:a_9)
embed_table_cat_html <-
  function(data,
           ...,
           cols = tidyselect::everything(),
           by = NULL,
           showNA = c("ifany", "always", "no"),
           data_label = c("proportion", "percentage", "percentage_bare", "count", "mean", "median"),
           digits = 1,
           sort_by = c(".variable_name"), #, ".variable_label", ".category"
           vertical = FALSE,
           descend = FALSE,
           ignore_if_below = 1,
           information =
             c(".variable_label", #".variable_name",
               ".category",
               ".count", ".count_se",
               ".proportion", ".proportion_se",
               ".mean", ".mean_se", ".mean_base",
               ".data_label", ".comb_categories", ".sum_value"),
           label_separator = NULL,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    showNA <- rlang::arg_match(showNA, error_call = call)
    data_label <- rlang::arg_match(data_label, error_call = call)
    check_data_frame(data)
    check_string(label_separator, null.ok=TRUE)

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
        !!!dots) %>%
      tidyr::pivot_wider(id_cols = c(".variable_name", ".variable_label"), names_from = ".category", values_from = ".data_label")

    if(data_label %in% c("percentage", "percentage_bare", "proportion")) {
      data_label2 <- "count"
    } else {
      data_label2 <- "percentage"
    }

    data_out2 <-
      rlang::exec(
        summarize_data,
        data = data,
        cols = cols_pos,
        by = by_pos,
        data_label = data_label2,
        showNA = showNA,
        digits = digits,
        sort_by = sort_by,
        descend = descend,
        ignore_if_below = ignore_if_below,
        label_separator = label_separator,
        call = call,
        !!!dots) %>%
      tidyr::pivot_wider(id_cols = c(".variable_name", ".variable_label"), names_from = ".category", values_from = ".data_label")

    table_html_detailer <-
      function(index) {
      htmltools::div(
        "Details for row: ", index,
        htmltools::tags$pre(paste(utils::capture.output(data_out2[index, ]), collapse = "\n"))
      )
    }
    data_out %>%
      reactable::reactable(sortable = TRUE)
  }

