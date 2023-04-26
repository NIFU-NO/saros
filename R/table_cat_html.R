
#' Embed Reactable Table
#'
#' @inheritParams summarize_data
#' @inheritParams embed_cat_plot_html
#' @param information Which pre-computed information for each variable-category to display.
#' @param ... Further arguments passed down.
#' @importFrom rlang !!! exec
#'
#' @return A reactable object. If return_raw=FALSE, then just the table.
#' @export
#'
#' @examples
#' embed_cat_table_html(data=ex_survey1, cols = a_1:a_9)
#' embed_cat_table_html(data=ex_survey1, cols = a_1:a_9, by = x1_sex)
embed_cat_table_html <-
  function(data,
           ...,
           cols = tidyselect::everything(),
           by = NULL,
           showNA = c("ifany", "always", "never"),
           data_label = c("proportion", "percentage", "percentage_bare", "count", "mean", "median"),
           digits = 1,
           sort_by = NULL, #c(".variable_name"), #, ".variable_label", ".category"
           vertical = FALSE,
           descend = FALSE,
           ignore_if_below = 0,
           information =
             c(".variable_label", #".variable_name",
               ".category",
               ".count", ".count_se",
               ".proportion", ".proportion_se",
               ".mean", ".mean_se", ".mean_base",
               ".data_label", ".comb_categories", ".sum_value"),
           label_separator = NULL,
           return_raw = TRUE,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    showNA <- rlang::arg_match(showNA, error_call = call)
    data_label <- rlang::arg_match(data_label, error_call = call)
    if(data_label %in% c("percentage", "percentage_bare", "proportion")) {
      data_label2 <- "count"
    } else {
      data_label2 <- "percentage"
    }
    check_data_frame(data, call = call)
    check_string(label_separator, null.ok=TRUE, call = c)

    cols_enq <- rlang::enquo(arg = cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data, error_call = call)
    by_enq <- rlang::enquo(arg = by)
    by_pos <- tidyselect::eval_select(by_enq, data = data, error_call = call)

    check_category_pairs(data = data, cols_pos = c(cols_pos), call = call)

    main_question <-
      get_raw_labels(data = data, cols_pos = cols_pos) %>%
      get_main_question2(label_separator = label_separator, warn_multiple = TRUE, call = call) %>%
      stringr::str_unique()

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
      tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", names(by_pos))),
                         names_from = ".category", values_from = ".data_label")
    if(nchar(main_question)>0) {
      names(data_out)[names(data_out)==".variable_label"] <- main_question
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
      tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", names(by_pos))),
                         names_from = ".category", values_from = ".data_label")
    if(nchar(main_question)>0) {
      names(data_out2)[names(data_out)==".variable_label"] <- main_question
    }

    ## MOVE THIS AND THE pivot_wider ABOVE TO prep_cat_table_html()
    table_html_detailer <-
      function(index) {
      htmltools::div(
        "Details for row: ", index,
        htmltools::tags$pre(paste(utils::capture.output(data_out2[index, ]), collapse = "\n"))
      )
      }
    if(return_raw) {
      data_out
    } else {
      data_out %>%
        reactable::reactable(sortable = TRUE)
    }
  }

