
#' Embed Reactable Table
#'
#' @inheritParams summarize_data
#' @inheritParams embed_cat_prop_plot
#' @param information Which pre-computed information for each variable-category to display.
#' @param ... Further arguments passed down.
#' @importFrom rlang !!!
#'
#' @return A reactable object. If return_raw=FALSE, then just the table.
#' @export
embed_cat_table <-
  function(data,
           ...,
           cols = tidyselect::everything(),
           by = NULL,
           summarized_data = NULL,
           label_separator = NULL,
           tailored_group = NULL,
           translations = .saros.env$defaults$translations,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)

    if(dots$data_label %in% c("percentage", "percentage_bare", "proportion")) {
      data_label2 <- "count"
    } else {
      data_label2 <- "percentage"
    }

    cols_enq <- rlang::enquo(arg = cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data, error_call = call)
    by_enq <- rlang::enquo(arg = by)
    by_pos <- tidyselect::eval_select(by_enq, data = data, error_call = call)

    check_category_pairs(data = data, cols_pos = c(cols_pos), call = call)




    ######### MUST TIDY UP FROM HERE ############
    main_question <-
      get_raw_labels(data = data, cols_pos = cols_pos) %>%
      get_main_question2(label_separator = label_separator, warn_multiple = TRUE, call = call) %>%
      unique()

    if(length(by_pos)>0) {
    by_label <-
      get_raw_labels(data = data, cols_pos = by_pos) %>%
      get_main_question2(label_separator = label_separator, warn_multiple = TRUE, call = call) %>%
      unique()
    } else by_label <- character(0)

    # by_label <- unname(get_raw_labels(data = data, cols_pos = by_pos))

    data_out <-
      rlang::exec(
        summarize_data,
        data = data,
        cols = names(cols_pos),
        by = names(by_pos),
        label_separator = label_separator,
        translations = translations,
        call = call,
        !!!dots)


    caption <-
    if(!rlang::is_null(label_separator)) {
        add_caption_attribute(main_question = main_question,
                              data_out = data_out, by_pos = by_label,
                              tailored_group = tailored_group,
                              translations = translations)
    }
    cat_lvls <- levels(data_out[[".category"]])
    cat_lvls <- cat_lvls[cat_lvls %in% unique(data_out[[".category"]])]
    if(length(by_label)==1 && length(names(by_pos))==0)  browser()

    data_out <-
      data_out %>%
      dplyr::mutate(N = sum(.data$.count, na.rm=TRUE), # Move to summarize_data?
                    .by = tidyselect::all_of(c(".variable_name", names(by_pos)))) %>%
      dplyr::arrange(as.integer(.data$.variable_label), if(length(by_pos)>0) as.integer(.data[[names(by_pos)]])) %>%
      tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", names(by_pos), "N")),
                         names_from = ".category", values_from = ".data_label") %>%
      dplyr::relocate(tidyselect::all_of(c(".variable_label", names(by_pos), cat_lvls, "N")), .after = 1) %>%
      # dplyr::relocate("N", .after = tidyselect::last_col()) %>%
      dplyr::rename_with(.cols = tidyselect::all_of(cat_lvls), .fn = ~stringr::str_c(.x, if(dots$data_label %in% c("percentage", "percentage_bare")) " (%)")) %>%
      dplyr::rename_with(.cols = "N", .fn = function(x) translations$table_heading_N)
    if(length(by_pos)>0) data_out <- dplyr::rename_with(data_out, .cols = tidyselect::all_of(names(by_pos)), .fn = function(x) by_label)

    if(rlang::is_string(main_question) && stringi::stri_length(main_question)>0) {
      names(data_out)[names(data_out)==".variable_label"] <- main_question
    }
    data_out[[1]] <- if(dplyr::n_distinct(data_out[[1]], na.rm = FALSE) > 1) data_out[[1]]

    attr(data_out, "saros_caption") <- caption


    ## MOVE THIS AND THE pivot_wider ABOVE TO prep_cat_table()

    if(dots$return_raw) {
      data_out
    } else {

      dots$data_label <- data_label2

      data_out2 <-
        rlang::exec(
          summarize_data,
          data = data,
          cols = names(cols_pos),
          by = names(by_pos),
          label_separator = label_separator,
          call = call,
          !!!dots) %>%
        dplyr::mutate(N = sum(.data$.count, na.rm=TRUE),
                      .by = tidyselect::all_of(c(".variable_name", names(by_pos)))) %>%
        dplyr::arrange(as.integer(.data$.variable_label)) %>%
        tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", names(by_pos), "N")),
                           names_from = ".category", values_from = ".data_label") %>%
        dplyr::relocate(tidyselect::all_of(cat_lvls), .after = 1) %>%
        dplyr::relocate("N", .after = tidyselect::last_col()) %>%
        dplyr::rename_with(.cols = tidyselect::all_of(cat_lvls), .fn = ~stringr::str_c(.x, if(dots$data_label %in% c("percentage", "percentage_bare")) " (%)")) %>%
        dplyr::rename_with(.cols = "N", .fn = function(x) translations$table_heading_N) %>%
        dplyr::rename_with(.cols = tidyselect::all_of(names(by_pos)), .fn = function(x) by_label)

      if(rlang::is_string(main_question) && stringi::stri_length(main_question)>0) {
        names(data_out2)[names(data_out)==".variable_label"] <- main_question
      }
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
  }

