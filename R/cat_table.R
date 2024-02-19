
#' Embed Reactable Table
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#' @inheritParams gen_qmd_chapters
#' @importFrom rlang !!!
#'
#' @return A reactable object. If return_raw=FALSE, then just the table.
#' @export
embed_cat_table <-
  function(data,
           ...,
           dep = tidyselect::everything(),
           indep = NULL,
           mesos_group = NULL,
           call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "cat_table")

    # check_summary_data_cols(data, call = call)

    if(dots$data_label %in% c("percentage", "percentage_bare", "proportion")) {
      data_label2 <- "count"
    } else {
      data_label2 <- "percentage"
    }

    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data, error_call = call)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data, error_call = call)

    check_category_pairs(data = data, cols_pos = c(dep_pos))




    ######### MUST TIDY UP FROM HERE ############
    main_question <-
      get_raw_labels(data = data, col_pos = dep_pos) %>%
      get_main_question2(label_separator = dots$label_separator,
                         warn_multiple = TRUE) %>%
      unique()

    if(length(indep_pos)>0) {
      indep_label <- get_raw_labels(data = data, col_pos = indep_pos)
      indep_label <- get_main_question2(indep_label,
                                        label_separator = dots$label_separator,
                                        warn_multiple = TRUE)
      indep_label <- unique(indep_label)
      if(nchar(indep_label)==0) browser() #cli::cli_warn("Indep {.var {indep_pos}} lacks a label.")

    } else indep_label <- character(0)

    # indep_label <- unname(get_raw_labels(data = data, col_pos = indep_pos))

    data_out <-
      rlang::exec(
        summarize_data,
        data = data,
        dep = names(dep_pos),
        indep = names(indep_pos),
        label_separator = dots$label_separator,
        translations = dots$translations,
        !!!dots)


    caption <-
    if(!is.null(dots$label_separator)) {
        create_caption(main_question = main_question,
                              data_out = data_out,
                              indep_pos = indep_label,
                              mesos_group = mesos_group,
                       filepath = NULL,
                              translations = dots$translations)
    }
    cat_lvls <- levels(data_out[[".category"]])
    cat_lvls <- cat_lvls[cat_lvls %in% unique(data_out[[".category"]])]
    if(length(indep_label)==1 && length(names(indep_pos))==0)  browser()

    # if(any(data_out$.variable_label == "Driving")) {
    #
    #   data_out %>%
    #     dplyr::mutate(N = sum(.data$.count, na.rm=TRUE), # Move to summarize_data?
    #                   .by = tidyselect::all_of(c(".variable_name", names(indep_pos)))) %>%
    #     dplyr::arrange(as.integer(.data$.variable_label), if(length(indep_pos)>0) as.integer(.data[[names(indep_pos)]])) %>%
    #     # tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", names(indep_pos), "N")),
    #     #                  names_from = ".category", values_from = ".data_label") %>%
    #     print()
    # }
    data_out <-
      data_out %>%
      dplyr::mutate(N = sum(.data$.count, na.rm=TRUE), # Move to summarize_data?
                    .by = tidyselect::all_of(c(".variable_name", names(indep_pos)))) %>%
      dplyr::arrange(as.integer(.data$.variable_label), if(length(indep_pos)>0) as.integer(.data[[names(indep_pos)]])) %>%
      tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", names(indep_pos), "N")),
                         names_from = ".category", values_from = ".data_label") %>%
      dplyr::relocate(tidyselect::all_of(c(".variable_label", names(indep_pos), cat_lvls, "N")), .after = 1) %>%
      # dplyr::relocate("N", .after = tidyselect::last_col()) %>%
      dplyr::rename_with(.cols = tidyselect::all_of(cat_lvls),
                         .fn = ~stringi::stri_c(ignore_null=TRUE, .x, if(dots$data_label %in% c("percentage", "percentage_bare")) " (%)")) %>%
      dplyr::rename_with(.cols = "N",
                         .fn = function(x) dots$translations$table_heading_N)
    if(length(indep_pos)>0 &&
       is.character(indep_label) &&
       length(indep_label) == length(indep_pos) &&
       all(nchar(indep_label)>0)) {

      data_out <- dplyr::rename_with(data_out,
                                     .cols = tidyselect::all_of(names(indep_pos)),
                                     .fn = function(x) indep_label)
    }

    if(is_string(main_question) && stringi::stri_length(main_question)>0) {
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
          dep = names(dep_pos),
          indep = names(indep_pos),
          label_separator = dots$label_separator,
          !!!dots) %>%
        dplyr::mutate(N = sum(.data$.count, na.rm=TRUE),
                      .by = tidyselect::all_of(c(".variable_name", names(indep_pos)))) %>%
        dplyr::arrange(as.integer(.data$.variable_label)) %>%
        tidyr::pivot_wider(id_cols = tidyselect::all_of(c(".variable_label", names(indep_pos), "N")),
                           names_from = ".category", values_from = ".data_label") %>%
        dplyr::relocate(tidyselect::all_of(cat_lvls), .after = 1) %>%
        dplyr::relocate("N", .after = tidyselect::last_col()) %>%
        dplyr::rename_with(.cols = tidyselect::all_of(cat_lvls),
                           .fn = ~stringi::stri_c(ignore_null=TRUE, .x, if(dots$data_label %in% c("percentage", "percentage_bare")) " (%)")) %>%
        dplyr::rename_with(.cols = "N",
                           .fn = function(x) dots$translations$table_heading_N) %>%
        dplyr::rename_with(.cols = tidyselect::all_of(names(indep_pos)), .fn = function(x) indep_label)

      if(is_string(main_question) && stringi::stri_length(main_question)>0 &&
         isTRUE(dots$table_main_question_as_header)) {
        names(data_out2)[names(data_out)==".variable_label"] <- main_question
      }
      # table_html_detailer <-
      #   function(index) {
      #     htmltools::div(
      #       "Details for row: ", index,
      #       htmltools::tags$pre(paste(utils::capture.output(data_out2[index, ]), collapse = "\n"))
      #     )
      #   }

      data_out #%>%
        # reactable::reactable(sortable = TRUE)
    }
  }

