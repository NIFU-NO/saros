

#' Test Significance Based on Randomization Theory
#'
#' @inheritParams draft_report
#' @inheritParams summarize_data
#'
#' @return Data frame
#' @export
#'
embed_sigtest <-
  function(data,
           chapter_overview,
           ...,
           call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "sigtest")


    main_question <-
      get_raw_labels(data = data,
                     col_pos = unique(as.character(chapter_overview$.variable_name_dep)))
    main_question <-
      get_main_question2(main_question,
                         label_separator = dots$label_separator,
                         warn_multiple = TRUE)
    main_question <- unique(main_question)
    main_question <-
      if(!is.null(dots$label_separator) &&
         length(main_question)==1 &&
         !is.na(main_question) &&
         nchar(main_question) > 0) main_question else ".variable_label"

    out <- dplyr::ungroup(chapter_overview)

    columns_needed <-
      c(".variable_name_dep", ".variable_label_suffix_dep", ".variable_type_dep",
        ".variable_name_indep", ".variable_label_suffix_indep", ".variable_type_indep",
        ".p_value", ".bi_test")
    out <- out[,columns_needed]

    for(i in seq_len(nrow(chapter_overview))) {

      y_var <- as.character(out[i, ".variable_name_dep", drop=TRUE])
      x_var <- as.character(out[i, ".variable_name_indep", drop=TRUE])
      if(is.na(x_var)) x_var <- NULL



      if(is.na(out[i, ".p_value", drop=TRUE]) &&
         out[i, ".variable_type_dep", drop=TRUE] %in% c("fct", "ord", "dbl", "int") &&
         !is.na(y_var)) {

        if(!is.null(x_var)) {

          data2 <- data[!is.na(data[[y_var]]) &
                          !is.na(data[[x_var]]), , drop=FALSE]
        } else {
          data2 <- data[!is.na(data[[y_var]]), , drop=FALSE]
        }

        y <- data2[[y_var]]
        x <- if(!is.null(x_var)) data2[[x_var]]
        stat_result <- find_test2(y=y, x=x)

        if(nrow(stat_result) >= 1) {
          out[i, ".p_value"] <- stat_result$.p_value
          out[i, ".bi_test"] <- stat_result$.bi_test
        }
      }
    }

    attr(out, "saros_caption") <- stringi::stri_c(ignore_null=TRUE,
                                                  dots$translations$sigtest_prefix,
                                                  main_question,
                                                  dots$translations$sigtest_suffix)

    out

  }

