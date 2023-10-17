#' Creates a structured list with text interpretations for a set of variables.
#'
#' @inheritParams draft_report
#' @inheritParams gen_qmd_chapters
#' @inheritParams summarize_data
#'
#' @importFrom dplyr %>%
#' @return List
#' @export
#'
#' @examples
#' embed_cat_text_html(ex_survey1,
#' dep = tidyselect::matches("e_"),
#' contents = c("intro", "mode_max", "value_max",
#' "value_min", "not_used_category", "mean_max", "mean_min"),
#' label_separator = " - ",
#' require_common_categories = FALSE,
#' n_top_bottom = 1,
#' showNA = "never",
#' descend = TRUE,
#' return_raw = TRUE,
#' hide_label_if_prop_below=0,
#' data_label = "count",
#' data_label_decimal_symbol = ",",
#' digits = 1)
#'
embed_cat_text_html <-
  function(data,
           dep = NULL,
           indep = NULL, # Not implemented
           ...,

           mesos_group = NULL,
           call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "cat_text")

    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data, error_call = call)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data, error_call = call)

    if(dots$require_common_categories) {
      check_category_pairs(data = data,
                           cols_pos = dep_pos,
                           call = call)
    }

    data_out <-
      rlang::exec(
        summarize_data,
        data = data,
        dep = names(dep_pos),
        indep = names(indep_pos),
        label_separator = dots$label_separator,
        !!!dots)



    generate_intro <- function() {

      if(length(indep_pos) == 0) {

      get_raw_labels(data = data, col_pos = dep_pos) %>%
        get_main_question2(label_separator = dots$label_separator) %>%
        create_text_collapse(last_sep = dots$translations$last_sep) %>%
        stringi::stri_c(ignore_null=TRUE, dots$translations$intro_prefix, .,
                       dots$translations$intro_suffix)
      } else {
        get_raw_labels(data = data, col_pos = dep_pos) %>%
          get_main_question2(label_separator = dots$label_separator) %>%
          create_text_collapse(last_sep = dots$translations$last_sep) %>%
          stringi::stri_c(ignore_null=TRUE, dots$translations$intro_prefix, .,
                         dots$translations$intro_suffix)

      }
    }


    generate_mode_max <- function() {
      data_out %>%
        dplyr::slice_max(order_by = .data$.count, by = ".variable_label", n = 1) %>%
        dplyr::mutate(.count = round(.data$.count, digits = dots$digits),
                      text = glue::glue("({.count}",
                                        if(.env$dots$data_label == "percentage") "%" else "", ")",
                                        dots$translations$mode_max_onfix, "{.variable_label}")) %>%
        dplyr::arrange(".category") %>%
        dplyr::summarize(text = create_text_collapse(.data$text,
                                                     last_sep = dots$translations$last_sep),
                         .by = ".category") %>%
        dplyr::mutate(text = glue::glue("{.category} {text}")) %>%
        dplyr::pull(.data$text) %>%
        create_text_collapse(last_sep = dots$translations$last_sep) %>%
        stringi::stri_c(ignore_null=TRUE, dots$translations$mode_max_prefix, .,
                       dots$translations$mode_max_suffix)
    }


    generate_not_used_category <- function() {
      out <-
        data_out %>%
        dplyr::filter(!is.na(.data$.count)) %>% #.data$.count < .env$dots$hide_label_if_prop_below,
        dplyr::select(tidyselect::all_of(c(".category", ".variable_label"))) %>%
        dplyr::arrange(dplyr::pick(tidyselect::all_of(c(".category", ".variable_label")))) %>%
        dplyr::group_by(.data$.category) %>%
        dplyr::group_map(.f = function(x, y) {
          create_text_collapse(x$.variable_label,
                               last_sep = dots$translations$last_sep) %>%
            stringi::stri_c(ignore_null=TRUE, y$.category, " (", ., ")")
        }, .keep = TRUE) %>%
        unlist() %>%
        create_text_collapse(last_sep = dots$translations$last_sep)

      if(length(out)>0 &&
         !is.na(out) &&
         stringi::stri_length(out) > 0) {
        stringi::stri_c(dots$translations$not_used_prefix,
                        out,
                        dots$translations$not_used_suffix, ignore_null=TRUE)
        } else ""
    }



    generate_value_min_max <- function(contents) {
      if(all(!data_out$.comb_categories)) return("")

      slice_function <- ifelse(contents == "value_max",
                               dplyr::slice_max, dplyr::slice_min)
      prefix_key <- stringi::stri_c(ignore_null=TRUE, contents, "_prefix")
      infix_key <- stringi::stri_c(ignore_null=TRUE, contents, "_infix")
      suffix_key <- stringi::stri_c(ignore_null=TRUE, contents, "_suffix")

      category_selection <-
        data_out %>%
        dplyr::filter(.data$.comb_categories) %>%
        dplyr::distinct(.data$.category) %>%
        dplyr::pull(.data$.category)


        data_out %>%
        {if(!is.null(.[[".sum_value"]])) dplyr::mutate(., .count = .data$.sum_value) else .} %>%
        dplyr::filter(!is.na(.data$.count)) %>% #.data$.count >= .env$dots$hide_label_if_prop_below,
        slice_function(order_by = dplyr::pick(".count"), n = dots$n_top_bottom, na_rm = TRUE, with_ties = FALSE) %>%
        dplyr::distinct(dplyr::pick(tidyselect::all_of(c(".variable_label", ".count"))), .keep_all = TRUE) %>%
        dplyr::mutate(.count = round(.data$.count, digits = dots$digits),
                      text = glue::glue("{.variable_label} ({.count}", if(.env$dots$data_label %in% c("percentage", "percentage_bare")) "%" else "", ")")) %>%
        dplyr::pull(.data$text) %>%
        create_text_collapse(last_sep = dots$translations$last_sep) %>%
        stringi::stri_c(ignore_null=TRUE, dots$translations[[prefix_key]], .,
                       dots$translations[[infix_key]],
                       create_text_collapse(category_selection,
                                            last_sep = dots$translations$last_sep),
                       dots$translations[[suffix_key]]) %>%
        cli::pluralize() %>%
        stringi::stri_replace(regex = " 1 ", replacement = " ")
    }

    generate_mean_min_max <- function(contents) {
      slice_function <- ifelse(contents == "mean_max", dplyr::slice_max, dplyr::slice_min)
      prefix_key <- stringi::stri_c(ignore_null=TRUE, contents, "_prefix")
      infix_key <- stringi::stri_c(ignore_null=TRUE, contents, "_infix")
      suffix_key <- stringi::stri_c(ignore_null=TRUE, contents, "_suffix")


      data_out %>%
        # dplyr::summarize(.mean = #round(sum(.data$.mean_base, na.rm=TRUE)/100, digits = .env$dots$digits),
        #                  .by = ".variable_label") %>%
        slice_function(order_by = .data$.mean, n = dots$n_top_bottom) %>%
        dplyr::mutate(.mean = round(.data$.mean, digits = max(c(1, dots$digits), na.rm = TRUE)),
                      text = glue::glue("{.variable_label} (",
                                        dots$translations$mean_onfix,
                                        "{.mean})")) %>%
        dplyr::pull(.data$text) %>%
        create_text_collapse(last_sep = dots$translations$last_sep) %>%
        stringi::stri_c(ignore_null=TRUE, dots$translations[[prefix_key]], .,
                       dots$translations[[suffix_key]])
    }

    ############## median ######################
    # if(any(contents %in% c("median_max", "median_min"))) {
    # data_median <-
    #   data_out %>%
    #   dplyr::mutate(variable_order = as.integer(.data$.variable)) %>%
    #   dplyr::arrange(tidyselect::all_of(c(".label", ".variable_order"))) %>%
    #   dplyr::group_by(.data$.label) %>%
    #   dplyr::mutate(cum_value = cumsum(.data$.count),
    #                 m = sum(.data$.count, na.rm=TRUE)/2,
    #                 median_category = cum_value > m & !duplicated(cum_value > m)) %>%
    #   3+((50-43)/26)*1
    #


    # output$median_max <-
    #   data_median %>%
    #   dplyr::slice_max(order_by = .data$median, n = dots$n_top_bottom) %>%
    #   dplyr::mutate(text = glue::glue("{label} (",
    #                                   dots$translations$median_onfix,
    #                                   "{median})")) %>%
    #   dplyr::pull(.data$text) %>%
    #   create_text_collapse(last_sep = dots$translations$last_sep) %>%
    #   stringi::stri_c(ignore_null=TRUE, dots$translations$median_max_prefix, .,
    #                  dots$translations$median_max_suffix)
    #
    # output$median_min <-
    #   data_median %>%
    #   dplyr::slice_min(order_by = .data$median, n = dots$n_top_bottom) %>%
    #   dplyr::mutate(text = glue::glue("{.label} (",
    #                                   .saros.env$defaults$translations$median_onfix,
    #                                   "{median})")) %>%
    #   dplyr::pull(.data$text) %>%
    #   create_text_collapse(last_sep = dots$translations$last_sep) %>%
    #   stringi::stri_c(ignore_null=TRUE, .saros.env$defaults$translations$median_min_prefix, .,
    #                  .saros.env$defaults$translations$median_min_suffix)

    # }

    #######variance_max ##########







    # Helper function to generate the output text for each content type
    generate_output_text <-
      function(contents) {
        output <- list()

        if (any(contents %in% "intro")) {
          output$intro <- generate_intro()
        }

        if (any(contents %in% "mode_max")) {
          output$mode_max <- generate_mode_max()
        }

        if (any(contents %in% "not_used_category")) {
          output$not_used_category <- generate_not_used_category()
        }

        if (any(contents %in% "value_max")) {
          output$value_max <- generate_value_min_max(contents="value_max")
        }

        if (any(contents %in% "value_min")) {
          output$value_min <- generate_value_min_max(contents="value_min")
        }

        if (any(contents %in% "mean_max")) {
          output$mean_max <- generate_mean_min_max(contents="mean_max")
        }

        if (any(contents %in% "mean_min")) {
          output$mean_min <- generate_mean_min_max(contents="mean_min")
        }

        output
      }


    out <-
    generate_output_text(contents = dots$contents) %>%
      lapply(FUN = function(.x) {
        stringi::stri_replace(str = .x, regex = "([[:alpha:]\\)])$", "$1.")
      })

    if(dots$return_raw) as.list(stringi::stri_c(ignore_null=TRUE, out, collapse=" ")) else out
  }




# #' Improves the generated text by ChatGPT (v 14.03.23) magic.
# #'
# #' @param x String
# #' @param log Previous messages as outputted from the function.
# #' @param temperature How creative the responses are, from 0 to 2, default in ChatGPT is 1, here set to .5 to avoid changes to the facts.
# #'
# #' @return List with response and input settings.
# #' @export
# #'
# improve_text <- function(x, log=NULL, temperature = .5) {
#   msg <-
#     list(
#       list(
#         "role" = "system",
#         "content" = "You are a helpful assistant with deep expertise on clarity and variation in written language. You will rewrite sentences I send you to improve variety and brevity. You will not change the meaning and you will always keep numbers within parentheses."
#       ),
#       list(
#         "role" = "user",
#         "content" = x
#       )
#     )
#   out <-
#     openai::create_chat_completion(model = "gpt-3.5-turbo-0301",
#                                    messages = msg, temperature = temperature)
#   c(list(input=x),
#     out)
# }
