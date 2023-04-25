#' Creates a structured list with text interpretations for a set of variables.
#'
#' @inheritParams summarize_data
#' @inheritParams embed_cat_plot_html
#' @param contents The type of text interpretations to return, multiple allowed. Defaults to all.
#' @param include_numbers Whether or not to include the actual numbers (percentages, means) in parentheses.
#' @param require_common_categories Whether to check if all questions share common categories.
#' @param n_top_bottom The number of top and bottom entries to report.
#' @param translations A list of translations for the template text. See getOption("saros").
#'
#' @importFrom dplyr %>% arrange select filter group_map group_by pull slice_max slice_min
#' @importFrom rlang arg_match enquo .data .env
#' @importFrom tidyselect eval_select
#' @importFrom purrr map_chr
#' @importFrom cli cli_warn cli_abort ansi_collapse pluralize
#' @importFrom stringr str_c str_replace str_remove
#' @importFrom glue glue
#' @return List
#' @export
#'
#' @examples
#' embed_cat_text_html(ex_survey1,
#'                   cols = tidyselect::matches("e_"),
#'                   label_separator = " - ")
#'
embed_cat_text_html <-
  function(data,
           ...,
           cols = NULL,
           by = NULL, # Not implemented
           data_label = c("proportion", "percentage", "percentage_bare", "count", "mean", "median"),
           showNA = c("ifany", "always", "never"),
           label_separator = NULL,

           contents = c("intro", "not_used_category",
                        "mode_max",
                        "value_max", "value_min", "value_diff", # Diff not implemented
                        "mean_max", "mean_min", "mean_diff",
                        "median_max", "median_min", "median_diff", # Not implemented
                        "variance_max", "variance_min"), # Not implemented
           include_numbers = TRUE, # not implemented
           sort_by = NULL, # Lacks providing a single integer for the top or bottom categories
           descend = FALSE, # not implemented
           ignore_if_below = 0,
           n_top_bottom = 1,
           require_common_categories = TRUE,
           translations = getOption("saros")$translations,
           digits = 1,
           return_raw = TRUE,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    data_label <- rlang::arg_match(data_label, call = call)
    contents <- rlang::arg_match(arg = contents, multiple = TRUE, call = call)
    showNA <- rlang::arg_match(arg = showNA, call = call)


    check_data_frame(data, call = call)
    check_string(label_separator, null.ok = TRUE, call = call)
    check_bool(include_numbers, call = call)
    check_bool(descend, call = call)
    check_bool(require_common_categories, call = call)
    check_integerish(ignore_if_below, min = 0, call = call)
    check_integerish(n_top_bottom, min = 0, call = call)
    check_integerish(digits, min = 0, call = call)
    check_list(translations, null.ok = FALSE, call = call)

    # data <- dplyr::select(data, {{cols}}, {{by}})
    cols_pos <-
      rlang::enquo(arg = cols) %>%
      tidyselect::eval_select(data = data)
    by_pos <-
      rlang::enquo(arg = by) %>%
      tidyselect::eval_select(data = data)

    if(require_common_categories) {
      check_category_pairs(data = data,
                           cols_pos = cols_pos,
                           call = call)
    }

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



    generate_intro <- function() {

      if(length(by_pos) == 0) {

      get_raw_labels(data = data, cols_pos = cols_pos) %>%
        get_main_question2(label_separator = label_separator) %>%
        create_text_collapse() %>%
        stringr::str_c(translations$intro_prefix, .,
                       translations$intro_suffix)
      } else {
        get_raw_labels(data = data, cols_pos = cols_pos) %>%
          get_main_question2(label_separator = label_separator) %>%
          create_text_collapse() %>%
          stringr::str_c(translations$intro_prefix, .,
                         translations$intro_suffix)

      }
    }


    generate_mode_max <- function() {
      data_out %>%
        dplyr::slice_max(order_by = .data$.count, by = ".variable_label", n = 1) %>%
        dplyr::mutate(text = glue::glue("({.count}", if(.env$data_label == "percentage") "%" else "", ")",
                                        translations$mode_max_onfix, "{.variable_label}")) %>%
        dplyr::arrange(".category") %>%
        dplyr::summarize(text = create_text_collapse(.data$text),
                         .by = ".category") %>%
        dplyr::mutate(text = glue::glue("{.category} {text}")) %>%
        dplyr::pull(.data$text) %>%
        create_text_collapse() %>%
        stringr::str_c(translations$mode_max_prefix, .,
                       translations$mode_max_suffix)
    }


    generate_not_used_category <- function() {
      data_out %>%
        dplyr::filter(.data$.count < .env$ignore_if_below, !is.na(.data$.count)) %>%
        dplyr::select(tidyselect::all_of(c(".category", ".variable_label"))) %>%
        dplyr::arrange(dplyr::pick(tidyselect::all_of(c(".category", ".variable_label")))) %>%
        dplyr::group_by(.data$.category) %>%
        dplyr::group_map(.f = function(x, y) {
          create_text_collapse(x$.variable_label) %>%
            stringr::str_c(y$.category, " (", ., ")")
        }, .keep = TRUE) %>%
        unlist() %>%
        create_text_collapse() %>%
        {if(nchar(.) > 0) stringr::str_c(
          translations$not_used_prefix,
          .,
          translations$not_used_suffix) else ""}
    }



    generate_value_min_max <- function(contents) {
      if(all(!data_out$.comb_categories)) return("")

      slice_function <- ifelse(contents == "value_max",
                               dplyr::slice_max, dplyr::slice_min)
      prefix_key <- paste0(contents, "_prefix")
      infix_key <- paste0(contents, "_infix")
      suffix_key <- paste0(contents, "_suffix")

      category_selection <-
        data_out %>%
        dplyr::filter(.data$.comb_categories) %>%
        dplyr::distinct(.data$.category) %>%
        dplyr::pull(.data$.category)


        data_out %>%
        {if(!is.null(.[[".sum_value"]])) dplyr::mutate(., .count = .data$.sum_value) else .} %>%
        dplyr::filter(.data$.count >= .env$ignore_if_below, !is.na(.data$.count)) %>%
        slice_function(order_by = dplyr::pick(".count"), n = n_top_bottom, na_rm = TRUE, with_ties = FALSE) %>%
        dplyr::distinct(dplyr::pick(tidyselect::all_of(c(".variable_label", ".count"))), .keep_all = TRUE) %>%
        dplyr::mutate(text = glue::glue("{.variable_label} ({.count}", if(.env$data_label == "percentage") "%" else "", ")")) %>%
        dplyr::pull(.data$text) %>%
        create_text_collapse() %>%
        stringr::str_c(translations[[prefix_key]], .,
                       translations[[infix_key]],
                       create_text_collapse(category_selection),
                       translations[[suffix_key]]) %>%
        cli::pluralize() %>%
        stringr::str_replace(pattern = " 1 ", replacement = " ")
    }

    generate_mean_min_max <- function(contents) {
      slice_function <- ifelse(contents == "mean_max", dplyr::slice_max, dplyr::slice_min)
      prefix_key <- paste0(contents, "_prefix")
      infix_key <- paste0(contents, "_infix")
      suffix_key <- paste0(contents, "_suffix")

      data_out %>%
        dplyr::summarize(.mean = round(sum(.data$.mean_base, na.rm=TRUE)/100, digits = .env$digits),
                         .by = ".variable_label") %>%
        slice_function(order_by = .data$.mean, n = n_top_bottom) %>%
        dplyr::mutate(text = glue::glue("{.variable_label} (",
                                        translations$mean_onfix,
                                        "{.mean})")) %>%
        dplyr::pull(.data$text) %>%
        create_text_collapse() %>%
        stringr::str_c(translations[[prefix_key]], .,
                       translations[[suffix_key]])
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
    #   dplyr::slice_max(order_by = .data$median, n = n_top_bottom) %>%
    #   dplyr::mutate(text = glue::glue("{label} (",
    #                                   translations$median_onfix,
    #                                   "{median})")) %>%
    #   dplyr::pull(.data$text) %>%
    #   create_text_collapse() %>%
    #   stringr::str_c(translations$median_max_prefix, .,
    #                  translations$median_max_suffix)
    #
    # output$median_min <-
    #   data_median %>%
    #   dplyr::slice_min(order_by = .data$median, n = n_top_bottom) %>%
    #   dplyr::mutate(text = glue::glue("{.label} (",
    #                                   getOption("saros")$translations$median_onfix,
    #                                   "{median})")) %>%
    #   dplyr::pull(.data$text) %>%
    #   create_text_collapse() %>%
    #   stringr::str_c(getOption("saros")$translations$median_min_prefix, .,
    #                  getOption("saros")$translations$median_min_suffix)

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
    generate_output_text(contents = contents) %>%
      purrr::map(.f = ~{
        stringr::str_replace(string = .x, pattern = "([[:alpha:]\\)])$", "\\1.")
      })
    if(return_raw) stringr::str_c(collapse=" ") else out
  }




#' Improves the generated text by ChatGPT (v 14.03.23) magic.
#'
#' @param x String
#' @param log Previous messages as outputted from the function.
#' @param temperature How creative the responses are, from 0 to 2, default in ChatGPT is 1, here set to .5 to avoid changes to the facts.
#'
#' @return List with response and input settings.
#' @export
#'
improve_text <- function(x, log=NULL, temperature = .5) {
  msg <-
    list(
      list(
        "role" = "system",
        "content" = "You are a helpful assistant with deep expertise on clarity and variation in written language. You will rewrite sentences I send you to improve variety and brevity. You will not change the meaning and you will always keep numbers within parentheses."
      ),
      list(
        "role" = "user",
        "content" = x
      )
    )
  out <-
    openai::create_chat_completion(model = "gpt-3.5-turbo-0301",
                                   messages = msg, temperature = temperature)
  c(list(input=x),
    out)
}
