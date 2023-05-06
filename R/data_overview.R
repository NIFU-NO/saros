
eval_cols <- function(x, data, call = rlang::caller_env()) {
  check_string(x = x, n = NULL, null.ok = FALSE, call = call)
  check_data_frame(data, call = call)
  x %>%
    purrr::map(.f = function(col_entry) {
      if(stringi::stri_length(col_entry)>0) {
        expr <- stringr::str_c('tidyselect::eval_select(expr = rlang::expr(c(',
                            col_entry,
                            ')), data = data)')
        out <- tryCatch(eval(parse(text = expr)),
                 error = function(e) cli::cli_abort("Column {.var {col_entry}} does not exist in data.",
                                        call = call)
          )

      } else out <- NA_integer_
    })

}


look_for_extended <- function(data,
                              cols = tidyselect::everything(),
                              label_separator = NULL,
                              name_separator = NULL) {
  ### Assume that related columns always have identical label prefix AND overlapping response categories.
  ### Assume that variables with identical label prefix may not be related.
  ### Assume that related columns are always next to each other OR share same variable name prefix.

  data_part <- dplyr::select(data, {{cols}})
  if(ncol(data_part) == 0 || nrow(data_part) == 0) cli::cli_abort("data.frame is of 0 length.")

  x <- data.frame(
    col_pos = match(colnames(data_part), colnames(data)),
    col_name = colnames(data_part),
    col_label = get_raw_labels(data = data_part),
    col_type = purrr::map_chr(names(data_part), ~vctrs::vec_ptype_abbr(data_part[[.x]])),
    row.names = NULL
  )

  if(!is.null(name_separator)) {
    if(rlang::is_character(name_separator, n = 1)) {
      x <-
        x %>%
        tidyr::separate_wider_delim(cols = "col_name",
                                    delim = name_separator,
                                    names = c("name_prefix", "name_suffix"),
                                    cols_remove = FALSE,
                                    too_few = "align_end", too_many = "merge")
      if(sum(stringi::stri_count_fixed(str = x$name_suffix, pattern = " - "), na.rm=TRUE) > 0) {
        cli::cli_warn(c("{.arg name_separator} matches more than one delimiter, your output is likely ugly.",
                      i="Consider renaming your variables with e.g. {.fun dplyr::rename_with()}."))
      }

    } else cli::cli_abort("Non-string {.arg name_separator} currently not supported.")
  } else x <- x %>% dplyr::mutate(name_prefix = .data$col_name,
                                  name_suffix = .data$col_name)

  if(!is.null(label_separator)) {
    if(rlang::is_character(label_separator, n = 1)) {
      x <-
        x %>%
        tidyr::separate_wider_delim(cols = "col_label",
                                    names = c("label_prefix", "label_suffix"),
                                    delim = label_separator,
                                    too_few = "align_end", too_many = "merge")
      if(sum(stringi::stri_count_fixed(str = x$label_suffix, pattern = " - "), na.rm=TRUE) > 0) {
        cli::cli_warn(c("{.arg label_separator} matches more than one delimiter, your output is likely ugly.",
                        i="Consider renaming your variable labels with e.g. {.fun labelled::set_variable_labels}."))
      }

    }  else cli::cli_abort("Non-string {.arg label_separator} currently not supported.")
  } else x <- x %>% dplyr::mutate(label_prefix = .data$col_label,
                                  label_suffix = .data$col_label,
                                  col_label = NULL)
  grouping_vars <-
    c(if(!is.null(label_separator)) "label_prefix",
      if(!is.null(name_separator)) "name_prefix")

  x %>%
    dplyr::mutate(

      name_prefix = dplyr::if_else(
        is.na(.data$name_prefix) & !is.na(.data$name_suffix),
        .data$name_suffix,
        .data$name_prefix),


      name_suffix = dplyr::if_else(
        is.na(.data$name_suffix) & !is.na(.data$name_prefix),
        .data$name_prefix,
        .data$name_suffix),

      label_prefix = dplyr::if_else(
        is.na(.data$label_prefix) & !is.na(.data$label_suffix),
        .data$label_suffix,
        .data$label_prefix),

      label_suffix = dplyr::if_else(
        is.na(.data$label_suffix) & !is.na(.data$label_prefix),
        .data$label_prefix,
        .data$label_suffix),

    ) %>%
    dplyr::relocate("col_pos", "col_name", "name_prefix", "name_suffix",
                    "label_prefix", "label_suffix", "col_type") %>%
    dplyr::mutate(col_group = dplyr::cur_group_id(),
                  .by = tidyselect::all_of(if(length(grouping_vars)>0) grouping_vars else "col_pos"))

  ### Return a grouped data frame with
  ### main question variable name prefix,
  ### main question variable label (prefix),
  ### subquestion variable name suffix,
  ### subquestion variable label (suffix)
  ### var_group,
  ### col_type,
  ### designated_role, designated_type, uni_bi_variate,
}
#




attach_indep <- function(refined_data_overview) {
  if(!rlang::is_null(refined_data_overview$designated_role)) {

    by_df <-
      refined_data_overview %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$designated_role == "indep") %>%
      tidyr::nest(.by = tidyselect::all_of(c("chapter")),
                  .key = "by_cols_df")

    dplyr::left_join(x = refined_data_overview,
                     y = by_df,
                     by = dplyr::join_by("chapter"))

  } else {
    cli::cli_warn("No column {.var designated_role} found, no bivariates possible.")
    dplyr::mutate(refined_data_overview, by_cols_df = list(NULL))
  }
}

remove_non_significant_bivariates <-
  function(refined_data_overview,
           data,
           hide_bi_entry_if_sig_above = .05,
           always_show_bi_for_by = c(),
           call = rlang::caller_env()) {

    check_double(hide_bi_entry_if_sig_above, min = 0, max = 1, call = call)
    check_string(always_show_bi_for_by, null.ok = TRUE, n = NULL, call = call)
    if(hide_bi_entry_if_sig_above < 1) {
      cli::cli_progress_message("Removing bivariate occurences if {.arg hide_bi_entry_if_sig_above}: {.arg {hide_bi_entry_if_sig_above}}.")

      out <-
        refined_data_overview %>%
        dplyr::rowwise() %>%
        dplyr::group_map(.keep = TRUE, .f = function(df_col_row, df_col_key) {

          out_by <-
            df_col_row$by_cols_df[[1]] %>%
            dplyr::rowwise() %>%
            dplyr::group_map(.keep = TRUE, .f = function(df_by_row, by_df_key) {

              if(df_by_row$col_name != df_col_row$col_name) {

                df_chitest <-
                  data %>%
                  dplyr::select(tidyselect::all_of(c(df_col_row$col_name, df_by_row$col_name))) %>%
                  dplyr::filter(dplyr::if_all(.cols = tidyselect::everything(), .fns = ~!is.na(.x)))
                count_uniques <- dplyr::count(df_chitest, .data[[df_col_row$col_name]], .data[[df_col_row$col_name]], name = ".n_count")

                if(dplyr::n_distinct(df_chitest[[df_col_row$col_name]]) > 1 &&
                   dplyr::n_distinct(df_chitest[[df_by_row$col_name]]) > 1 &&
                   all(count_uniques$.n_count >= 10)) {

                  df_by_row$chi_p <-
                    stats::chisq.test(x=df_chitest[[df_col_row$col_name]],
                                      y=df_chitest[[df_by_row$col_name]])$p.value %>%
                    suppressWarnings()


                  return(df_by_row)
                }
              }
              df_by_row$chi_p <- NA_real_
              return(df_by_row)

            }) %>%
            dplyr::bind_rows()

          df_col_row$by_cols_df[[1]] <-
            vctrs::vec_slice(out_by,
                             (!is.na(out_by$chi_p) &
                               out_by$chi_p <= hide_bi_entry_if_sig_above) |
                               out_by$col_name %in% always_show_bi_for_by,
                             error_call = call)

          df_col_row

        }) %>%
        dplyr::bind_rows()

      out
    } else refined_data_overview
  }


#' Processes A 'data_overview' Data Frame
#'
#' @param data_overview A data frame containing the columns:
#' @param data Optional full survey data set for which columns can be looked up.
#' @param group_by Character vector of colnames used for identifying chapters and sections.
#' @param sort_by Character vector of of colnames used to order pieces within each group_by combination.
#' @param label_separator Optional string for separating label between main question and sub-item.
#' @param name_separator Optional string for separating column name between main question and sub-item.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' refine_data_overview(ex_survey_ch_overview)
#' refine_data_overview(ex_survey_ch_overview,
#'                      data = ex_survey1,
#'                      label_separator = " - ",
#'                      name_separator = "_")
refine_data_overview <-
  function(data_overview,
           data = NULL,
           label_separator = NULL,
           name_separator = NULL,
           ...,
           call = rlang::caller_env()) {

    cli::cli_progress_message(msg = "Refining data_overview...")

    dots <- rlang::list2(...)

  col_headers <- c("dep_int", "dep_cat", "dep_txt",
                   "indep_int", "indep_cat", "indep_txt")
  data_present <- !is.null(data) && is.data.frame(data)
  out <-
    data_overview %>%
    tidyr::pivot_longer(cols = tidyselect::any_of(col_headers), values_to = "col_spec") %>%
    tidyr::separate_longer_delim(cols = "col_spec",
                                 delim = stringr::regex(",|[[:space:]]+")) %>%
    tidyr::separate(col = .data$name, into = c("designated_role", "designated_type"), sep="_") %>%
    dplyr::mutate(col_spec = dplyr::if_else(!stringi::stri_detect(.data$col_spec, regex = "matches\\(") &
                                              stringi::stri_detect(.data$col_spec, regex = "\\*"),
                                            true = stringr::str_c("matches('", .data$col_spec, "')"),
                                            false = .data$col_spec)) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    dplyr::filter(!.data$col_spec == "" & !is.na(.data$col_spec)) %>%
    dplyr::arrange(.data$designated_role, .data$designated_type) %>%
    dplyr::relocate(tidyselect::all_of(c("designated_role", "designated_type", "col_spec")))


  if(data_present) {
    out <-
    out %>%
    dplyr::mutate(cols = eval_cols(x = .data$col_spec,
                                 data = .env$data,
                                 call = call)) %>%
     tidyr::unnest_longer(col = "cols", values_to = "col_pos", indices_to = "col_name") %>%
     dplyr::left_join(y=look_for_extended(data = data,
                                          label_separator = label_separator,
                                          name_separator = name_separator),
                      by = dplyr::join_by("col_pos", "col_name")) %>%
      dplyr::mutate(label_prefix = stringr::str_trim(.data$label_prefix),
                    label_prefix = stringr::str_replace_all(.data$label_prefix, pattern = "[[:space:]]+", replacement = " "),
                    label_suffix = stringr::str_trim(.data$label_suffix),
                    label_suffix = stringr::str_replace_all(.data$label_suffix, pattern = "[[:space:]]+", replacement = " ")) %>%
      attach_indep() %>%
      remove_non_significant_bivariates(data = data,
                                        hide_bi_entry_if_sig_above = dots$hide_bi_entry_if_sig_above,
                                        always_show_bi_for_by = dots$always_show_bi_for_by) %>%
      dplyr::group_by(dplyr::pick(tidyselect::all_of(dots$group_by))) %>%
      dplyr::arrange(dplyr::pick(tidyselect::all_of(names(dots$sort_by))))
  }
  out
}
