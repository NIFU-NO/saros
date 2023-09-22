
eval_cols <- function(x, data,
                      call = rlang::caller_env()) {
  check_string(x = x, n = NULL, null.ok = FALSE, call = call)
  check_data_frame(data, call = call)
  lapply(x, function(col_entry) {
      if(stringi::stri_length(col_entry)>0) {
        expr <- stringi::stri_c('tidyselect::eval_select(expr = rlang::expr(c(',
                            col_entry,
                            ')), data = data)',
                            ignore_null=TRUE)
        out <- rlang::try_fetch(eval(parse(text = expr)),
                 error = function(e) cli::cli_abort("Column {.var {col_entry}} doesn't exist in data.",
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
    .variable_position = match(colnames(data_part), colnames(data)),
    .variable_name = colnames(data_part),
    .variable_label = get_raw_labels(data = data_part),
    .variable_type = as.character(unlist(lapply(names(data_part), function(.x) vctrs::vec_ptype_abbr(data_part[[.x]])))),
    row.names = NULL
  )
  check_duplicates <- duplicated(x$.variable_label)
  if(any(check_duplicates)) {
    duplicates <- unique(x$.variable_label[check_duplicates])
    cli::cli_warn(c("Found duplicated variable labels: {duplicates}.",
                    "This will likely cause problems!"))
  }

  if(!is.null(name_separator)) {
    if(rlang::is_character(name_separator, n = 1)) {
      x <-
        tidyr::separate_wider_delim(x,
                                    cols = ".variable_name",
                                    delim = name_separator,
                                    names = c(".variable_name_prefix", ".variable_name_suffix"),
                                    cols_remove = FALSE,
                                    too_few = "align_end", too_many = "merge")
      if(sum(stringi::stri_count_fixed(str = x$.variable_name_suffix, pattern = " - "), na.rm=TRUE) > 0) {
        cli::cli_warn(c("{.arg name_separator} matches more than one delimiter, your output is likely ugly.",
                      i="Consider renaming your variables with e.g. {.fun dplyr::rename_with()}."))
      }

    } else cli::cli_abort("Non-string {.arg name_separator} currently not supported.")
  } else x <- dplyr::mutate(x,
                            .variable_name_prefix = .data$.variable_name,
                            .variable_name_suffix = .data$.variable_name)

  if(!is.null(label_separator)) {
    if(rlang::is_character(label_separator, n = 1)) {
      x <-
        tidyr::separate_wider_delim(x,
                                    cols = ".variable_label",
                                    names = c(".variable_label_prefix", ".variable_label_suffix"),
                                    delim = label_separator,
                                    too_few = "align_end", too_many = "merge")
      if(sum(stringi::stri_count_fixed(str = x$.variable_label_suffix, pattern = " - "), na.rm=TRUE) > 0) {
        cli::cli_warn(c("{.arg label_separator} matches more than one delimiter, your output is likely ugly.",
                        i="Consider renaming your variable labels with e.g. {.fun labelled::set_variable_labels}."))
      }

    }  else cli::cli_abort("Non-string {.arg label_separator} currently not supported.")
  } else {
    x$.variable_label_prefix <- x$.variable_label
    x$.variable_label_suffix <- x$.variable_label
    x$.variable_label <- NULL
  }
  grouping_vars <-
    c(if(!is.null(label_separator)) ".variable_label_prefix",
      if(!is.null(name_separator)) ".variable_name_prefix")

  x %>%
    dplyr::mutate(

      .variable_name_prefix = dplyr::if_else(
        is.na(.data$.variable_name_prefix) & !is.na(.data$.variable_name_suffix),
        .data$.variable_name_suffix,
        .data$.variable_name_prefix),


      .variable_name_suffix = dplyr::if_else(
        is.na(.data$.variable_name_suffix) & !is.na(.data$.variable_name_prefix),
        .data$.variable_name_prefix,
        .data$.variable_name_suffix),

      .variable_label_prefix = dplyr::if_else(
        is.na(.data$.variable_label_prefix) & !is.na(.data$.variable_label_suffix),
        .data$.variable_label_suffix,
        .data$.variable_label_prefix),

      .variable_label_suffix = dplyr::if_else(
        is.na(.data$.variable_label_suffix) & !is.na(.data$.variable_label_prefix),
        .data$.variable_label_prefix,
        .data$.variable_label_suffix),

    ) %>%
    dplyr::relocate(tidyselect::any_of(c(".variable_position", ".variable_name", ".variable_name_prefix", ".variable_name_suffix",
                    ".variable_label_prefix", ".variable_label_suffix", ".variable_type"))) %>%
    dplyr::mutate(.variable_group_id = dplyr::cur_group_id(),
                  .by = tidyselect::all_of(if(length(grouping_vars)>0) grouping_vars else ".variable_position"))

  ### Return a grouped data frame with
  ### main question variable name prefix,
  ### main question variable label (prefix),
  ### subquestion variable name suffix,
  ### subquestion variable label (suffix)
  ### var_group,
  ### .variable_type,
  ### .variable_role, designated_type, uni_bi_variate,
}
#

validate_labels <- function(data) {
  miss_label_vars <- data[is.na(data$.variable_label_prefix), ]
  if(nrow(miss_label_vars) > 0) cli::cli_warn("Using variable name in place of missing label for {.var {miss_label_vars$.variable_name}}.")
  # if(data$.variable_label)
  data$.variable_label_prefix <- dplyr::if_else(!is.na(data$.variable_label_prefix), data$.variable_label_prefix, data$.variable_name)
  data$.variable_label_suffix <- dplyr::if_else(!is.na(data$.variable_label_suffix), data$.variable_label_suffix, data$.variable_name)
  data
}


attach_indep <- function(refined_chapter_overview) {
  if(!rlang::is_null(refined_chapter_overview$.variable_role)) {

    indep_df <- refined_chapter_overview
    indep_df <- dplyr::ungroup(indep_df)
    indep_df <- dplyr::filter(indep_df, .data$.variable_role == "indep")
    indep_df <- tidyr::nest(indep_df, .by = tidyselect::all_of("chapter"),
                  .key = "indep_cols_df")

    dplyr::left_join(x = refined_chapter_overview,
                     y = indep_df,
                     by = dplyr::join_by("chapter"))

  } else {
    cli::cli_warn("No column {.var .variable_role} found, no bivariates possible.")
    dplyr::mutate(refined_chapter_overview, indep_cols_df = list(NULL))
  }
}


find_test <- function(y, x) {

  chisq_test2 <- function(...) stats::chisq.test(table(...))

  if((inherits(y, what = "double") ||
      inherits(y, what = "integer")) &&
     (inherits(x, what = "double") ||
      inherits(x, what = "integer"))) return(stats::cor.test)

  if(inherits(y, what = "factor") &&
     inherits(x, what = "factor")) return(chisq_test2)

  if((inherits(y, what = "double") ||
      inherits(y, what = "integer")) &&
     inherits(x, what = "factor")) return(stats::t.test)

}

remove_non_significant_bivariates <-
  function(refined_chapter_overview,
           data,
           hide_bi_entry_if_sig_above = .05,
           always_show_bi_for_indep = c(),
           progress = TRUE,
           call = rlang::caller_env()) {

    check_double(hide_bi_entry_if_sig_above, min = 0, max = 1, call = call)
    check_string(always_show_bi_for_indep, null.ok = TRUE, n = NULL, call = call)

    if(hide_bi_entry_if_sig_above < 1) {
      if(progress) cli::cli_progress_message("Removing bivariate occurences if {.arg hide_bi_entry_if_sig_above}: {.arg {hide_bi_entry_if_sig_above}}, except {always_show_bi_for_indep}.")

      out <- refined_chapter_overview
      out <- dplyr::rowwise(out)
      out <- dplyr::group_map(out, .keep = TRUE, .f = function(df_col_row, df_col_key) {

          if(rlang::is_null(df_col_row$indep_cols_df[[1]]) || nrow(df_col_row$indep_cols_df[[1]]) == 0) {

            df_col_row

          } else {

          out_indep <-
            df_col_row$indep_cols_df[[1]] %>% ### COULD ALSO lapply(1:nrow(df_col_row$indep_cols_df[[1]])) %>% bind_rows()
            dplyr::rowwise() %>%
            dplyr::group_map(.keep = TRUE, .f = function(df_indep_row, indep_df_key) {

              if(df_indep_row$.variable_name != df_col_row$.variable_name) {

                df_chitest <-
                  data[!is.na(data[[df_col_row$.variable_name]]) & !is.na(data[[df_indep_row$.variable_name]]),
                       c(df_col_row$.variable_name, df_indep_row$.variable_name)]

                count_uniques <- dplyr::count(df_chitest,
                                              .data[[df_col_row$.variable_name]],
                                              .data[[df_indep_row$.variable_name]],
                                              name = ".n_count")

                if(dplyr::n_distinct(df_chitest[[df_col_row$.variable_name]]) > 1 &&
                   dplyr::n_distinct(df_chitest[[df_indep_row$.variable_name]]) > 1 &&
                   all(count_uniques$.n_count >= 10)) {



                  stattest <- find_test(y = df_chitest[[df_col_row$.variable_name]],
                                        x = df_chitest[[df_indep_row$.variable_name]])

                  df_indep_row$chi_p <-
                    stattest(x=df_chitest[[df_col_row$.variable_name]],
                             y=df_chitest[[df_indep_row$.variable_name]])$p.value %>%
                    suppressWarnings()


                  return(df_indep_row)
                }
              }
              df_indep_row$chi_p <- NA_real_
              return(df_indep_row)

            }) %>%
            dplyr::bind_rows()

          df_col_row$indep_cols_df[[1]] <-
            vctrs::vec_slice(out_indep,
                             (!is.na(out_indep$chi_p) &
                               out_indep$chi_p <= hide_bi_entry_if_sig_above) |
                               out_indep$.variable_name %in% always_show_bi_for_indep,
                             error_call = call)
          df_col_row
          }


        }) %>%
        dplyr::bind_rows()

      out
    } else refined_chapter_overview
  }


add_element_names <- function(refined_chapter_overview, element_names) {
  refined_chapter_overview <- dplyr::group_map(refined_chapter_overview,
                                               .f = ~tidyr::crossing(.x, .element_name = element_names))
  refined_chapter_overview <- dplyr::bind_rows(refined_chapter_overview)
  refined_chapter_overview
}

#' Processes A 'chapter_overview' Data Frame
#'
#' @inheritParams draft_report
#' @inheritParams gen_qmd_chapters
#' @param progress *Whether to display progress message*
#'
#'    `scalar<logical>` // *default:* `TRUE`
#'
#' @return Data frame
#' @export
#'
#' @examples
#' ref_df <- refine_chapter_overview(chapter_overview = ex_survey_ch_overview)
#' ref_df2 <- refine_chapter_overview(chapter_overview = ex_survey_ch_overview,
#'                      data = ex_survey1)
refine_chapter_overview <-
  function(chapter_overview = NULL,
           data = NULL,
           ...,
           progress = TRUE,
           call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        allow_unique_overrides = FALSE)

    if(progress) cli::cli_progress_message(msg = "Refining chapter_overview...\n")


  col_headers <- c("dep", "indep")
  if(!rlang::is_null(chapter_overview) &&
     !any(colnames(chapter_overview) == "dep")) {
    cli::cli_abort("{.arg chapter_overview} must contain columns `dep` (and optionally `indep`).")
  } else {
    if(rlang::is_null(chapter_overview)) {
      chapter_overview <- data.frame(chapter = "All", dep = "everything()")
    }
  }
  data_present <- !is.null(data) && is.data.frame(data)

  delim_regex <- ",|[[:space:]]+"
  attr(delim_regex, "options") <-
    list(case_insensitive = FALSE,
         comments = FALSE,
         dotall = FALSE,
         multiline = FALSE)
  class(delim_regex) <- c("stringr_regex", "stringr_pattern", "character")

  out <-
    tidyr::pivot_longer(chapter_overview,
                        cols = tidyselect::any_of(col_headers),
                        values_to = ".variable_selection")
  out <-
    tidyr::separate_longer_delim(out,
                                 cols = ".variable_selection",
                                 delim = delim_regex)
  out <-
    tidyr::separate(out,
                    col = .data$name,
                    into = ".variable_role",
                    sep="_")
  out <-
    dplyr::mutate(out,
                  .variable_selection =
                    dplyr::if_else(!stringi::stri_detect(.data$.variable_selection,
                                                         regex = "matches\\(") &
                                     stringi::stri_detect(.data$.variable_selection,
                                                          regex = "\\*"),
                                   true = stringi::stri_c(ignore_null=TRUE,
                                                          "matches('",
                                                          .data$.variable_selection,
                                                          "')"),
                                   false = .data$.variable_selection))
  out <-
    dplyr::distinct(out, .keep_all = TRUE)
  out <-
    dplyr::filter(out, !.data$.variable_selection == "" & !is.na(.data$.variable_selection))
  out <-
    dplyr::arrange(out, .data$.variable_role)
  out <-
    dplyr::relocate(out, tidyselect::all_of(c(".variable_role", ".variable_selection")))


  if(data_present) {
    out$.variable_selection <-
      stringi::stri_replace_all_fixed(out$.variable_selection,
                                      pattern = '\"',
                                      replacement = "'")
    out$.variable_selection <-
      stringi::stri_replace_all_regex(out$.variable_selection,
                                      pattern = '[[:space:]]+',
                                      replacement = "")

    out$cols <- eval_cols(x = out$.variable_selection,
                          data = data,
                          call = call)
    out <-
      tidyr::unnest_longer(out,
                           col = "cols",
                           values_to = ".variable_position",
                           indices_to = ".variable_name")
    # check_duplicates_in_chapter_overview(out)
    out <-
     dplyr::left_join(x=out,
                      y=look_for_extended(data = data,
                                          label_separator = dots$label_separator,
                                          name_separator = dots$name_separator),
                      by = dplyr::join_by(".variable_position", ".variable_name"))
    out <- # Move to separate function, and add argument that defaults to TRUE
      dplyr::mutate(out,
                    .variable_label_prefix = stringi::stri_trim_both(.data$.variable_label_prefix),
                    .variable_label_prefix = stringi::stri_replace_all_regex(.data$.variable_label_prefix, pattern = "[[:space:]]+", replacement = " "),
                    .variable_label_suffix = stringi::stri_trim_both(.data$.variable_label_suffix),
                    .variable_label_suffix = stringi::stri_replace_all_regex(.data$.variable_label_suffix, pattern = "[[:space:]]+", replacement = " "))
    out <-
      validate_labels(out)
    out <-
      attach_indep(out)
    out <-
      remove_non_significant_bivariates(out,
                                        data = data,
                                        hide_bi_entry_if_sig_above = dots$hide_bi_entry_if_sig_above,
                                        always_show_bi_for_indep = dots$always_show_bi_for_indep,
                                        progress = progress)
    out <-
      tidyr::expand_grid(out, .element_name = dots$element_names)
    out <-
      dplyr::group_by(out, dplyr::pick(tidyselect::all_of(dots$organize_by)))
    # out <-
    #   dplyr::arrange(out, dplyr::pick(tidyselect::any_of(c(dots$organize_by, names(dots$sort_by)))))
  }
  if(!rlang::is_null(out$chapter)) out$chapter <- factor(out$chapter, levels=unique(out$chapter))
  out
}
