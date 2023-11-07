
remove_non_significant_bivariates <-
  function(refined_chapter_overview,
           data,
           hide_bi_entry_if_sig_above = .05,
           always_show_bi_for_indep = c(),
           hide_test_if_n_below = 10,
           progress = TRUE,
           call = rlang::caller_env()) {

    check_double(hide_bi_entry_if_sig_above, min = 0, max = 1, call = call)
    check_string(always_show_bi_for_indep, null.ok = TRUE, n = NULL, call = call)

    if(!is.na(hide_bi_entry_if_sig_above) && hide_bi_entry_if_sig_above < 1) {
      if(progress) cli::cli_progress_message("Removing bivariate occurences if {.arg hide_bi_entry_if_sig_above}: {.arg {hide_bi_entry_if_sig_above}}, except {.arg {always_show_bi_for_indep}}.")

      out <- refined_chapter_overview
      out <- dplyr::rowwise(out)
      out <- dplyr::group_map(out, .keep = TRUE, .f = function(df_col_row, df_col_key) {


        if(rlang::is_null(df_col_row$indep_cols_df[[1]]) || nrow(df_col_row$indep_cols_df[[1]]) == 0) {

          df_col_row

        } else {

          # View(out[3, "indep_cols_df"])
          # stop()
          out_indep <-
            df_col_row$indep_cols_df[[1]] %>% ### COULD ALSO lapply(1:nrow(df_col_row$indep_cols_df[[1]])) %>% bind_rows()
            dplyr::rowwise() %>%
            dplyr::group_map(.keep = TRUE, .f = function(df_indep_row, indep_df_key) {

              df_indep_row$.keep_indep <- FALSE
              df_indep_row$chi_p <- NA_real_

              if(!is.na(df_col_row$.variable_name) &&
                 !is.na(df_indep_row$.variable_name) &&
                 df_indep_row$.variable_name != df_col_row$.variable_name) {

                df_chitest <-
                  data[!is.na(data[[df_col_row$.variable_name]]) & !is.na(data[[df_indep_row$.variable_name]]),
                       c(df_col_row$.variable_name, df_indep_row$.variable_name)]



                count_uniques <- dplyr::count(df_chitest,
                                              .data[[df_col_row$.variable_name]],
                                              .data[[df_indep_row$.variable_name]],
                                              name = ".n_count")

                stattest <- find_test(y = df_chitest[[df_col_row$.variable_name]],
                                      x = df_chitest[[df_indep_row$.variable_name]])

                df_indep_row$.keep_indep <-
                  dplyr::n_distinct(df_chitest[[df_col_row$.variable_name]], na.rm = TRUE) > 1 &&
                  dplyr::n_distinct(df_chitest[[df_indep_row$.variable_name]], na.rm = TRUE) > 1 &&
                  !any(df_col_row$.variable_type == "chr") &&
                  !any(df_indep_row$.variable_type == "chr") &&
                  rlang::is_function(stattest)

                if(df_indep_row$.keep_indep) { # &&
                  # all(count_uniques$.n_count >= 1)


                  df_indep_row$chi_p <-
                    suppressWarnings(
                    stattest(x = df_chitest[[df_col_row$.variable_name]],
                             y = df_chitest[[df_indep_row$.variable_name]])$p.value
                    )
                }
              }

              return(df_indep_row)

            }) %>%
            dplyr::bind_rows()

          if(nrow(out_indep)==0 || length(out_indep$.keep_indep)==0 ||
             length(out_indep$.variable_name) == 0 ||
             length(out_indep$chi_p) == 0) browser()

          df_col_row$indep_cols_df[[1]] <-
            vctrs::vec_slice(out_indep,
                             (!is.na(out_indep$chi_p) &
                                out_indep$chi_p <= hide_bi_entry_if_sig_above) |
                               (out_indep$.variable_name %in% always_show_bi_for_indep &
                                  out_indep$.keep_indep),
                             error_call = call)
          df_col_row
        }


      })
      out <- dplyr::bind_rows(out)

      out
    } else refined_chapter_overview
  }



remove_non_significant_bivariates2 <-
  function(refined_chapter_overview,
           data,
           hide_bi_entry_if_sig_above = .05,
           always_show_bi_for_indep = c(),
           hide_test_if_n_below = 10,
           progress = TRUE,
           call = rlang::caller_env()) {

    check_data_frame(refined_chapter_overview)
    check_data_frame(data)
    check_double(hide_bi_entry_if_sig_above, min = 0, max = 1, call = call)
    check_string(always_show_bi_for_indep, null.ok = TRUE, n = NULL, call = call)
    check_integerish(hide_test_if_n_below, min=0, max=Inf, call = call)

    refined_chapter_overview$.bi_test <- NA_character_
    refined_chapter_overview$.p_value <- NA_real_

    if(is.null(hide_bi_entry_if_sig_above) ||
       is.na(hide_bi_entry_if_sig_above) ||
       hide_bi_entry_if_sig_above == 1) {

      refined_chapter_overview

    } else {

      if(progress) cli::cli_progress_message("Removing bivariate occurences if {.arg hide_bi_entry_if_sig_above}: {.arg {hide_bi_entry_if_sig_above}}, except {.arg {always_show_bi_for_indep}}.")

      out_keep_anyway <-
        vctrs::vec_slice(refined_chapter_overview, is.na(refined_chapter_overview[[".variable_name_indep"]]))

      out_bivariates <-
        vctrs::vec_slice(refined_chapter_overview,
                      !is.na(refined_chapter_overview[[".variable_name_dep"]]) &
                        !is.na(refined_chapter_overview[[".variable_name_indep"]]) &
                        refined_chapter_overview[[".variable_name_dep"]] !=
                        refined_chapter_overview[[".variable_name_indep"]])

      if(nrow(out_bivariates) > 0) {

      out_bivariates <- dplyr::rowwise(out_bivariates)
      out_bivariates <- dplyr::group_map(out_bivariates, .keep = TRUE,
                              .f = function(df_col_row, df_col_key) {


                                df_col_row$.keep_indep <- FALSE
                                df_col_row$chi_p <- NA_real_

                                if(is.null(df_col_row$.variable_name_dep) ||
                                   length(df_col_row$.variable_name_dep) == 0 ||
                                   is.na(df_col_row$.variable_name_dep)) browser()

              df_chitest <-
                  data[!is.na(data[[df_col_row$.variable_name_dep]]) &
                         !is.na(data[[df_col_row$.variable_name_indep]]),
                       c(df_col_row$.variable_name_dep, df_col_row$.variable_name_indep)]

              if(nrow(df_chitest) < 1) return()


                count_uniques <- dplyr::count(df_chitest,
                                              .data[[df_col_row$.variable_name_dep]],
                                              .data[[df_col_row$.variable_name_indep]],
                                              name = ".n_count")


                df_col_row$.keep_indep <-
                  dplyr::n_distinct(df_chitest[[df_col_row$.variable_name_dep]], na.rm = TRUE) > 1 &&
                  dplyr::n_distinct(df_chitest[[df_col_row$.variable_name_indep]], na.rm = TRUE) > 1 &&
                  df_col_row$.variable_type_dep != "chr" &&
                  df_col_row$.variable_type_indep != "chr"

                if(df_col_row$.keep_indep) {

                  stat_result <-
                    find_test2(y = df_chitest[[df_col_row$.variable_name_dep]],
                               x = df_chitest[[df_col_row$.variable_name_indep]])
                  df_col_row$.bi_test <- stat_result$.bi_test
                  df_col_row$.p_value <- stat_result$.p_value

                }
                df_col_row
              })
      out_bivariates <- dplyr::bind_rows(out_bivariates)
      out_bivariates <- vctrs::vec_slice(out_bivariates,
                                         (!is.na(out_bivariates$.p_value) &
                                            out_bivariates$.p_value <= hide_bi_entry_if_sig_above) |
                                           (out_bivariates$.variable_name_indep %in% always_show_bi_for_indep &
                                              out_bivariates$.keep_indep),
                                         error_call = call)
      }
      dplyr::bind_rows(out_keep_anyway,
                       out_bivariates)
    }
  }
