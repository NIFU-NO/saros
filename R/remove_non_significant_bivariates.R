
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

    if(hide_bi_entry_if_sig_above < 1) {
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


              if(!is.na(df_col_row$.variable_name) &&
                 df_indep_row$.variable_name != df_col_row$.variable_name) {

                df_chitest <-
                  data[!is.na(data[[df_col_row$.variable_name]]) & !is.na(data[[df_indep_row$.variable_name]]),
                       c(df_col_row$.variable_name, df_indep_row$.variable_name)]

                count_uniques <- dplyr::count(df_chitest,
                                              .data[[df_col_row$.variable_name]],
                                              .data[[df_indep_row$.variable_name]],
                                              name = ".n_count")

                if(dplyr::n_distinct(df_chitest[[df_col_row$.variable_name]]) > 1 &&
                   dplyr::n_distinct(df_chitest[[df_indep_row$.variable_name]]) > 1 &&
                   all(count_uniques$.n_count >= hide_test_if_n_below) &&
                   !any(df_col_row$.variable_type == "chr") &&
                   !any(df_indep_row$.variable_type == "chr")) {



                  stattest <- find_test(y = df_chitest[[df_col_row$.variable_name]],
                                        x = df_chitest[[df_indep_row$.variable_name]])

                  df_indep_row$chi_p <-
                    stattest(x = df_chitest[[df_col_row$.variable_name]],
                             y = df_chitest[[df_indep_row$.variable_name]])$p.value %>%
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
