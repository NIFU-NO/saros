prepare_chunk.bi_sigtest <-
  function(chapter_overview_section,
           data,
           y_col_pos,
           mesos_group,
           filepaths,
           obj_name,
           variable_prefix,
           element_folderpath_relative,
           element_folderpath_absolute,
           filename_prefix,
           ...) {

    dots <- rlang::list2()

    data_cols <- if(inherits(data, "survey")) colnames(data$variables) else colnames(data)


    if(any(chapter_overview_section$.variable_role == "indep") ||
       rlang::is_null(chapter_overview_section$indep_cols_df) ||
       rlang::is_null(chapter_overview_section$indep_cols_df[[1]]) ||
       nrow(chapter_overview_section$indep_cols_df[[1]]) == 0 ||
       !compare_many(chapter_overview_section$indep_cols_df) ||
       !inherits(chapter_overview_section$indep_cols_df[[1]], what = "data.frame")) return()

    indep_df <- chapter_overview_section$indep_cols_df[[1]]


    name_indep <-
      stats::setNames(unique(indep_df$.variable_name),
                      nm = stringi::stri_c(obj_name, "_BY_", unique(indep_df$.variable_name), ignore_null=TRUE))


    filename_prefix <- stringi::stri_c(filename_prefix, "_BY_ALL_INDEP", ignore_null=TRUE)
    filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                     element_folderpath_absolute = element_folderpath_absolute,
                                     filename_prefix = filename_prefix)


    out <-
      lapply(X = seq_along(name_indep), FUN = function(i) {
        .x <- name_indep[[i]]
        indep_pos <- match(.x, data_cols)
        .y <- names(name_indep)[[i]]

        # Early check whether x and y are the same, which saros cannot handle
        if(is.null(y_col_pos) || is.null(.x) || any(y_col_pos == indep_pos)) return(data.frame())


        indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
        indep_type <- indep_type$.variable_type

        ##############################################################################
        if(dplyr::n_distinct(chapter_overview_section$.variable_type) == 1 &&
           dplyr::n_distinct(indep_type) == 1) {

          rlang::exec(
            embed_bi_sigtest,
            data = data,
            dep = y_col_pos,
            indep = indep_pos,
            .variable_type = unique(chapter_overview_section$.variable_type),
            indep_type = unique(indep_type),
            mesos_group = mesos_group,
            !!!dots)
        }
      })

    out <- dplyr::bind_rows(out)

    if(nrow(out)>0) {
      writexl::write_xlsx(x=out, path = filepaths$abs$xlsx)
      qs::qsave(out, file = filepaths$abs$rds)
      return(
        insert_obj_in_qmd(element_name = "bi_sigtest",
                          index = filename_prefix,
                          mesos_group = mesos_group,
                          filepath_txt = filepaths$abs$rds,
                          filepath = filepaths$rel$rds,
                          max_width_obj = dots$max_width_obj,
                          max_width_file = dots$max_width_file,
                          translations = dots$translations,
                          caption = attr(out, "saros_caption")))
    }
  }
