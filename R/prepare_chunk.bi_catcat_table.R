prepare_chunk.bi_catcat_table <-
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
      lapply(X = seq_along(name_indep),
             FUN = function(i) {
               .x <- name_indep[[i]]
               indep_pos <- match(.x, data_cols)
               .y <- names(name_indep)[[i]]
               # print(name_indep[i])

               # If dep and indep are the same, or empty, return early.
               if(is.null(y_col_pos) || is.null(.x) || any(y_col_pos == indep_pos)) return()



               indep_type <- vctrs::vec_slice(indep_df, indep_df$.variable_name == .x)
               indep_type <- indep_type$.variable_type

               if(!all(chapter_overview_section$.variable_type %in% c("fct", "ord")) ||
                  !all(indep_type %in% c("fct", "ord"))) return()

               filename_prefix <- stringi::stri_c(filename_prefix, "BY", .x, ignore_null=TRUE, sep = "_")
               filepaths <- make_filenames_list(element_folderpath_relative = element_folderpath_relative,
                                                element_folderpath_absolute = element_folderpath_absolute,
                                                filename_prefix = filename_prefix)

               common_data_type <- get_common_data_type(data, col_pos = y_col_pos)
               common_levels <- get_common_levels(data, col_pos = y_col_pos)

               out <-
                 rlang::exec(
                   embed_cat_table,
                   data = data,
                   dep = y_col_pos,
                   indep = indep_pos,
                   mesos_group = mesos_group,
                   !!!dots)
               writexl::write_xlsx(x=out, path = filepaths$abs$xlsx)
               qs::qsave(out, file = filepaths$abs$rds)



               out <-
                 insert_obj_in_qmd(element_name = "bi_catcat_table",
                                   index = filename_prefix,
                                   mesos_group = mesos_group,
                                   filepath_txt = filepaths$abs$rds,
                                   filepath = filepaths$rel$rds,
                                   max_width_obj = dots$max_width_obj,
                                   max_width_file = dots$max_width_file,
                                   translations = dots$translations,
                                   caption = attr(out, "saros_caption"))


             })
    out <- unlist(out)
    stringi::stri_c(out, ignore_null=TRUE, collapse = "\n")
  }
