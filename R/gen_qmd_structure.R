gen_qmd_structure <-
  function(chapter_overview,
           data,
           mesos_group = NULL,
           chapter_folderpath_absolute,
           chapter_foldername,
           ...,
           call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        allow_unique_overrides = FALSE)
    # dots <- rlang::list2(...)
    # dots <- utils::modifyList(x = formals(draft_report)[!names(formals(draft_report)) %in% c("data", "chapter_overview")],
    #                           val = dots[!names(dots) %in% c("...")], keep.null = TRUE)


  gen_group_structure <- function(grouped_data,
                                  level = 1,
                                  grouping_structure,
                                  level_values = character()) {
    output <- ""

    if (level > ncol(grouped_data)) {
      return(output)
    }


    for(value in unique(grouped_data[[level]])) {

      if(names(grouped_data)[level] == ".variable_name") {
        heading <- chapter_overview[chapter_overview[[".variable_name"]] == value,
                                    ".variable_label_suffix"][[1]]
      } else heading <- value


      output <-
        stringi::stri_c(ignore_null=TRUE,
                        output,
                       strrep("#", times = level), " ", heading,
                       "{#sec-", conv_to_valid_obj_name(value), "_",
                       stringi::stri_c(ignore_null=TRUE, sample(0:9, size=3, replace = TRUE), collapse=""),
                       "}\n")

      sub_df <- dplyr::filter(grouped_data, .data[[colnames(grouped_data)[level]]] == value)
      names(grouping_structure)[level] <- value

      if (level == length(grouping_structure)) {

        chapter_overview_section <-
          chapter_overview

        for(i in seq_along(grouping_structure)) {
          chapter_overview_section <-
            dplyr::filter(chapter_overview_section,
                          .data[[grouping_structure[i]]] == names(grouping_structure)[i])
        }


        chapter_overview_section <-
          dplyr::group_by(chapter_overview_section,
                          dplyr::pick(tidyselect::all_of(unname(grouping_structure))))


        if(nrow(chapter_overview_section) > 1 && rlang::is_true(dots$single_y_bivariate_elements)) {

          # chapter_overview_section <-

          new_out <-
            unlist(lapply(
              X = seq_along(dots$element_names),
              FUN = function(.x) {


                qmd_snippet <- NULL
                if(!(stringi::stri_detect(str = dots$element_names[.x], fixed = "chr") &&
                     dots$hide_chr_for_others &&
                     rlang::is_string(mesos_group))) {

                  data_for_all <-
                    if(rlang::is_true(dots$mesos_report) &&
                       rlang::is_string(dots$mesos_var) &&
                       rlang::is_string(mesos_group)) data[data[[dots$mesos_var]] != mesos_group, ] else data

                  if(nrow(data_for_all) > 0) {

                    qmd_snippet <-
                      gen_element_and_qmd_snippet(
                        chapter_overview_section = chapter_overview_section,
                        data = data_for_all,
                        mesos_var = NULL,
                        mesos_group = if(rlang::is_string(dots$mesos_var)) dots$translations$mesos_label_all_others,
                        element_name = dots$element_names[.x],
                        grouping_structure = grouping_structure,
                        element_folderpath_absolute = fs::path(chapter_folderpath_absolute, dots$element_names[.x]),
                        element_folderpath_relative = fs::path(chapter_foldername, dots$element_names[.x]),
                        # translations = translations,
                        !!!dots#[!names(dots) %in% c("chapter_overview", "mesos_var", "data", "call")]
                        )
                  }
                }

                if(rlang::is_true(dots$mesos_report) &&
                   rlang::is_string(dots$mesos_var) &&
                   rlang::is_string(mesos_group)) {

                  qmd_snippet_mesos <-
                    gen_element_and_qmd_snippet(
                      chapter_overview_section = chapter_overview_section,
                      data = data[data[[dots$mesos_var]] == mesos_group, ],
                      # mesos_var = dots$mesos_var,
                      mesos_group = mesos_group,
                      element_name = dots$element_names[.x],
                      grouping_structure = grouping_structure,
                      element_folderpath_absolute = fs::path(chapter_folderpath_absolute, dots$element_names[.x]),
                      element_folderpath_relative = fs::path(chapter_foldername, dots$element_names[.x]),
                      # translations = translations,
                      !!!dots#[!names(dots) %in% c("chapter_overview", "data", "call")]
                      )
                }
                insert_qmd_tablet_mesos_order(element_name = dots$element_names[.x],
                                                 qmd_snippet = qmd_snippet,
                                                 qmd_snippet_mesos = qmd_snippet_mesos,
                                              mesos_report = dots$mesos_report,
                                                 mesos_var = dots$mesos_var,
                                                 mesos_group = mesos_group,
                                                 panel_tabset_mesos = dots$panel_tabset_mesos,
                                                 mesos_first = dots$mesos_first,
                                                 translations = dots$translations)
              }))


          new_out <- stringi::stri_c(ignore_null=TRUE, new_out[!stringi::stri_isempty(new_out)],
                                    collapse = "\n\n") # Space between elements

          output <- stringi::stri_c(ignore_null=TRUE, output, new_out, sep = "\n") # Space between heading and first element


        } else if(nrow(chapter_overview_section) > 0) {
          new_out <-
            unlist(lapply(
              X = seq_along(dots$element_names),
              FUN = function(.x) {


                qmd_snippet <- NULL
                if(!(stringi::stri_detect(str = dots$element_names[.x], fixed = "chr") &&
                     dots$hide_chr_for_others &&
                     rlang::is_string(mesos_group))) {

                  data_for_all <-
                    if(rlang::is_true(dots$mesos_report) &&
                       rlang::is_string(dots$mesos_var) &&
                       rlang::is_string(mesos_group)) data[data[[dots$mesos_var]] != mesos_group, ] else data

                  if(nrow(data_for_all) > 0) {

                    qmd_snippet <-
                      gen_element_and_qmd_snippet(
                        chapter_overview_section = chapter_overview_section,
                        data = data_for_all,
                        mesos_var = NULL,
                        mesos_group = if(rlang::is_true(dots$mesos_report) &&
                                         rlang::is_string(dots$mesos_var)) dots$translations$mesos_label_all_others,
                        element_name = dots$element_names[.x],
                        grouping_structure = grouping_structure,
                        element_folderpath_absolute = fs::path(chapter_folderpath_absolute, dots$element_names[.x]),
                        element_folderpath_relative = fs::path(chapter_foldername, dots$element_names[.x]),
                        # translations = dots$translations,
                        !!!dots#[!names(dots) %in% c("chapter_overview", "mesos_var", "mesos_group", "data", "call")]
                        )
                  }
                }

                if(rlang::is_true(dots$mesos_report) &&
                   rlang::is_string(dots$mesos_var) &&
                   rlang::is_string(mesos_group)) {

                  qmd_snippet_mesos <-
                    gen_element_and_qmd_snippet(
                      chapter_overview_section = chapter_overview_section,
                      data = data[data[[dots$mesos_var]] == mesos_group, ],
                      # mesos_var = dots$mesos_var,
                      mesos_group = mesos_group,
                      element_name = dots$element_names[.x],
                      grouping_structure = grouping_structure,
                      element_folderpath_absolute = fs::path(chapter_folderpath_absolute, dots$element_names[.x]),
                      element_folderpath_relative = fs::path(chapter_foldername, dots$element_names[.x]),
                      # translations = dots$translations,
                      !!!dots#[!names(dots) %in% c("chapter_overview", "data", "call")]
                      )
                }
                insert_qmd_tablet_mesos_order(element_name = dots$element_names[.x],
                                                 qmd_snippet = qmd_snippet,
                                                 qmd_snippet_mesos = qmd_snippet_mesos,
                                              mesos_report = dots$mesos_report,
                                                 mesos_var = dots$mesos_var,
                                                 mesos_group = mesos_group,
                                                 panel_tabset_mesos = dots$panel_tabset_mesos,
                                                 mesos_first = dots$mesos_first,
                                                 translations = dots$translations)
              }))


          new_out <- stringi::stri_c(ignore_null=TRUE, new_out[!stringi::stri_isempty(new_out)],
                                    collapse = "\n\n") # Space between elements

          output <- stringi::stri_c(ignore_null=TRUE, output, new_out, sep = "\n") # Space between heading and first element

        }
      }

      output <-
        stringi::stri_c(ignore_null=TRUE,  output,
                        gen_group_structure(grouped_data = sub_df,
                                            level = level + 1,
                                            grouping_structure = grouping_structure,
                                            level_values = level_values),
                        sep="\n\n") # Space between each section (before new heading)
    }

    return(output)
  }



  grouping_structure <- dplyr::group_vars(chapter_overview)
  non_grouping_vars <- colnames(chapter_overview)[!colnames(chapter_overview) %in% grouping_structure]

  grouped_data <-
    chapter_overview %>%
    dplyr::group_by(dplyr::pick(tidyselect::all_of(grouping_structure))) %>%
    dplyr::distinct(dplyr::pick(tidyselect::all_of(grouping_structure)))

  gen_group_structure(grouped_data = grouped_data,
                      grouping_structure = grouping_structure)
}


