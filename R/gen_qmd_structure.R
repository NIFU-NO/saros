gen_qmd_structure <-
  function(data_overview,
           data,
           tailored_var = NULL,
           tailored_group = NULL,
           element_names = list_available_element_types(),
           chapter_folderpath_absolute,
           chapter_foldername,
           translations = .saros.env$defaults$translations,
           summarized_data = NULL,
           ...,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)


  gen_group_structure <- function(grouped_data,
                                  level = 1,
                                  grouping_structure,
                                  level_values = character()) {
    output <- ""

    if (level > length(grouped_data)) {
      return(output)
    }

    for(value in unique(grouped_data[[level]])) {
      output <-
        stringr::str_c(output, strrep("#", times = level), " ", value,
                       "{#sec-", conv_to_valid_obj_name(value), "_",
                       stringr::str_c(sample(0:9, size=3, replace = TRUE), collapse=""), "}\n")

      sub_df <- dplyr::filter(grouped_data, .data[[colnames(grouped_data)[level]]] == value)
      names(grouping_structure)[level] <- value

      if (level == length(grouping_structure)) {

        data_overview_section <-
          data_overview

        for(i in seq_along(grouping_structure)) {
          data_overview_section <-
            dplyr::filter(data_overview_section,
                          .data[[grouping_structure[i]]] == names(grouping_structure)[i])
        }


        data_overview_section <-
          dplyr::group_by(data_overview_section,
                          dplyr::pick(tidyselect::all_of(unname(grouping_structure))))


        if(nrow(data_overview_section) > 1 && rlang::is_true(dots$single_y_bivariate_elements)) {

          data_overview_section <-

          new_out <-
            purrr::map_chr(
              .x = seq_along(element_names), .f = ~{


                qmd_snippet <- NULL
                if(!(stringi::stri_detect(str = element_names[.x], fixed = "chr") &&
                     dots$hide_chr_for_others &&
                     rlang::is_string(tailored_group))) {

                  data_for_all <-
                    if(rlang::is_string(tailored_var) &&
                       rlang::is_string(tailored_group)) data[data[[tailored_var]] != tailored_group, ] else data

                  if(nrow(data_for_all) > 0) {

                    qmd_snippet <-
                      gen_element_and_qmd_snippet(
                        data_overview = data_overview_section,
                        data = data_for_all,
                        tailored_var = NULL,
                        tailored_group = if(rlang::is_string(tailored_var)) translations$tailored_label_all_others,
                        element_name = element_names[.x],
                        summarized_data = summarized_data,
                        grouping_structure = grouping_structure,
                        element_folderpath_absolute = fs::path(chapter_folderpath_absolute, element_names[.x]),
                        element_folderpath_relative = fs::path(chapter_foldername, element_names[.x]),
                        translations = translations,
                        !!!dots,
                        call = call)
                  }
                }

                if(rlang::is_string(tailored_var) && rlang::is_string(tailored_group)) {

                  qmd_snippet_tailored <-
                    gen_element_and_qmd_snippet(
                      data_overview = data_overview_section,
                      data = data[data[[tailored_var]] == tailored_group, ],
                      tailored_var = tailored_var,
                      tailored_group = tailored_group,
                      element_name = element_names[.x],
                      summarized_data = summarized_data,
                      grouping_structure = grouping_structure,
                      element_folderpath_absolute = fs::path(chapter_folderpath_absolute, element_names[.x]),
                      element_folderpath_relative = fs::path(chapter_foldername, element_names[.x]),
                      translations = translations,
                      !!!dots,
                      call = call)
                }
                insert_qmd_tablet_tailored_order(element_name = element_names[.x],
                                                 qmd_snippet = qmd_snippet,
                                                 qmd_snippet_tailored = qmd_snippet_tailored,
                                                 tailored_var = tailored_var,
                                                 tailored_group = tailored_group,
                                                 panel_tabset_tailored = dots$panel_tabset_tailored,
                                                 tailored_first = dots$tailored_first,
                                                 translations = translations)
              })


          new_out <- stringr::str_c(new_out[!stringi::stri_isempty(new_out)],
                                    collapse = "\n\n") # Space between elements

          output <- stringr::str_c(output, new_out, sep = "\n") # Space between heading and first element


        } else if(nrow(data_overview_section) > 0) {
          new_out <-
            purrr::map_chr(
              .x = seq_along(element_names), .f = ~{


                qmd_snippet <- NULL
                if(!(stringi::stri_detect(str = element_names[.x], fixed = "chr") &&
                     dots$hide_chr_for_others &&
                     rlang::is_string(tailored_group))) {

                  data_for_all <-
                    if(rlang::is_string(tailored_var) &&
                       rlang::is_string(tailored_group)) data[data[[tailored_var]] != tailored_group, ] else data

                  if(nrow(data_for_all) > 0) {

                    qmd_snippet <-
                      gen_element_and_qmd_snippet(
                        data_overview = data_overview_section,
                        data = data_for_all,
                        tailored_var = NULL,
                        tailored_group = if(rlang::is_string(tailored_var)) translations$tailored_label_all_others,
                        element_name = element_names[.x],
                        summarized_data = summarized_data,
                        grouping_structure = grouping_structure,
                        element_folderpath_absolute = fs::path(chapter_folderpath_absolute, element_names[.x]),
                        element_folderpath_relative = fs::path(chapter_foldername, element_names[.x]),
                        translations = translations,
                        !!!dots,
                        call = call)
                  }
                }

                if(rlang::is_string(tailored_var) && rlang::is_string(tailored_group)) {

                  qmd_snippet_tailored <-
                    gen_element_and_qmd_snippet(
                      data_overview = data_overview_section,
                      data = data[data[[tailored_var]] == tailored_group, ],
                      tailored_var = tailored_var,
                      tailored_group = tailored_group,
                      element_name = element_names[.x],
                      summarized_data = summarized_data,
                      grouping_structure = grouping_structure,
                      element_folderpath_absolute = fs::path(chapter_folderpath_absolute, element_names[.x]),
                      element_folderpath_relative = fs::path(chapter_foldername, element_names[.x]),
                      translations = translations,
                      !!!dots,
                      call = call)
                }
                insert_qmd_tablet_tailored_order(element_name = element_names[.x],
                                                 qmd_snippet = qmd_snippet,
                                                 qmd_snippet_tailored = qmd_snippet_tailored,
                                                 tailored_var = tailored_var,
                                                 tailored_group = tailored_group,
                                                 panel_tabset_tailored = dots$panel_tabset_tailored,
                                                 tailored_first = dots$tailored_first,
                                                 translations = translations)
              })


          new_out <- stringr::str_c(new_out[!stringi::stri_isempty(new_out)],
                                    collapse = "\n\n") # Space between elements

          output <- stringr::str_c(output, new_out, sep = "\n") # Space between heading and first element

        }
      }

      output <-
        stringr::str_c( output,
                        gen_group_structure(grouped_data = sub_df,
                                            level = level + 1,
                                            grouping_structure = grouping_structure,
                                            level_values = level_values),
                        sep="\n\n") # Space between each section (before new heading)
    }

    return(output)
  }



  grouping_structure <- dplyr::group_vars(data_overview)
  non_grouping_vars <- colnames(data_overview)[!colnames(data_overview) %in% grouping_structure]

  grouped_data <-
    data_overview %>%
    dplyr::group_by(dplyr::pick(tidyselect::all_of(grouping_structure))) %>%
    dplyr::distinct(dplyr::pick(tidyselect::all_of(grouping_structure)))

  gen_group_structure(grouped_data = grouped_data,
                      grouping_structure = grouping_structure)
}


