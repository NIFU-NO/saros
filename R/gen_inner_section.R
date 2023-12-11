gen_inner_section <- function(.x, .y,
                              data,
                              grouping_structure,
                              dots,
                              mesos_group,
                              chapter_folderpath_absolute,
                              chapter_foldername
                              ) {

  if(all(!is.na(.x$chapter)) &&
     all(is.na(as.character(.x$.element_name)))) return()

  if((all(!is.na(.x$chapter)) && nrow(.x) > 1 &&
     all(is.na(.x$.variable_name_dep))) ||
     any(is.na(as.character(.y$.element_name)))) browser()

    .x <- dplyr::group_by(.x,
                          dplyr::pick(tidyselect::all_of(unname(grouping_structure))))
    .y$.element_name <- as.character(.y$.element_name)
    # if(all(!is.na(.y$.element_name)) &&
    #    all(stringi::stri_detect_fixed(str = .y$.element_name, pattern = "chr", negate = TRUE)) &&
    #            rlang::is_true(dots$hide_chr_for_others) &&
    #            rlang::is_string(mesos_group)) browser()
    # if(all(.x$chapter == "Ambivalence") &&
    #    all(.x$.element_name == "bi_catcat_prop_plot") &&
    #    any(.x$.variable_name_dep == "a_1")) browser()


    qmd_snippet <- NULL
    qmd_snippet_mesos <- NULL

    .x <- droplevels(.x)


    # If not a text-based element with hiding of other mesos_groups,
    # produce the default "everyone (else)" data
    if(!(all(stringi::stri_detect_fixed(str = .y$.element_name, pattern = "chr")) &&
         rlang::is_true(dots$hide_chr_for_others) &&
         rlang::is_string(mesos_group))) {



      if(rlang::is_true(dots$mesos_report) &&
         rlang::is_string(dots$mesos_var) &&
         rlang::is_string(mesos_group)) {

        data_for_all <-
          vctrs::vec_slice(data, data[[dots$mesos_var]] != mesos_group)

      } else {
        data_for_all <- data
      }

      if(nrow(data_for_all) > dots$hide_result_if_n_below) {


        qmd_snippet <-
          rlang::exec(
            gen_element_and_qmd_snippet2,
            chapter_overview_section = .x,
            data = data_for_all,
            mesos_group = if(rlang::is_string(dots$mesos_var)) dots$translations$mesos_label_all_others,
            grouping_structure = grouping_structure,
            chapter_folderpath_absolute = chapter_folderpath_absolute,
            chapter_foldername = chapter_foldername,
            !!!dots
          )
      }
    }

    if(rlang::is_true(dots$mesos_report) &&
       rlang::is_string(dots$mesos_var) &&
       rlang::is_string(mesos_group)) {

      data_for_mesos <- vctrs::vec_slice(data,
                                         !is.na(data[[dots$mesos_var]]) &
                                           data[[dots$mesos_var]] == mesos_group)
      if(!inherits(data_for_mesos, "data.frame")) browser()

      if(nrow(data_for_mesos) > dots$hide_result_if_n_below ||
         all(stringi::stri_detect_fixed(str = .y$.element_name, pattern = "chr"))) {

        qmd_snippet_mesos <-
          rlang::exec(
            gen_element_and_qmd_snippet2,
            chapter_overview_section = .x,
            data = data_for_mesos,
            mesos_var = dots$mesos_var,
            mesos_group = mesos_group,
            grouping_structure = grouping_structure,
            chapter_folderpath_absolute = chapter_folderpath_absolute,
            chapter_foldername = chapter_foldername,
            !!!dots
          )
      }


    }


    insert_qmd_tablet_mesos_order(element_name = unique(.y$.element_name),
                                  qmd_snippet = qmd_snippet,
                                  qmd_snippet_mesos = qmd_snippet_mesos,
                                  mesos_report = dots$mesos_report,
                                  mesos_var = dots$mesos_var,
                                  mesos_group = mesos_group,
                                  panel_tabset_mesos = dots$panel_tabset_mesos,
                                  mesos_first = dots$mesos_first,
                                  translations = dots$translations)
}
