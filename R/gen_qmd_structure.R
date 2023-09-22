#' @keywords internal
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


  gen_group_structure <- function(grouped_data,
                                  level = 1,
                                  grouping_structure,
                                  level_values = character()) {
    output <- ""

    if (level > ncol(grouped_data)) {
      return(output)
    }


    for(value in unique(grouped_data[[level]])) {

      heading_line <- add_section_heading_line(
                                         grouped_data = grouped_data,
                                         level = level,
                                         chapter_overview = chapter_overview,
                                         value = value)
      # Create heading, section tag and random ID if not a .element_name
      if(names(grouped_data)[level] != ".element_name" &&
         level < ncol(grouped_data)) {

        output <-
          stringi::stri_c(output,
                        heading_line,
                        ignore_null=TRUE)
      }

      # Keep only relevant part of meta data
      sub_df <- dplyr::filter(grouped_data, .data[[colnames(grouped_data)[level]]] == value)

      # Setting a specific sub-chapter (e.g. a label_prefix) as the column name. WHY?
      names(grouping_structure)[level] <- value

    # If innermost/deepest level, start producing contents
      if(level == length(grouping_structure)) {

        # Create new metadata with bare minimum needed, and reapply grouping
        chapter_overview_section <-
          chapter_overview
        for(i in seq_along(grouping_structure)) {
          chapter_overview_section <-
            dplyr::filter(chapter_overview_section,
                          .data[[grouping_structure[i]]] == names(grouping_structure)[i])
        }


        if(nrow(chapter_overview_section) >= 1) {

          chapter_overview_section <-
            dplyr::group_by(chapter_overview_section,
                            dplyr::pick(tidyselect::all_of(unname(grouping_structure))))


          new_out <-
            dplyr::group_map(chapter_overview_section,
                             .keep = TRUE,
                             .f = ~{

                .x <- dplyr::group_by(.x,
                                      dplyr::pick(tidyselect::all_of(unname(grouping_structure))))
                .y$.element_name <- as.character(.y$.element_name)

                qmd_snippet <- NULL
                qmd_snippet_mesos <- NULL



                # If not a text-based element with hiding of other mesos_groups,
                # produce the default "everyone (else)" data
                if(!(stringi::stri_detect(str = .y$.element_name, fixed = "chr") &&
                     dots$hide_chr_for_others &&
                     rlang::is_string(mesos_group))) {



                  if(rlang::is_true(dots$mesos_report) &&
                     rlang::is_string(dots$mesos_var) &&
                     rlang::is_string(mesos_group)) {

                    data_for_all <-
                      data[data[[dots$mesos_var]] != mesos_group, ]

                  } else {
                    data_for_all <- data
                  }

                  if(nrow(data_for_all) > 0) {


                    qmd_snippet <-
                      rlang::exec(
                      gen_element_and_qmd_snippet,
                        chapter_overview_section = .x,
                        data = data_for_all,
                        mesos_group = if(rlang::is_string(dots$mesos_var)) dots$translations$mesos_label_all_others,
                        element_name = .y$.element_name,
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

                  data_for_mesos <- data[data[[dots$mesos_var]] == mesos_group, ]

                  if(nrow(data_for_mesos) > 0) {
                    qmd_snippet_mesos <-
                      rlang::exec(
                      gen_element_and_qmd_snippet,
                        chapter_overview_section = .x,
                        data = data_for_mesos,
                        mesos_var = dots$mesos_var,
                        mesos_group = mesos_group,
                        element_name = .y$.element_name,
                        grouping_structure = grouping_structure,
                        chapter_folderpath_absolute = chapter_folderpath_absolute,
                        chapter_foldername = chapter_foldername,
                        !!!dots
                      )
                  }


                }


                insert_qmd_tablet_mesos_order(element_name = .y$.element_name,
                                              qmd_snippet = qmd_snippet,
                                              qmd_snippet_mesos = qmd_snippet_mesos,
                                              mesos_report = dots$mesos_report,
                                              mesos_var = dots$mesos_var,
                                              mesos_group = mesos_group,
                                              panel_tabset_mesos = dots$panel_tabset_mesos,
                                              mesos_first = dots$mesos_first,
                                              translations = dots$translations)
              })


          output <- attach_new_output_to_output(new_out = new_out,
                                                output = output,
                                                level = level,
                                                grouped_data = grouped_data,
                                                heading_line = heading_line)

        }
      }

      output <- # Recursive call
        stringi::stri_c(output,
                        gen_group_structure(grouped_data = sub_df,
                                            level = level + 1,
                                            grouping_structure = grouping_structure,
                                            level_values = level_values),
                        sep="\n\n", ignore_null=TRUE) # Space between each section (before new heading)
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


