#' @keywords internal
gen_qmd_structure2 <-
  function(chapter_overview,
           data,
           mesos_group = NULL,
           chapter_folderpath_absolute,
           chapter_foldername,
           ...,
           call = rlang::caller_env()) {


    dots <- update_dots(dots = rlang::list2(...),
                        allow_unique_overrides = FALSE)




    gen_group_structure2 <- function(grouped_data,
                                    level = 1,
                                    grouping_structure) {
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
        # Create heading, section tag and random ID if not a .element_name or chapter
        if(!names(grouped_data)[level] %in% c(".element_name", ".variable_type_dep", "chapter") &&
           level < ncol(grouped_data)) {

          output <- stringi::stri_remove_empty_na(output)
          heading_line <- stringi::stri_remove_empty_na(heading_line)
          output <-
            stringi::stri_c(output,
                            heading_line,
                            sep="\n",
                            ignore_null=TRUE)
        }

        # Keep only relevant part of meta data


        sub_df <- vctrs::vec_slice(grouped_data,
                                   is.na(as.character(grouped_data[[colnames(grouped_data)[level]]])) |
                                   as.character(grouped_data[[colnames(grouped_data)[level]]]) == value)

        sub_df <- droplevels(sub_df)


        # Setting a specific sub-chapter (e.g. a label_prefix) as the column name. WHY?
        names(grouping_structure)[level] <- value

        # If innermost/deepest level, start producing contents
        if(level == length(grouping_structure)) {

          # Create new metadata with bare minimum needed, and reapply grouping
          chapter_overview_section <- chapter_overview

          for(i in seq_along(grouping_structure)) {
            if(!is.na(names(grouping_structure)[i])) {
              chapter_overview_section <-
                vctrs::vec_slice(chapter_overview_section,
                                 as.character(chapter_overview_section[[grouping_structure[[i]]]]) ==
                                   names(grouping_structure)[i])
            } else {
              chapter_overview_section <-
                vctrs::vec_slice(chapter_overview_section,
                                 is.na(as.character(chapter_overview_section[[grouping_structure[[i]]]])))

            }
          }
          # if(all(chapter_overview$chapter != "Introduction")) browser()

          chapter_overview_section <- droplevels(chapter_overview_section)

          if(nrow(chapter_overview_section) >= 1) {


            chapter_overview_section <-
              dplyr::group_by(chapter_overview_section,
                              dplyr::pick(tidyselect::all_of(unname(grouping_structure))))
            chapter_overview_section <- droplevels(chapter_overview_section)


            new_out <-
              dplyr::group_map(chapter_overview_section,
                               .keep = TRUE,
                               .f = ~{
                                 # browser()

                                 .x <- dplyr::group_by(.x,
                                                       dplyr::pick(tidyselect::all_of(unname(grouping_structure))))
                                 .y$.element_name <- as.character(.y$.element_name)
                                 if(is.na(!(all(stringi::stri_detect_fixed(str = .y$.element_name, pattern = "chr")) &&
                                                  rlang::is_true(dots$hide_chr_for_others) &&
                                                  rlang::is_string(mesos_group)))) browser()
                                 # if(all(.x$chapter == "Ambivalence") &&
                                 #    all(.x$.element_name == "bi_catcat_prop_plot") &&
                                 #    any(.x$.variable_name_dep == "a_1")) browser()


                                 qmd_snippet <- NULL
                                 qmd_snippet_mesos <- NULL



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

                                   if(nrow(data_for_all) > 0) {


                                     qmd_snippet <-
                                       rlang::exec(
                                         gen_element_and_qmd_snippet2,
                                         chapter_overview_section = droplevels(.x),
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

                                   if(nrow(data_for_mesos) > 0) {

                                     qmd_snippet_mesos <-
                                       rlang::exec(
                                         gen_element_and_qmd_snippet2,
                                         chapter_overview_section = droplevels(.x),
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
                               })

            output <- attach_new_output_to_output(new_out = new_out,
                                                  output = output,
                                                  level = level,
                                                  grouped_data = grouped_data,
                                                  heading_line = heading_line)

          }
        }


        added <-
          gen_group_structure2(grouped_data = sub_df,
                               level = level + 1,
                               grouping_structure = grouping_structure)
        added <- stringi::stri_remove_empty_na(added)
        output <- # Recursive call
          stringi::stri_c(output,
                          added,
                          sep="\n\n", ignore_null=TRUE) # Space between each section (before new heading)
      }

      output <- if(!is.na(output)) output else ""

      return(output)
    }

    chapter_overview <- chapter_overview
    if(rlang::is_string(mesos_group)) {
      unique_vars <- unique(as.character(chapter_overview$.variable_name_dep))
      unique_vars <- unique_vars[!is.na(unique_vars)]
      for(var in unique_vars) {
        tmp <- vctrs::vec_slice(data, !is.na(data[[dots$mesos_var]]) &
                                  data[[dots$mesos_var]] == mesos_group)
        tmp <- tmp[[var]]
        if(all(is.na(tmp)) || length(tmp)==0) {
          cli::cli_inform("In mesos_group {mesos_group}, removing empty column {var}.")
          chapter_overview <- vctrs::vec_slice(chapter_overview,
                                               as.character(chapter_overview[[".variable_name_dep"]]) != var)
        }
      }
    }

    grouping_structure <- dplyr::group_vars(chapter_overview)
    non_grouping_vars <- colnames(chapter_overview)[!colnames(chapter_overview) %in% grouping_structure]

    grouped_data <- chapter_overview
    grouped_data <-dplyr::group_by(grouped_data, dplyr::pick(tidyselect::all_of(grouping_structure)))
    grouped_data <- dplyr::distinct(grouped_data, dplyr::pick(tidyselect::all_of(grouping_structure)))

    out <-
    gen_group_structure2(grouped_data = grouped_data,
                        grouping_structure = grouping_structure)
    out
  }


