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

        # Add heading line if not a .element_name or chapter
        if(!names(grouped_data)[level] %in% dots$ignore_heading_for_group &&
           level < ncol(grouped_data)) {

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

          if(all(is.na(chapter_overview_section$chapter)) && nrow(chapter_overview_section)>1) browser()

          for(i in seq_along(grouping_structure)) {
            variable <- as.character(chapter_overview_section[[grouping_structure[[i]]]])
            lgl_filter <-
              (!is.na(names(grouping_structure)[i]) & !is.na(variable) & variable == names(grouping_structure)[i]) |
              (is.na(names(grouping_structure)[i]) & is.na(variable))

            chapter_overview_section <-
              vctrs::vec_slice(chapter_overview_section, lgl_filter)
          }
          if(all(is.na(chapter_overview_section$.variable_name_dep)) &&
             nrow(chapter_overview_section) > 1) browser()

          chapter_overview_section <- droplevels(chapter_overview_section)

          if(nrow(chapter_overview_section) >= 1) {


            chapter_overview_section <-
              dplyr::group_by(chapter_overview_section,
                              dplyr::pick(tidyselect::all_of(unname(grouping_structure))))
            chapter_overview_section <- droplevels(chapter_overview_section)


            new_out <-
              dplyr::group_map(chapter_overview_section,
                               .keep = TRUE,
                               .f = ~gen_inner_section(
                                 .x=.x, .y=.y,
                                 data = data,
                                 grouping_structure = grouping_structure,
                                 dots = dots,
                                 mesos_group = mesos_group,
                                 chapter_folderpath_absolute = chapter_folderpath_absolute,
                                 chapter_foldername = chapter_foldername
                               ))

            output <- attach_new_output_to_output(new_out = new_out,
                                                  output = output,
                                                  level = level,
                                                  grouped_data = grouped_data,
                                                  heading_line = heading_line)

          }
        }


        added <- # Recursive call
          gen_group_structure2(grouped_data = sub_df,
                               level = level + 1,
                               grouping_structure = grouping_structure)
        added <- stringi::stri_remove_empty_na(added)
        output <-
          stringi::stri_c(output,
                          added,
                          sep="\n\n", ignore_null=TRUE) # Space between each section (before new heading)
      }

      if(length(output)>1 || (length(output)==1 && is.na(output))) browser()

      return(output)
    }

    chapter_overview <- remove_empty_col_for_mesos_group(data = data,
                                                         chapter_overview = chapter_overview,
                                                         mesos_group = mesos_group,
                                                         mesos_var = dots$mesos_var,
                                                         hide_result_if_n_below = dots$hide_result_if_n_below)

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
