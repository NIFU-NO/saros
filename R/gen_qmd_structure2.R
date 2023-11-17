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
                                    grouping_structure,
                                    chapter_overview,
                                    data,
                                    dots) {
      output <- ""


      if (level > ncol(grouped_data)) {
        return(output)
      }


      for(value in unique(as.character(grouped_data[[level]]))) {


        # Create heading, section tag and random ID if not a .element_name or chapter
        output <- add_section_heading_line(
          output = output,
          grouped_data = grouped_data,
          level = level,
          chapter_overview = chapter_overview,
          value = value,
          added = NULL,
          ignore_heading_for_group = dots$ignore_heading_for_group)




        # Keep only relevant part of meta data, e.g. a specific chapter

        sub_df <- vctrs::vec_slice(grouped_data,
                                   is.na(as.character(grouped_data[[level]])) |
                                   as.character(grouped_data[[level]]) == value)

        sub_df <- droplevels(sub_df)


        # Setting a specific sub-chapter (e.g. a label_prefix) as the name of the grouping vector. WHY?
        names(grouping_structure)[level] <- value

        # If innermost/deepest level, start producing contents
        if(level == length(grouping_structure)) {


          # Create new metadata with bare minimum needed, and reapply grouping
          # WHY NOT USED sub_df?!?
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
            if(all(is.na(as.character(chapter_overview_section$chapter)))) browser()

            new_out <-
              dplyr::group_map(chapter_overview_section,
                               .keep = TRUE,
                               .f = get_inner_section(
                                 .x, .y,
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
                               grouping_structure = grouping_structure,
                               chapter_overview = chapter_overview,
                               data = data,
                               dots = dots)

        output <-
          stringi::stri_c(output,
                          added,
                          sep="\n\n", ignore_null=TRUE) # Space between each section (before new heading)
      }

      if(length(output)>1 || is.na(output)) browser()
      # output <- if(!is.na(output)) output else ""

      return(output)
    }



    chapter_overview <-
      remove_empty_col_for_mesos_group(
      data = data,
      chapter_overview = chapter_overview,
      mesos_group = mesos_group,
      mesos_var = dots$mesos_var
    )

    grouping_structure <- dplyr::group_vars(chapter_overview)
    non_grouping_vars <- colnames(chapter_overview)[!colnames(chapter_overview) %in% grouping_structure]

    grouped_data <- chapter_overview
    grouped_data <-dplyr::group_by(grouped_data, dplyr::pick(tidyselect::all_of(grouping_structure)))
    grouped_data <- dplyr::distinct(grouped_data, dplyr::pick(tidyselect::all_of(grouping_structure)))

    out <-
    gen_group_structure2(grouped_data = grouped_data,
                        grouping_structure = grouping_structure,
                        chapter_overview = chapter_overview,
                        data = data,
                        dots = dots)
    out
  }


