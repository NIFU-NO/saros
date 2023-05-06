gen_qmd_structure <-
  function(data_overview,
           data,
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


        if(nrow(data_overview_section)>0) {
        new_out <-
          purrr::map_chr(
            .x = seq_along(element_names), .f = ~{


              qmd_snippet <-
              gen_element_and_qmd_snippet(
                data_overview = data_overview_section,
                element_name = element_names[.x],
                data = data,
                summarized_data = summarized_data,
                grouping_structure = grouping_structure,
                element_folderpath_absolute = fs::path(chapter_folderpath_absolute, element_names[.x]),
                element_folderpath_relative = fs::path(chapter_foldername, element_names[.x]),
                translations = translations,
                !!!dots,
                call = call)



                if(!rlang::is_null(qmd_snippet) && !is.na(qmd_snippet) && !stringi::stri_isempty(qmd_snippet)) {
                  stringr::str_c("\n\n", qmd_snippet)
                } else ""
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


