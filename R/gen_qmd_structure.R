gen_qmd_structure <-
  function(data_overview,
           # grouping_structure = NULL,
           elements,
           glue_index_string = NULL,
           ignore_if_below = 0,
           path,
           call = rlang::caller_env()) {


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
        stringr::str_dup(string = "#", times = level) %>%
        stringr::str_c(output, ., " ", value, "\n")

      sub_df <- dplyr::filter(grouped_data, .data[[colnames(grouped_data)[level]]] == value)
      names(grouping_structure)[level] <- value

      if (level == length(grouping_structure)) {

        data_overview_section <-
          data_overview

        for(i in seq_along(grouping_structure)) {
          data_overview_section <-
            data_overview_section %>%
            dplyr::filter(.data[[grouping_structure[i]]] == names(grouping_structure)[i])
        }


        data_overview_section <-
          data_overview_section %>%
          dplyr::group_by(dplyr::pick(tidyselect::all_of(unname(grouping_structure))))


        if(nrow(data_overview_section)>0) {
        output <-
          purrr::map(
            .x = seq_along(elements), .f= ~{

              content <-
                get_element_path(
                  data_overview = data_overview_section,
                  elements = elements[.x],
                  glue_index_string = glue_index_string,
                  ignore_if_below = ignore_if_below,
                  path = path,
                  call = call)

              if(!rlang::is_null(content)) {
                stringr::str_c("\n\n", content)
              }

            }) %>%
          .[lengths(.)>0] %>%
          stringr::str_c(collapse = "\n\n") %>% # Space between elements
          stringr::str_c(output, ., sep = "\n") # Space between heading and first element
        }
      }

      output <-
        gen_group_structure(grouped_data = sub_df,
                            level = level + 1,
                            grouping_structure = grouping_structure,
                            level_values = level_values) %>%
        stringr::str_c(output, ., sep="\n\n") # Space between each section (before new heading)
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


