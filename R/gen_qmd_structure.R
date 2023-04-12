gen_qmd_structure <-
  function(data_overview,
           # grouping_structure = NULL,
           elements,
           glue_index_string = NULL,
           show_if_alpha_below = 1,
           path,
           captions = "asis",
           call = rlang::caller_env()) {


  gen_group_structure <- function(grouped_data,
                                  level = 1,
                                  grouping_structure,
                                  level_values = character()) {
    output <- ""
    if (level > length(grouped_data)) {
      return(output)
    }

    for (value in unique(grouped_data[[level]])) {
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
          purrr::map_chr(
            .x = seq_along(elements), .f= ~{
              if(captions %in% c("asis", "pretty")) {
                caption <-
                  names(elements)[.x]
              } else caption <- ""
              if(captions == "pretty") {
                caption <-
                  caption %>%
                stringr::str_replace_all(pattern = "_", replacement = " ") %>%
                stringr::str_replace_all(pattern = "cat", replacement = "Categorical") %>%
                stringr::str_replace_all(pattern = "int", replacement = "Interval/Continous") %>%
                stringr::str_replace_all(pattern = "uni", replacement = "Univariate") %>%
                  stringr::str_replace_all(pattern = "bi", replacement = "Bivariate") %>%
                stringr::str_replace_all(pattern = "plot", replacement = "Plot") %>%
                  stringr::str_replace_all(pattern = "table", replacement = "Table") %>%
                stringr::str_replace_all(pattern = "html|docx", replacement = "")
              }
              content <-
                get_element_path(
                  data_overview = data_overview_section,
                  elements = elements[.x],
                  glue_index_string = glue_index_string,
                  show_if_alpha_below = show_if_alpha_below,
                  path = path,
                  call = call)
              if(!rlang::is_null(content)) {
                stringr::str_c(caption, "\n\n", content)
              }

            }) %>%
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


