


#' Generate Quarto Section
#'
#' This function generates a Quarto section containing text, or references to plots and tables, based on the given data and element configurations.
#'
#' @param data_overview_section A data.frame providing an overview of all columns considered part of a section.
#' @param glue_index_string A glue string for indicating the index in data_overview_section that matches the names in the elements list.
#' @param elements A named list of elements to be included in the Quarto section.
#' @param grouping_structure A named character vector of the groups in the data_overview_section. Names correspond to number in original hierarchy, and thus heading level.
#' @param ignore_if_below A double between 0 and 1, indicating the threshold for showing an element. If 0 (default), shows everything.
#' @param path A string specifying the path to the main folder. If not provided, the function uses the current working directory.
#' @param call Internal call argument. Not to be fiddled with by the user.
#' @return A single string containing Quarto section code.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(glue)
#' test_path <- tempdir()
#'
#' data_overview <-
#'   refine_data_overview(
#'               data_overview = ex_survey_ch_overview,
#'               data = ex_survey1,
#'               label_separator = " - ",
#'               name_separator = "_")
#'
#' data_overview_section <-
#'   data_overview %>%
#'   filter(chapter == "2 Ambivalence",
#'          designated_role == "dep",
#'          col_group == 3)
#'
#' elements_list <-
#'    list(header_text =
#'            lst_saros_elements(
#'                    data = ex_survey1,
#'                    data_overview = data_overview_section,
#'                    element_name = "header_text"),
#'         uni_cat_plot_html =
#'            lst_saros_elements(
#'                    data = ex_survey1,
#'                    data_overview = data_overview_section,
#'                    element_name = "uni_cat_plot_html"))
#' gen_qmd_section(
#'   data_overview_section = data_overview_section,
#'   elements = elements_list,
#'   path = test_path)
#'
#' unlink(test_path)

gen_qmd_section <-
  function(data_overview_section,
           elements = NULL,
           glue_index_string = NULL,

           grouping_structure = NULL,

           ignore_if_below = 0,
           path = NULL,
           call = rlang::caller_env()) {



    # heading <- create_heading(x = grouping_structure)

    # if(dplyr::n_distinct(data_overview_section$designated_role) > 1) {
    #   cli::cli_abort("Multiple designated_role values detected when a single was expected.",
    #                  call = call)
    # }
    # if(dplyr::n_distinct(data_overview_section$designated_type) > 1) {
    #   cli::cli_abort("Multiple designated_type values detected when a single was expected.",
    #                  call = call)
    # }
    # if(dplyr::n_distinct(data_overview_section$label_prefix) > 1) {
    #   cli::cli_abort("Multiple label_prefix values detected when a single was expected.",
    #                  call = call)
    # }



    # purrr::map_chr(
    #   .x = seq_along(elements), .f= ~{
    #
    #       get_element_path(
    #         data_overview_section = data_overview_section,
    #         element = elements[.x],
    #         glue_index_string = glue_index_string,
    #
    #         path = path,
    #         call = call)
    #
    #   }) %>%
    #   stringr::str_c(collapse = "\n") %>%
    #   stringr::str_c(heading, ., sep="\n\n")
  }

