#' Generate Quarto Chapter
#'
#' This function generates a Quarto chapter containing sections based on the provided data_overview, element configurations, and grouping structure.
#'
#' @inheritParams gen_qmd_report
#' @param grouping_structure A string or a character vector specifying the column names to be used for grouping sections in the chapter.
#' @param chapter_yml ymlthis-object. Will obtain defaults from getOption("ymlthis.default_options") if NULL.
#' @param call Internal call argument. Not to be fiddled with by the user.
#'
#' @return A string containing the filepath to the generated Quarto chapter file.
gen_qmd_chapter <-
  function(data_overview,
           elements = NULL,

           grouping_structure = NULL, # Computed in gen_qmd_report
           glue_index_string = NULL,

           ignore_if_below = 0,
           chapter_yml = NULL,
           path = getwd(),
           call = rlang::caller_env()) {


    check_data_frame(data_overview, call = call)

    check_string(grouping_structure, n = NULL, null.ok = TRUE, call = call)
    if(rlang::is_null(grouping_structure) || length(grouping_structure) == 0) {
      grouping_structure <-
        data_overview %>%
        dplyr::group_vars() %>%
        rlang::set_names(nm = seq_along(.))
    } else {
      check_string(names(grouping_structure), n = length(grouping_structure), null.ok = FALSE, call = call)
      # if(dplyr::n_distinct(data_overview[[grouping_structure[1]]]) > 1) {
      #   cli::cli_abort("Must be only one group in the first grouping level ({.var {data_overview[[grouping_structure[1]]]}}",
      #                  i="but found {dplyr::n_distinct(data_overview[[grouping_structure[1]]])}.",
      #                  call = call)
      # }
    }
    check_elements(elements, call = call)
    check_string(glue_index_string, null.ok=TRUE, call = call)



    ### Paths
    check_string(path, null.ok=TRUE, call = call)

    # if(!fs::is_dir(path)) cli::cli_abort(c(
    #   x="{.arg path} provided ({.path {path}}) is not a valid path.",
    #   i="Consider changing working directory or provide absolute valid path to existing directory."),
    #   call = call)

    if(length(grouping_structure)>0) {
      chapter_folderpath <-
        data_overview[[grouping_structure[1]]] %>%
        stringr::str_unique() %>%
        fix_path_spaces()
    } else chapter_folderpath <- ""

    fs::dir_create(path = fs::path(path, chapter_folderpath), recurse = TRUE)

    chapter_filepath_relative <-
      fs::path(chapter_folderpath,
               stringr::str_c(chapter_folderpath, ".qmd"))

    chapter_filepath_absolute <-
      fs::path(path, chapter_filepath_relative)


    ### YAML
    chapter_yml <-
      if(!ymlthis::is_yml(chapter_yml)) {
        if(!is.null(chapter_yml)) {
          cli::cli_warn(c("{.arg chapter_yml} must be a yml-object, not {.obj_type_friendly {chapter_yml}}.",
                          i="See {.fun ymlthis::yml}.",
                          i="Ignoring {.arg chapter_yml} argument."), call = call)
        }
        chapter_yml <-
          ymlthis::yml_empty()
      }
    chapter_yml <-
      chapter_yml %>%
      ymlthis::yml_author(name = unique(data_overview$author)) %>%
      ymlthis::asis_yaml_output() %>%
      as.character() %>%
      stringr::str_replace_all(pattern = "```|yaml|\\[\\]", replacement = "\n") %>%
      stringr::str_replace_all(pattern = "\\'FALSE\\'", replacement = "false") %>%
      stringr::str_replace_all(pattern = "\\'TRUE\\'", replacement = "true")



    # Hvis grouping_structure== c("chapter", "designated_role", "col_group"), så ignorer nivå 1, grupper på resten
    # Hvis grouping_structure== c("col_group") så ignorer nivå 1, ignorer gruppering.
    # Hvis grouping_structure==character(0) så ignorer gruppering.


    gen_qmd_structure(data_overview = data_overview,
                      grouping_structure = grouping_structure,
                      elements = elements,
                      glue_index_string = glue_index_string,
                      ignore_if_below = ignore_if_below,
                      path = path) %>%

    # data_overview %>%
    #   {if(length(grouping_structure)>0) dplyr::group_by(., dplyr::pick(tidyselect::all_of(grouping_structure[-1]))) else .} %>%
    #   dplyr::group_map(
    #     .keep = TRUE,
    #     .f = function(data_overview_section,
    #                   key_section) {
    #
    #
    #       gen_qmd_section(
    #         data_overview_section = data_overview_section,
    #         elements = elements,
    #         glue_index_string = glue_index_string,
    #
    #         grouping_structure = grouping_structure[-1],
    #
    #         ignore_if_below = ignore_if_below,
    #         path = path,
    #         call = call)
    #
    #     }) %>%
    #   stringr::str_c(collapse = "\n") %>%
      stringr::str_c(chapter_yml, ., sep="\n") %>%
      cat(file = chapter_filepath_absolute)

    chapter_filepath_relative
  }
