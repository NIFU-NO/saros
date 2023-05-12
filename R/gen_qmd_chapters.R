#' Generate A Quarto Survey Report
#'
#' This function generates a Quarto survey report.
#'
#' @details
#' A report consists of multiple chapters and an index file that merges them together.
#' A chapter can contain any user-defined set of dependent, independent or bivariate variable sets.
#' A chapter consists of multiple sections.
#' A section is defined as a group in the data_overview (ignoring the chapter grouping level) containing variables of the same type, meaning at a minimum that the variables in the section sharing the same response options, the same main question, and being of the same data type.
#'
#' @param data_overview A (possibly grouped) tbl with overview of the survey columns in data.
#' @param elements A named list of elements used to populate each chapter's section. The names of the list must be one of \code{\link{list_available_element_types}}. Defaults to a list of TRUE values for various elements. Each named element of the list can contain
#' \describe{
#' \item{\code{NULL}}{(default)}{ no element is inserted.}
#' \item{\code{list()}}{A named sublist of already produced objects of the same type. The names must be of }
#' \item{\code{character()}}{A character vector of paths to Rds-files containing the objects. These will be read in as needed, saving memory.}
#' }
#' @param index_filename String specifying the name of the main index Quarto file (and its subfolder) used to collect all the chapters. Defaults to "index.qmd".
#' @param report_yaml_file,chapter_yaml_file Optional ymlthis-objects. Defaults will be obtained from getOptions("ymlthis.default_options") if NULL. The chapter_yaml_file will be used across all chapters. Authors will always be taken from the data_overview.
#' @param path String specifying the path to the working directory. Defaults to the current working directory.
#'
#' @return A Quarto survey report generated in the specified working directory.
gen_qmd_chapters <-
  function(data_overview,
           data,
           element_names = NULL,
           label_separator = NULL,
           report_yaml_file = NULL,
           chapter_yaml_file = NULL,
           translations = .saros.env$defaults$translations,
           path = getwd(),
           ...,
           call = rlang::caller_env()
  ) {
    dots <- rlang::list2(...)

    cli::cli_progress_message(msg = "Creating report files...")
    path <- fs::as_fs_path(path)
    fs::dir_create(path = path, recurse = TRUE)


    if(is.null(chapter_yaml_file)) {
       chapter_yml <-
         ymlthis::yml_empty()
    } else chapter_yml <- chapter_yaml_file

    grouping_structure <- dplyr::group_vars(data_overview)

    if(length(grouping_structure) == 0) {
      cli::cli_abort(c(
        "!"="No grouping variables found in {.arg data_overview}.",
        "x"="Without grouping variables, contents in {.arg elements} cannot be located.",
        "i"="Consider using {.fun refine_data_overview}."))
    }

    data_overview_chapter_groups <-
      data_overview %>%
      dplyr::group_by(.data[[grouping_structure[1]]])

    cli::cli_progress_bar(name = "Creating chapter files...", type = "iterator",
                          format_done = "Chapter files completed!", clear = FALSE,
                          total = dplyr::n_distinct(data_overview_chapter_groups[[grouping_structure[1]]]))


    ## Generate each chapter. Returns paths to files, which are then used for index.qmd
    chapter_filepaths <-
      data_overview_chapter_groups %>%
      dplyr::group_map(
        .keep = TRUE,
        .f = function(data_overview_chapter,
                      key_chapter) {





          data_overview_chapter <-
            dplyr::group_by(data_overview_chapter, dplyr::pick(tidyselect::all_of(grouping_structure)))

          # Paths
            chapter_foldername <-
              data_overview_chapter[[grouping_structure[1]]] %>%
              unique() %>%
              fix_path_spaces()

            cli::cli_progress_message(msg = "Generating chapter {chapter_foldername}")


            chapter_folderpath_absolute <- fs::path(path, chapter_foldername)

          fs::dir_create(path = chapter_folderpath_absolute, recurse = TRUE)

          chapter_filepath_relative <-
            stringr::str_c(chapter_foldername, ".qmd")

          chapter_filepath_absolute <-
            fs::path(path, chapter_filepath_relative)


          ### YAML

          chapter_yml <-
            chapter_yml %>%
            ymlthis::yml_author(name = unique(data_overview_chapter[["author"]])) %>%
            ymlthis::yml_toplevel(format = "html",
                                  execute = list(list(cache = TRUE, echo = FALSE))) %>%
            ymlthis::asis_yaml_output() %>%
            as.character() %>%
            stringi::stri_replace_all(regex = "```|yaml|\\[\\]", replacement = "\n") %>%
            stringi::stri_replace_all(regex = "\\'FALSE\\'", replacement = "false") %>%
            stringi::stri_replace_all(regex = "\\'TRUE\\'", replacement = "true") %>%
            stringi::stri_replace_all(regex = "^\n\n\n", replacement = "")

          chapter_contents <-
          rlang::exec(
            gen_qmd_structure,
            data = data,
            data_overview = data_overview_chapter,
            element_names = element_names,
            label_separator = label_separator,
            chapter_folderpath_absolute = chapter_folderpath_absolute,
            chapter_foldername = chapter_foldername,
            translations = translations,
            summarized_data = NULL,
            !!!dots,
            call = call)


            stringr::str_c(chapter_yml,
                           chapter_contents,
                           sep = "\n") %>%
            cat(file = chapter_filepath_absolute)

          chapter_filepath_relative

        })

    cli::cli_process_done()
    chapter_filepaths


  }
