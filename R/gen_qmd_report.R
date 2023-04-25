


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
#' @param glue_index_string String used to glue together columns in the data_overview for creating an identifier to look up objects in elements-lists and character vectors. Defaults to "{designated_role}_{col_group}_{name_prefix}".
#' @param ignore_if_below A double between 0 and 1 to control which elements are hidden based on their significance level. Defaults to 0 (show all).
#' @param captions A string indicating whether the caption for each plot/table is displayed. One of
#' \describe{
#' \item{\code{"asis"}}{A technical ID, which is useful for troubleshooting or quick search and replace at the end.}
#' \item{\code{"pretty"}}{A full text title in English. Currently no translations have been implemented.}
#' \item{\code{"none"}}{None.}
#' }
#' @param index_filename String specifying the name of the main index Quarto file (and its subfolder) used to collect all the chapters. Defaults to "index.qmd".
#' @param report_ymlthis_config,chapter_ymlthis_config Optional ymlthis-objects. Defaults will be obtained from getOptions("ymlthis.default_options") if NULL. The chapter_ymlthis_config will be used across all chapters. Authors will always be taken from the data_overview.
#' @param path String specifying the path to the working directory. Defaults to the current working directory.
#'
#' @return A Quarto survey report generated in the specified working directory.
#' @export
#'
#' @examples
#' # Define temporary folder for storing the elements
#' library(dplyr)
#' temp_folder <- tempdir()
#' data_overview <-
#'  ex_survey_ch_overview %>%
#'   saros::refine_data_overview(data = ex_survey1,
#'                               label_separator = " - ",
#'                               name_separator = "_")
#'
#' if(!is.null(quarto::quarto_path())) {
#'   index_filepath <-
#'    gen_qmd_report(
#'       data_overview = data_overview,
#'       elements = ex_survey_elements_list,
#'       captions = "pretty",
#'       path = temp_folder)
#'
#'   if(interactive()) {
#'     browseURL(fs::path(temp_folder, "index.qmd"))
#'     quarto::quarto_render(fs::path(temp_folder, "index.qmd"))
#'     browseURL(fs::path(temp_folder, "index.html"))
#'   }
#' }
#' unlink(temp_folder)
gen_qmd_report <-
  function(data_overview,
           elements = NULL,
           glue_index_string = NULL,
           ignore_if_below = 0,
           captions = c("asis", "pretty", "none"),
           report_ymlthis_config = NULL,
           chapter_ymlthis_config = NULL,
           index_filename = "index.qmd",
           path = getwd()
  ) {

    check_data_frame(data_overview, call = call)
    check_elements(elements, call = call)
    check_string(glue_index_string, null.ok = TRUE, call = call)
    check_pval(ignore_if_below, call = call)
    captions <- rlang::arg_match(captions, multiple = FALSE, error_call = call)
    check_yml(report_ymlthis_config, call = call)
    check_yml(chapter_ymlthis_config, call = call)
    check_string(index_filename, null.ok = FALSE, call = call)
    check_string(path, null.ok = TRUE, call = call)
    path <- fs::as_fs_path(path)
    fs::dir_create(path = path, recurse = TRUE)


    if(is.null(report_ymlthis_config)) {
      report_yml <-
        ymlthis::yml_empty()
    } else report_yml <- report_ymlthis_config

    if(is.null(chapter_ymlthis_config)) {
       chapter_yml <-
         ymlthis::yml_empty()
    } else chapter_yml <- chapter_ymlthis_config

    grouping_structure <-
      data_overview %>%
      dplyr::group_vars() %>%
      rlang::set_names(nm = seq_along(.))

    if(length(grouping_structure) < 1) {
      cli::cli_abort(c(
        "!"="No grouping variables found in {.arg data_overview}.",
        "x"="Without grouping variables, contents in {.arg elements} cannot be located.",
        "i"="Consider using {.fun refine_data_overview}."))
    }


    grouping_structure <-
      data_overview %>%
      dplyr::group_vars()

    data_overview_chapter_groups <-
    data_overview %>%
      dplyr::group_by(.data[[grouping_structure[1]]])
    cli::cli_progress_bar(name = "Creating chapter files...", type = "iterator",
                          format_done = "Chapter files completed!",
                          total = dplyr::n_distinct(data_overview_chapter_groups[[grouping_structure[1]]]))


    ## Generate each chapter. Returns paths to files, which are then used for index.qmd
    chapter_filepaths <-
      data_overview_chapter_groups %>%
      dplyr::group_map(
        .keep = TRUE,
        .f = function(data_overview_chapter,
                      key_chapter) {



          data_overview_chapter <-
            data_overview_chapter %>%
            dplyr::group_by(dplyr::pick(tidyselect::all_of(grouping_structure)))

          # data_overview_chapter_by <-
          #   data_overview_chapter %>%
          #   dplyr::filter(.data$designated_role == "indep") %>%
          #   dplyr::ungroup() %>%
          #   tidyr::nest(.key = ".by_col_information")

          # Paths
            chapter_folderpath <-
              data_overview_chapter[[grouping_structure[1]]] %>%
              stringr::str_unique() %>%
              fix_path_spaces()


          fs::dir_create(path = fs::path(path, chapter_folderpath), recurse = TRUE)

          chapter_filepath_relative <-
            fs::path(stringr::str_c(chapter_folderpath, ".qmd"))

          chapter_filepath_absolute <-
            fs::path(path, chapter_filepath_relative)


          ### YAML

          chapter_yml <-
            chapter_yml %>%
            ymlthis::yml_author(name = unique(data_overview_chapter$author)) %>%
            ymlthis::yml_toplevel(format = "html",
                                  echo = FALSE,
                                  editor = "visual") %>%
            ymlthis::asis_yaml_output() %>%
            as.character() %>%
            stringr::str_replace_all(pattern = "```|yaml|\\[\\]", replacement = "\n") %>%
            stringr::str_replace_all(pattern = "\\'FALSE\\'", replacement = "false") %>%
            stringr::str_replace_all(pattern = "\\'TRUE\\'", replacement = "true") %>%
            stringr::str_replace_all(pattern = "^\n\n\n", replacement = "")



          gen_qmd_structure(data_overview = data_overview_chapter,
                            elements = elements,
                            glue_index_string = glue_index_string,
                            ignore_if_below = ignore_if_below,
                            captions = captions,
                            path = path) %>%
          stringr::str_c(chapter_yml,
                         "```{r}",
                         "###  experimental docx-production",
                         "library(officer)",
                         "docx <- officer::read_docx()",
                         "```",
                         .,
                         "```{r}",
                         "###  experimental docx-production",
                         paste0("print(docx, target='", chapter_folderpath, ".docx')"),
                         "```",
                         sep = "\n") %>%
            cat(file = chapter_filepath_absolute)

          chapter_filepath_relative

        })

    cli::cli_process_done()

    gen_qmd_index(ymlthis_config = report_ymlthis_config,
                  authors = unique(data_overview[["author"]]),
                  index_filepath = fs::path(path, index_filename),
                  chapter_filepaths = chapter_filepaths,
                  call = rlang::caller_env()
    )

  }
