

#' Generate Quarto Index File That Merges All Chapters
#'
#' This function creates an index Quarto file (QMD) that merges all chapters in the specified order. It can also include optional title and author(s) information.
#'
#' @param yaml_file A ymlthis-object. Defaults will be obtained from getOptions("ymlthis.default_options") if NULL.
#' @param authors A character vector listing the authors of the Quarto document. If not provided, the author information will be omitted.
#' @param index_filepath A string specifying the name of the Quarto index file to be created. Defaults to "complete_report.qmd".
#' @param chapter_filepaths A character vector containing the filepaths of the chapter files to be included in the index, in the desired order.
#' @param call Internal call argument. Not to be fiddled with by the user.
#'
#' @return A string containing the filepath to the generated Quarto index file.
#' @examples
#' filepath <- tempfile(pattern = "index", fileext = ".qmd")
#' index_file <-
#'   saros:::gen_qmd_index(
#'                  authors = c("Marky Twain",
#'                              "J.K. Rawlings",
#'                              "Stephen Prince"),
#'                chapter_filepaths =
#'                  c("Ch1.qmd",
#'                  "Ch2.qmd",
#'                  "Ch3.qmd"),
#'                  index_filepath = filepath)
gen_qmd_index <-
  function(
    yaml_file = NULL,
    title = NULL,
    authors = NULL,
    index_filepath = "complete_report.qmd",
    ...,
    chapter_filepaths = NULL,
    call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    check_string(authors, n = NULL, null.ok = TRUE, call = call)
    check_string(index_filepath, null.ok = FALSE, n = 1, call = call)

    yml_section <-
      process_yaml(yaml_file = yaml_file,
                   title = title,
                   authors = authors)


    main_section <-
      purrr::map_chr(chapter_filepaths,
                     .f = ~stringr::str_c('\n{{< include "', .x, '" >}}'))
    main_section <-  stringr::str_c(main_section, collapse = "\n")

    qmd_start_section <- if(!rlang::is_null(dots$qmd_start_section_filepath)) stringr::str_c(readLines(con = dots$qmd_start_section_filepath), collapse="\n")
    qmd_end_section <- if(!rlang::is_null(dots$qmd_end_section_filepath)) stringr::str_c(readLines(con = dots$qmd_end_section_filepath), collapse="\n")

    out <-
      stringr::str_c(yml_section,
                   qmd_start_section,
                   main_section,
                   qmd_end_section)
    cat(out, file = index_filepath, append = FALSE)
    index_filepath
  }

